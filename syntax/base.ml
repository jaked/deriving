(* Copyright Jeremy Yallop 2007.
   This file is free software, distributed under the MIT license.
   See the file COPYING for details.
*)

open Utils
open Type
open Defs
open Camlp4.PreCast

exception Underivable of string
exception NoSuchClass of string

(* display a fatal error and exit *)
let error loc (msg : string) =
  Syntax.print_warning loc msg;
  exit 1

let contains_tvars, contains_tvars_decl =
  let o = object
     inherit [bool] fold as default
     method crush = List.exists F.id
     method expr = function
       | `Param _ -> true
       | e -> default#expr e
  end in (o#expr, o#decl)

module InContext(L : Loc)(D : ClassDescription) : ClassHelpers =
struct
  include L
  module Untranslate = Untranslate(L)

  open D

  let instantiate, instantiate_repr =
    let o lookup = object 
      inherit transform as super
      method expr = function
        | `Param (name, _) -> lookup name
        | e                -> super # expr e
    end in
      (fun (lookup : name -> expr) -> (o lookup)#expr),
      (fun (lookup : name -> expr) -> (o lookup)#repr)

  let instantiate_modargs, instantiate_modargs_repr =
    let lookup argmap var = 
      try 
        `Constr ([NameMap.find var argmap; "a"], [])
      with Not_found ->
        failwith ("Unbound type parameter '" ^ var)
    in (fun argmap -> instantiate (lookup argmap)),
       (fun argmap -> instantiate_repr (lookup argmap))

  let substitute env =
    (object
       inherit transform as default
       method expr = function
         | `Param (p,v) when NameMap.mem p env -> 
             `Param (NameMap.find p env,v)
         | e -> default# expr e
     end) # expr

  let cast_pattern argmap ?(param="x") t = 
    let t = Untranslate.expr (instantiate_modargs argmap t) in
      (<:patt< $lid:param$ >>,
       <:expr<
         let module M = 
             struct
               type $Ast.TyDcl (_loc, "t", [], t, [])$
               let test = function #t -> true | _ -> false
             end in M.test $lid:param$ >>,
       <:expr<
         (let module M = 
              struct
                type $Ast.TyDcl (_loc, "t", [], t, [])$
                let cast = function #t as t -> t | _ -> assert false
              end in M.cast $lid:param$ )>>)

  let seq l r = <:expr< $l$ ; $r$ >>
  let rec seq_list = function
    | [] -> <:expr< () >>
    | [e] -> e
    | e::es -> seq e (seq_list es)

  let record_pattern ?(prefix="") (fields : Type.field list) : Ast.patt = 
    <:patt<{$list:
              (List.map (fun (label,_,_) -> <:patt< $lid:label$ = $lid:prefix ^ label$ >>) 
                      fields) $}>>

  let record_expr : (string * Ast.expr) list -> Ast.expr = 
    fun fields ->
      let fs = 
      List.fold_left1 
        (fun l r -> <:rec_binding< $l$ ; $r$ >>)
        (List.map (fun (label, exp) -> <:rec_binding< $lid:label$ = $exp$ >>) 
           fields) in
        Ast.ExRec (_loc, fs, Ast.ExNil _loc)

  let record_expression ?(prefix="") : Type.field list -> Ast.expr = 
    fun fields ->
      let es = List.fold_left1
        (fun l r -> <:rec_binding< $l$ ; $r$ >>)
        (List.map (fun (label,_,_) -> <:rec_binding< $lid:label$ = $lid:prefix ^ label$ >>) 
           fields) in
        Ast.ExRec (_loc, es, Ast.ExNil _loc)

  let mproject mexpr name = 
    match mexpr with
      | <:module_expr< $id:m$ >> -> <:expr< $id:m$.$lid:name$ >>
      | _ -> <:expr< let module M = $mexpr$ in M.$lid:name$ >>

  let mProject mexpr name =
    match mexpr with
      | <:module_expr< $uid:m$ >> -> <:module_expr< $uid:m$.$uid:name$ >>
      | _ -> <:module_expr< struct module M = $mexpr$ include M.$uid:name$ end >>

  let expr_list : Ast.expr list -> Ast.expr = 
      (fun exprs ->
         List.fold_right 
           (fun car cdr -> <:expr< $car$ :: $cdr$ >>)
           exprs
         <:expr< [] >>)

  let patt_list : Ast.patt list -> Ast.patt = 
      (fun patts ->
         List.fold_right 
           (fun car cdr -> <:patt< $car$ :: $cdr$ >>)
           patts
         <:patt< [] >>)

  let tuple_expr : Ast.expr list -> Ast.expr = function
    | [] -> <:expr< () >>
    | [x] -> x
    | x::xs -> Ast.ExTup (_loc, List.fold_left (fun e t -> Ast.ExCom (_loc, e,t)) x xs)

  let tuple ?(param="v") n : string list * Ast.patt * Ast.expr =
    let v n = Printf.sprintf "%s%d" param n in
      match n with
        | 0 -> [], <:patt< () >>, <:expr< () >>
        | 1 -> [v 0], <:patt< $lid:v 0$ >>, <:expr< $lid:v 0$ >>
        | n -> 
            let patts, exprs = 
              (* At time of writing I haven't managed to write anything
                 using quotations that generates an n-tuple *)
              List.fold_left 
                (fun (p, e) (patt, expr) -> Ast.PaCom (_loc, p, patt), Ast.ExCom (_loc, e, expr))
                (<:patt< >>, <:expr< >>)
                (List.map (fun n -> <:patt< $lid:v n$ >>, <:expr< $lid:v n $ >>)
                   (List.range 0 n))
            in
              List.map v (List.range 0 n), Ast.PaTup (_loc, patts), Ast.ExTup (_loc, exprs)

  let rec modname_from_qname ~qname ~classname =
    match qname with 
      | [] -> invalid_arg "modname_from_qname"
      | [t] -> <:ident< $uid:classname ^ "_"^ t$ >>
      | t::ts -> <:ident< $uid:t$.$modname_from_qname ~qname:ts ~classname$ >>

  let apply_functor (f : Ast.module_expr) (args : Ast.module_expr list) : Ast.module_expr =
      List.fold_left (fun f p -> <:module_expr< $f$ ($p$) >>) f args

  let atype_expr ctxt expr =
    Untranslate.expr (instantiate_modargs ctxt.argmap expr)

  let atype ctxt (name, params, rhs, _, _) =
    match rhs with
    | `Fresh _ | `Variant _ | `Nothing ->
	let params =
	  List.map
	    (fun (p, _) -> `Constr ([NameMap.find p ctxt.argmap; "a"],[]))
	    params in
	Untranslate.expr (`Constr ([name], params))
    | `Expr e -> atype_expr ctxt e

  let wrap_default m = match default_module with
  | None -> m
  | Some name ->
      <:module_expr< $uid:runtimename$.$uid:name$($m$) >>

  let import_depend ctxt ty depends =
    let d = depends _loc in
    assert (ctxt.toplevel = None);
    let ctxt = { ctxt with toplevel = Some (classname, ty) } in
    <:str_item< module $uid:d.d_classname$ = $d.d_generate_expr ctxt ty$ >>

  let import_depends ctxt ty =
    List.map (import_depend ctxt ty) depends

  class virtual make_module_expr : generator =
  object (self)

    method wrap ctxt ty items =
      let mexpr =
	<:module_expr< struct
	  type a = ($atype_expr ctxt ty$)
	  $list:import_depends ctxt ty$
	  $list:items$
	end >> in
      wrap_default mexpr

    method mapply ctxt (funct : Ast.module_expr) args =
      apply_functor funct (List.map (self#expr ctxt) args)

    method virtual variant:
	context ->
	  Type.name -> Type.param list -> Type.constraint_ list ->
	    variant -> Ast.str_item list
    method virtual sum:
	?eq:expr -> context ->
	  Type.name -> Type.param list -> Type.constraint_ list ->
	    summand list -> Ast.str_item list
    method virtual record:
	?eq:expr -> context ->
	  Type.name -> Type.param list -> Type.constraint_ list ->
	    field list -> Ast.str_item list
    method virtual tuple: context -> expr list -> Ast.str_item list

    method param ctxt (name, _) =
      match ctxt.toplevel with
      | None -> <:module_expr< $uid:NameMap.find name ctxt.argmap$ >>
      | Some _ ->
	  let mexpr = <:module_expr< $uid:NameMap.find name ctxt.argmap$ >> in
	  mProject mexpr classname


    method object_   _ o = raise (Underivable (classname ^ " cannot be derived for object types"))
    method class_    _ c = raise (Underivable (classname ^ " cannot be derived for class types"))
    method label     _ l = raise (Underivable (classname ^ " cannot be derived for label types"))
    method function_ _ f = raise (Underivable (classname ^ " cannot be derived for function types"))

    method constr ctxt (qname, params) =
      let ty : Type.expr = `Constr (qname, params) in
      match qname with
      | [name] when NameSet.mem name ctxt.tnames -> begin
	  match ctxt.toplevel with
	  | None -> <:module_expr< $uid:Printf.sprintf "%s_%s" classname name$ >>
	  | Some (topclassname, (`Constr ([name], params) as ty')) when ty' = ty ->
	      let e = apply_functor
		  <:module_expr< $uid:Printf.sprintf "%s_%s" classname name$ >>
	        (List.map (fun p -> self#expr ctxt p) params) in
	      <:expr< $e$ >>
	  | Some (topclassname, _) ->
	      let mexpr =
		<:module_expr< $uid:Printf.sprintf "%s_%s" topclassname name$ >> in
	      mProject mexpr classname
      end
      | _ ->
	  let qname =
	    try [runtimename ; List.assoc qname predefs]
	    with Not_found -> qname in
          let f = (modname_from_qname ~qname ~classname) in
          self#mapply ctxt (Ast.MeId (_loc, f)) params

    method expr ctxt ty = match ty with
      | `Param p    ->                   (self#param      ctxt p)
      | `Object o   -> self#wrap ctxt ty (self#object_    ctxt o)
      | `Class c    -> self#wrap ctxt ty (self#class_     ctxt c)
      | `Label l    -> self#wrap ctxt ty (self#label      ctxt l)
      | `Function f -> self#wrap ctxt ty (self#function_  ctxt f)
      | `Constr c   ->                   (self#constr     ctxt c)
      | `Tuple t    -> self#wrap ctxt ty (self#tuple      ctxt t)

    method rhs ctxt (tname, params, rhs, constraints, _) : Ast.module_expr =
      let ty = `Constr([tname], List.map (fun p -> `Param p) params) in
      match rhs with
        | `Fresh (_, _, (`Private : [`Private|`Public])) when not allow_private ->
            raise (Underivable ("The class "^ classname ^" cannot be derived for private types"))
        | `Fresh (eq, Sum summands, _) ->
	    self#wrap ctxt ty (self#sum ?eq ctxt tname params constraints summands)
        | `Fresh (eq, Record fields, _) ->
	    self#wrap ctxt ty (self#record ?eq ctxt tname params constraints fields)
        | `Expr e -> self#expr ctxt e
        | `Variant v -> self#wrap ctxt ty (self#variant ctxt tname params constraints v)
        | `Nothing -> <:module_expr< >>

    method call_expr ctxt ty name = mproject (self#expr ctxt ty) name

  end

  let make_safe (decls : (decl * Ast.module_binding) list) : Ast.module_binding list =
    (* re-order a set of mutually recursive modules in an attempt to
       make initialization problems less likely *) 
    List.map snd
      (List.sort 
         (fun ((_,_,lrhs,_,_), _) ((_,_,rrhs,_,_), _) -> match (lrhs : rhs), rrhs with
            (* aliases to types in the group score higher than
               everything else.

               In general, things that must come first receive a
               positive score when they occur on the left and a
               negative score when they occur on the right. *)
            | (`Fresh _|`Variant _), (`Fresh _|`Variant _) -> 0
            | (`Fresh _|`Variant _), _ -> -1
            | _, (`Fresh _|`Variant _) -> 1
            | (`Nothing, `Nothing) -> 0
            | (`Nothing, _) -> 1
            | (_, `Nothing) -> -1
            | `Expr l, `Expr r -> 
                let module M = 
                    struct
                      type low = 
                          [`Param of param
                          |`Tuple of expr list]
                    end in
                  match l, r with
                    | #M.low, _ -> 1
                    | _, #M.low -> -1
                    | _         -> 0)
         decls)

  let find_non_regular params tnames decls : name list =
    List.concat_map
      (object 
	inherit [name list] fold as default
	method crush = List.concat
	method expr = function
          | `Constr ([t], args) 
            when NameSet.mem t tnames ->
              (List.concat_map2
                 (fun (p,_) a -> match a with
                 | `Param (q,_) when p = q -> []
                 | _ -> [t])
                 params
                 args)
          | e -> default#expr e
      end)#decl decls

  let extract_params = 
    let has_params params (_, ps, _, _, _) = ps = params in
    function
      | [] -> invalid_arg "extract_params"
      | (_,params,_,_,_)::rest
        when List.for_all (has_params params) rest ->
          params
      | (_,_,rhs,_,_)::_ -> 
          (* all types in a clique must have the same parameters *)
          raise (Underivable ("Instances can only be derived for "
                              ^"recursive groups where all types\n"
                              ^"in the group have the same parameters."))

  let setup_context (tdecls : decl list) : context =
    let params = extract_params tdecls 
    and tnames = NameSet.fromList (List.map (fun (name,_,_,_,_) -> name) tdecls) in
    match find_non_regular params tnames tdecls with
    | _::_ as names -> 
        failwith ("The following types contain non-regular recursion:\n   "
                  ^String.concat ", " names
                  ^"\nderiving does not support non-regular types")
    | [] ->
        let argmap = 
          List.fold_right
            (fun (p,_) m -> NameMap.add p (Printf.sprintf "V_%s" p) m)
            params
            NameMap.empty in 
        { argmap = argmap;
          params = params; 
          tnames = tnames;
	  toplevel = None;
	}

  let default_generate ~make_module_expr ~make_module_type decls =
    (* plan: 
       set up an enclosing recursive module
       generate functors for all types in the clique
       project out the inner modules afterwards.
       
       later: generate simpler code for simpler cases:
       - where there are no type parameters
       - where there's only one type
       - where there's no recursion
       - etc.
    *)
    let context = setup_context decls in
    let wrapper_name = Printf.sprintf "%s_%s" classname (random_id 32)  in
    let make_functor = 
      List.fold_right 
        (fun (p,_) rhs -> 
           let arg = NameMap.find p context.argmap in
             <:module_expr< functor ($arg$ : $uid:runtimename$.$uid:classname$) -> $rhs$ >>)
        context.params in
    let mbinds =
      List.map 
        (fun (name,_,_,_,_ as decl) -> 
             (decl,
              <:module_binding< 
                $uid:classname ^ "_"^ name$
                : $make_module_type context decl$
               = $make_module_expr context decl$ >>))
        decls in
    let sorted_mbinds = make_safe mbinds in
    let mrec =
      <:str_item< module rec $list:sorted_mbinds$ >> in
      match context.params with
        | [] -> mrec
        | _ ->
           let fixed = make_functor <:module_expr< struct $mrec$ end >> in
           let applied = apply_functor <:module_expr< $uid:wrapper_name$ >> 
                                       (List.map (fun (p,_) -> <:module_expr< $uid:NameMap.find p context.argmap$>>) 
                                             context.params) in
           let projected =
             List.map (fun (name,params,rhs,_,_) -> 
                         let modname = classname ^ "_"^ name in
                         let rhs = <:module_expr< struct module P = $applied$ include P.$uid:modname$ end >> in
                           <:str_item< module $uid:modname$ = $make_functor rhs$>>)
               decls in
           let m = <:str_item< module $uid:wrapper_name$ = $fixed$ >> in
             <:str_item< $m$ $list:projected$ >>

   let make_module_sig context (tname,params,_,_,generated as decl) =
      if tname = "a" then
        raise (Underivable ("deriving: types called `a' are not allowed.\n"
                            ^"Please change the name of your type and try again."))
      else
        List.fold_right
          (fun (p,_) m ->
	    <:module_type< functor ($NameMap.find p context.argmap$ : $uid:runtimename$.$uid:classname$) -> $m$ >>)
          params
          <:module_type< $uid:runtimename$.$uid:classname$ with type a = $atype context decl$ >>

   let make_module_type context (name, _, _, _, _ as decl) =
     if name = "a" then
       raise (Underivable ("deriving: types called `a' are not allowed.\n"
                           ^"Please change the name of your type and try again."))
     else
       <:module_type< $uid:runtimename$.$uid:classname$ with type a = $atype context decl$ >>

    let default_generate_sigs ~make_module_sig decls =
      let context = setup_context decls in
      let make (tname, _, _ ,_, generated as decl) =
        if generated
	then <:sig_item< >>
	else
	  let t = make_module_sig context decl in
          <:sig_item< module $uid:Printf.sprintf "%s_%s" classname tname$ : $t$ >> in
      <:sig_item< $list:List.map make decls$ >>

end


type deriver = Loc.t * Type.decl list -> Ast.str_item
and sigderiver = Loc.t * Type.decl list -> Ast.sig_item
let derivers : (name, (deriver * sigderiver)) Hashtbl.t = Hashtbl.create 15
let hashtbl_add = Hashtbl.add derivers
let register_hook = ref [hashtbl_add]
let add_register_hook f = register_hook := f :: !register_hook
let register name derivers =
  List.iter (fun f -> f name derivers) !register_hook
let find classname =
  try Hashtbl.find derivers classname
  with Not_found -> raise (NoSuchClass classname)
let is_registered : name -> bool =
  fun classname -> try ignore (find classname); true with NoSuchClass _ -> false

module Register
    (Desc : ClassDescription)
    (MakeClass : functor(L : Loc) -> Class) = struct

  let generate (loc, decls) =
    let module Class = MakeClass(struct let _loc = loc end) in
    Class.generate decls

  let generate_sigs (loc, decls) =
    let module Class = MakeClass(struct let _loc = loc end) in
    Class.generate_sigs decls

  let depends loc =
    let module Class = MakeClass(struct let _loc = loc end) in
    { d_classname = Desc.classname;
      d_generate_expr = Class.generate_expr }

  let _ = register Desc.classname (generate, generate_sigs)

end
