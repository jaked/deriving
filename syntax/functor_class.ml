(* Copyright Jeremy Yallop 2007.
   This file is free software, distributed under the MIT license.
   See the file COPYING for details.
*)

open Pa_deriving_common.Defs
open Camlp4.PreCast

module Description : ClassDescription = struct
  let classname = "Functor"
  let runtimename = "Deriving_Functor"
  let default_module = None
  let allow_private = false
  let predefs = [
    ["list"], "list";
    ["ref"], "ref";
    ["option"], "option";
  ]
  let depends = []
end

module InContext (C : sig val _loc : Camlp4.PreCast.Loc.t end) =
struct
  open C
  open Pa_deriving_common.Type
  open Pa_deriving_common.Utils
  open Pa_deriving_common.Base

  module Helpers = Pa_deriving_common.Base.InContext(C)(Description)
  open Helpers
  open Description

  let param_map context : string NameMap.t =
    List.fold_right
      (fun (name,_) map -> NameMap.add name ("f_" ^ name) map)
      context.params
      NameMap.empty

  let tdec, sigdec =
    let dec context name =
      ("f", context.params,
       `Expr (`Constr ([name], List.map (fun p -> `Param p) context.params)), [], false)
    in
      (fun context name -> Untranslate.decl (dec context name)),
      (fun context name -> Untranslate.sigdecl (dec context name))

  let wrapper context name expr =
    let param_map = param_map context in
    let patts :Ast.patt list =
      List.map
        (fun (name,_) -> <:patt< $lid:NameMap.find name param_map$ >>)
        context.params in
    let rhs =
      List.fold_right (fun p e -> <:expr< fun $p$ -> $e$ >>) patts expr in
      <:module_expr< struct
        type $tdec context name$
        let map = $rhs$
      end >>
(*
   prototype: [[t]] : t -> t[b_i/a_i]


   [[a_i]]   = f_i

   [[C1|...CN]] = function [[C1]] ... [[CN]]               sum
   [[`C1|...`CN]] = function [[`C1]] ... [[`CN]]           variant

   [[{t1,...tn}]] = fun (t1,tn) -> ([[t1]],[[tn]])         tuple
   [[{l1:t1; ... ln:tn}]] =
         fun {l1=t1;...ln=tn} -> {l1=[[t1]];...ln=[[tn]]}  record

   [[(t1,...tn) c]] = c_map [[t1]]...[[tn]]                constructor

   [[a -> b]] = f . [[a]] (where a_i \notin fv(b))         function

   [[C0]]    = C0->C0                                      nullary constructors
   [[C1 (t1...tn)]]  = C1 t -> C0 ([[t1]] t1...[[tn]] tn)  unary constructor
   [[`C0]]   = `C0->`C0                                    nullary tag
   [[`C1 t]] = `C1 t->`C0 [[t]] t                          unary tag
*)
  let rec polycase context = function
    | Tag (name, []) -> <:match_case< `$name$ -> `$name$ >>
    | Tag (name, es) -> <:match_case< `$name$ x -> `$name$ ($expr context (`Tuple es)$ x) >>
    | Extends t ->
        let patt, guard, exp = cast_pattern context.argmap t in
          <:match_case< $patt$ when $guard$ -> $expr context t$ $exp$ >>

  and expr context : Pa_deriving_common.Type.expr -> Ast.expr = function
    | t when not (contains_tvars t) -> <:expr< fun x -> x >>
    | `Param (p,_) -> <:expr< $lid:NameMap.find p (param_map context)$ >>
    | `Function (f,t) when not (contains_tvars t) ->
        <:expr< fun f x -> f ($expr context f$ x) >>
    | `Constr (qname, ts) ->
	let qname =
	  try [runtimename ; List.assoc qname predefs]
	  with Not_found -> qname in
        List.fold_left
          (fun fn arg -> <:expr< $fn$ $expr context arg$ >>)
          <:expr< $id:modname_from_qname ~qname ~classname$.map >>
          ts
    | `Tuple ts -> tup context ts
    | _ -> raise (Underivable "Functor cannot be derived for this type")

  and tup context = function
    | [t] -> expr context t
    | ts ->
        let args, exps =
          (List.fold_right2
             (fun t n (p,e) ->
                let v = Printf.sprintf "t%d" n in
                  Ast.PaCom (_loc, <:patt< $lid:v$ >>, p),
                  Ast.ExCom (_loc, <:expr< $expr context t$ $lid:v$ >>, e))
             ts
             (List.range 0 (List.length ts))
             (<:patt< >>, <:expr< >>)) in
        let pat, exp = Ast.PaTup (_loc, args), Ast.ExTup (_loc, exps) in
          <:expr< fun $pat$ -> $exp$ >>

  and case context = function
    | (name, []) -> <:match_case< $uid:name$ -> $uid:name$ >>
    | (name, args) ->
        let f = tup context args
        and _, tpatt, texp = tuple (List.length args) in
          <:match_case< $uid:name$ $tpatt$ -> let $tpatt$ = ($f$ $texp$) in $uid:name$ ($texp$) >>

  and field context (name, (_,t), _) : Ast.expr =
    <:expr< $expr context t$ $lid:name$ >>

  let rhs context = function
    |`Fresh (_, _, `Private) -> raise (Underivable "Functor cannot be derived for private types")
    |`Fresh (_, Sum summands, _)  ->
       <:expr<  function $list:List.map (case context) summands$ >>
    |`Fresh (_, Record fields, _) ->
       <:expr< fun $record_pattern fields$ ->
                   $record_expr (List.map (fun ((l,_,_) as f) -> (l,field context f)) fields)$ >>
    |`Expr e                  -> expr context e
    |`Variant (_, tags) ->
       <:expr< function $list:List.map (polycase context) tags$ | _ -> assert false >>
    | `Nothing -> raise (Underivable "Cannot generate functor instance for the empty type")


  let maptype context name =
    let param_map = param_map context in
    let ctor_in = `Constr ([name], List.map (fun p -> `Param p) context.params) in
    let ctor_out = substitute param_map ctor_in  (* c[f_i/a_i] *) in
      List.fold_right (* (a_i -> f_i) -> ... -> c[a_i] -> c[f_i/a_i] *)
        (fun (p,_) out ->
           (<:ctyp< ('$lid:p$ -> '$lid:NameMap.find p param_map$) -> $out$>>))
        context.params
        (Untranslate.expr (`Function (ctor_in, ctor_out)))

   let signature context name : Ast.sig_item list =
     [ <:sig_item< type $list:sigdec context name$ >>;
       <:sig_item< val map : $maptype context name$ >> ]

  let decl context (name, _, r, _, _) : Camlp4.PreCast.Ast.module_binding =
    if name = "f" then
      raise (Underivable ("deriving: Functor cannot be derived for types called `f'.\n"
                          ^"Please change the name of your type and try again."))
    else
      <:module_binding<
         $uid:classname ^ "_" ^ name$
       : sig $list:signature context name$ end
       = $wrapper context name (rhs context r)$ >>

  let gen_sig context (tname, params, _, _, generated) =
    if tname = "f" then
      raise (Underivable ("deriving: Functor cannot be derived for types called `f'.\n"
                          ^"Please change the name of your type and try again."))
    else
      if generated then
        <:sig_item< >>
      else
        <:sig_item< module $uid:classname ^ "_" ^ tname$ :
                    sig type $tdec context tname$ val map : $maptype context tname$ end >>

  let generate decls =
    let context = setup_context decls in
    <:str_item< module rec $list:List.map (decl context) decls$ >>

  let generate_sigs decls =
    let context = setup_context decls in
    <:sig_item< $list:List.map (gen_sig context) decls$>>

  let generate_expr _ = assert false

end

module Functor = Pa_deriving_common.Base.Register(Description)(InContext)
