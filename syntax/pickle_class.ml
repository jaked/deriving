(* Copyright Jeremy Yallop 2007.
   This file is free software, distributed under the MIT license.
   See the file COPYING for details.
*)

open Pa_deriving_common.Defs

module Description : ClassDescription = struct
  let classname = "Pickle"
  let runtimename = "Deriving_Pickle"
  let default_module = Some "Defaults"
  let allow_private = false
  let predefs = [
    ["int"], "int";
    ["bool"], "bool";
    ["unit"], "unit";
    ["char"], "char";
    ["int32"], "int32";
    ["Int32";"t"], "int32";
    ["int64"], "int64";
    ["Int64";"t"], "int64";
    ["nativeint"], "nativeint";
    ["float"], "float";
    ["num"], "num";
    ["string"], "string";
    ["list"], "list";
    ["ref"], "ref";
    ["option"], "option";
  ]
  let depends = [Typeable_class.Typeable.depends; Eq_class.Eq.depends]
end

module InContext (L : Loc) : Class = struct

  open Pa_deriving_common.Base
  open Pa_deriving_common.Utils
  open Pa_deriving_common.Type
  open Camlp4.PreCast

  open Description
  open L
  module Helpers = Pa_deriving_common.Base.InContext(L)(Description)
  open Helpers
  open Description

  let bind, seq =
    let bindop = ">>=" and seqop = ">>" in
    <:expr< $lid:bindop$ >>, <:expr< $lid:seqop$ >>

  let wrap ctxt ~picklers ~unpickler =
    let unpickler = <:expr< let module R = Utils(Typeable) in $unpickler$ >> in
    let pickle = <:expr<
      let module W = Utils(Typeable)(Eq) in
      let rec pickle = function $list:picklers$ in pickle >> in
    [ <:str_item< open $uid:runtimename$.Write >>;
      <:str_item< let pickle = $pickle$ >>;
      <:str_item< open $uid:runtimename$.Read >>;
      <:str_item< let unpickle = $unpickler$ >> ]

  let instance = object (self)

    inherit make_module_expr

    method tuple ctxt tys =
      let ntys = List.length tys in
      let ids, tpatt,texpr = tuple ~param:"id" ntys in
      let picklers =
	let eidlist = expr_list (List.map (fun id -> <:expr< $lid:id$ >>) ids) in
        let inner =
          List.fold_right2
            (fun id ty expr ->
               <:expr< $bind$ ($self#call_expr ctxt ty "pickle"$ $lid:id$)
                              (fun $lid:id$ -> $expr$) >>)
            ids tys
            <:expr< W.store_repr this ($uid:runtimename$.Repr.make $eidlist$) >> in
        [ <:match_case< ($tpatt$ as obj) -> W.allocate obj (fun this -> $inner$) >>]
      and unpickler =
        let msg = "unexpected object encountered unpickling "
	          ^ string_of_int ntys ^ "-tuple" in
	let pidlist = patt_list (List.map (fun id -> <:patt< $lid:id$ >>) ids) in
        let inner =
          List.fold_right2
            (fun id ty expr ->
               <:expr< $bind$ ($self#call_expr ctxt ty "unpickle"$ $lid:id$)
		              (fun $lid:id$ -> $expr$) >>)
            ids tys
            <:expr< return $texpr$ >> in
          <:expr< R.tuple
            (function
               | $pidlist$ -> $inner$
               | _ -> raise ($uid:runtimename$.UnpicklingError $str:msg$)) >> in
        wrap ctxt ~picklers ~unpickler


    method case_pickle ctxt (name, params') n =
      let nparams = List.length params' in
      let ids = List.mapn (fun _ n -> Printf.sprintf "id%d" n) params' in
      let svalue = expr_list (List.map (fun id -> <:expr< $lid:id$>>) ids) in
      let repr =
	<:expr< $uid:runtimename$.Repr.make ~constructor:$`int:n$ $svalue$ >> in
      let expr = <:expr< W.store_repr thisid $repr$ >> in
      match params' with
      | [] ->
	  <:match_case< $uid:name$ as obj -> W.allocate obj (fun thisid -> $expr$) >>
      | _  ->
	  let vs, tpatt, _ = tuple ~param:"v" nparams in
	  let bind_param p (id, v) expr =
	    <:expr< $bind$ ($self#call_expr ctxt p "pickle"$ $lid:v$)
              (fun $lid:id$ -> $expr$)>> in
          let expr = List.fold_right2 bind_param params' (List.zip ids vs) expr in
	  <:match_case< $uid:name$ $tpatt$ as obj ->
                        W.allocate obj (fun thisid -> $expr$) >>

    method case_unpickle ctxt (name, params') n =
      match params' with
      | [] -> <:match_case< $`int:n$, [] -> return $uid:name$ >>
      | _ ->
	  let nparams = List.length params' in
	  let ids, _, texpr = tuple ~param:"id" nparams in
	  let ms = List.mapn (fun _ n -> Printf.sprintf "M%d" n) params' in
	  let bind_param t (id, m) (pat, exp) =
              <:patt< $lid:id$ :: $pat$ >>,
              <:expr< let module $uid:m$ = $self#expr ctxt t$ in
                      $bind$ ($uid:m$.unpickle $lid:id$)
		             (fun $lid:id$ -> $exp$) >> in
	    let patt, expr =
	      List.fold_right2 bind_param params' (List.zip ids ms)
		(<:patt< [] >>, <:expr< return ($uid:name$ $texpr$) >>) in
	    <:match_case< $`int:n$, $patt$ -> $expr$ >>

    method sum ?eq ctxt tname params constraints summands =
      let picklers = List.mapn (self#case_pickle ctxt) summands in
      let unpickler = <:expr<
	fun id ->
          let f = function
	      $list:List.mapn (self#case_unpickle ctxt) summands$
              | n,_ -> raise ($uid:runtimename$.UnpicklingError
				($str:"Unexpected tag when unpickling " ^ tname ^ ": "$
				 ^ string_of_int n)) in
          R.sum f id >> in
      wrap ctxt ~picklers ~unpickler


    method record_pickler ctxt fields =
      let ids = List.map (fun (id,_,_) -> <:expr< $lid:id$ >>) fields in
      let expr =
	<:expr< (W.store_repr this ($uid:runtimename$.Repr.make $expr_list ids$)) >> in
      let bind_field (id,(vars,t),_) e =
	if vars <> [] then
	  raise (Underivable (classname ^ " cannot be derived for record types "
			      ^ "with polymorphic fields"));
        <:expr< $bind$ ($self#call_expr ctxt t "pickle"$ $lid:id$)
                       (fun $lid:id$ -> $e$) >> in
      let inner = List.fold_right bind_field fields expr in
      <:match_case<
	  ($record_pattern fields$ as obj) -> W.allocate obj (fun this -> $inner$) >>


    method record_unpickle ctxt tname fields =
      let msg = "unexpected object encountered unpickling " ^ tname in
      let assignments =
	List.fold_right
          (fun (id,_,_) exp ->
            <:expr< this.Mutable.$lid:id$ <- $lid:id$; $exp$ >>)
          fields
	  <:expr< return self >> in
      let bind_field (id,(vars,t),_) exp =
	assert (vars = []);
	<:expr< $bind$ ($self#call_expr ctxt t "unpickle"$ $lid:id$)
                       (fun $lid:id$ -> $exp$) >> in
      let inner = List.fold_right bind_field fields assignments in
      let idpat = patt_list (List.map (fun (id,_,_) -> <:patt< $lid:id$ >>) fields) in
      let record =
	<:expr< R.record
          (fun self -> function
            | $idpat$ -> let this = (Obj.magic self : Mutable.t) in $inner$
            | _ -> raise ($uid:runtimename$.UnpicklingError $str:msg$))
	  $`int:List.length fields$ >> in
      let mutable_type =
	instantiate_modargs_repr ctxt.argmap
	  (Record (List.map (fun (n,p,_) -> (n,p,`Mutable)) fields)) in
      <:expr< let module Mutable = struct
                type $Ast.TyDcl (_loc, "t", [], Untranslate.repr mutable_type, [])$
              end in $record$ >>

    method record ?eq ctxt tname params constraints (fields : Pa_deriving_common.Type.field list) =
      wrap ctxt
        ~picklers:[self#record_pickler ctxt fields]
        ~unpickler:(self#record_unpickle ctxt tname fields)


    method polycase_pickle ctxt = function
    | Tag (name, None) -> <:match_case<
        (`$name$ as obj) ->
          W.allocate obj
              (fun thisid ->
                 W.store_repr thisid
                    ($uid:runtimename$.Repr.make ~constructor:$`int:tag_hash name$ [])) >>
    | Tag (name, Some t) -> <:match_case<
        (`$name$ v1 as obj) ->
           W.allocate obj
            (fun thisid ->
             $bind$ ($self#call_expr ctxt t "pickle"$ v1)
                    (fun mid ->
                    (W.store_repr thisid
                        ($uid:runtimename$.Repr.make ~constructor:$`int:tag_hash name$ [mid])))) >>
    | Extends t ->
        let patt, guard, cast = cast_pattern ctxt.argmap t in
	<:match_case<
            ($patt$) when $guard$ ->
            ($self#call_expr ctxt t "pickle"$ $cast$) >>

    method polycase_unpickler ctxt tname tags =
      let do_tag = function
      | (name, None)   ->
	  <:match_case< $`int:(tag_hash name)$, [] -> return `$name$ >>
      | (name, Some t) ->
	  <:match_case< $`int:(tag_hash name)$, [x] ->
	                $bind$ ($self#call_expr ctxt t "unpickle"$ x)
	                       (fun o -> return (`$name$ o)) >> in
      let do_extensions tys =
	(* Try each extension in turn.  If we get an UnknownTag failure,
           try the next one.  This is

           * safe because any two extensions that define the same tag
             must be compatible at that point

           * fast because we can tell on the first integer comparison
             whether we've picked the right path or not.

	 *)
	let fail =
          <:expr< raise ($uid:runtimename$.UnknownTag
			   (n, ($str:"Unexpected tag encountered during unpickling of "
                                     ^ tname$))) >> in
	let try_extension ty expr =
	  <:expr<
              let module M = $(self#expr ctxt ty)$ in
              try $expr$
              with $uid:runtimename$.UnknownTag _ ->
		(M.unpickle id :> a $uid:runtimename$.Read.m) >> in
        <:match_case< n,_ -> $List.fold_right try_extension tys fail$ >> in
      let tags, extensions = either_partition
          (function Tag (name,t) -> Left (name,t) | Extends t -> Right t) tags in
      let tag_cases = List.map do_tag tags in
      let extension_case = do_extensions extensions in
      <:expr< fun id -> R.sum (function $list:tag_cases @ [extension_case]$) id >>

    method variant ctxt tname params constraints (_, tags) =
      let wildcard = <:match_case< _ -> assert false >> in
      wrap ctxt
	~picklers:(List.map (self#polycase_pickle ctxt) tags @ [ wildcard ])
	~unpickler:(self#polycase_unpickler ctxt tname tags)

end

  let make_module_expr = instance#rhs
  let generate = default_generate ~make_module_expr ~make_module_type
  let generate_sigs = default_generate_sigs ~make_module_sig
  let generate_expr = instance#expr

end

module Pickle = Pa_deriving_common.Base.Register(Description)(InContext)
