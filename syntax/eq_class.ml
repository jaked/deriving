(* Copyright Jeremy Yallop 2007.
   This file is free software, distributed under the MIT license.
   See the file COPYING for details.
*)

open Pa_deriving_common.Defs

module Description : ClassDescription = struct
  let classname = "Eq"
  let runtimename = "Deriving_Eq"
  let default_module = None
  let allow_private = true
  let predefs = [
    ["unit"], "unit";
    ["bool"], "bool";
    ["char"], "char";
    ["int"], "int";
    ["int32"], "int32";
    ["Int32";"t"], "int32";
    ["int64"], "int64";
    ["Int64";"t"], "int64";
    ["nativeint"], "nativeint";
    ["float"], "float";
    ["num"], "num";
    ["list"], "list";
    ["option"], "option";
    ["string"], "string";
    ["ref"], "ref";
    ["array"], "array";
  ]
  let depends = []
end

module InContext (L : Loc) = struct

  open Pa_deriving_common.Base
  open Pa_deriving_common.Utils
  open Pa_deriving_common.Type
  open Camlp4.PreCast

  open L
  module Helpers = Pa_deriving_common.Base.InContext(L)(Description)
  open Helpers
  include Description

  let lprefix = "l" and rprefix = "r"

  let wrap eq =
    [ <:str_item< let eq l r = match l, r with $list:eq$ >>]

  let instance = object (self)

    inherit make_module_expr

    method tuple ctxt tys =
      let n = List.length tys in
      let lnames, lpatt, _ = tuple ~param:lprefix n in
      let rnames, rpatt, _ = tuple ~param:rprefix n in
      let test_and ty (lid, rid) e =
	<:expr< $self#call_expr ctxt ty "eq"$ $lid:lid$ $lid:rid$ && $e$ >> in
      let expr =
        List.fold_right2 test_and tys (List.zip lnames rnames) <:expr< true >> in
      wrap [ <:match_case< (($lpatt$),($rpatt$)) -> $expr$ >> ]


    method case ctxt (name,args) =
      match args with
      | [] -> <:match_case< ($uid:name$, $uid:name$) -> true >>
      | _ ->
          let nargs = List.length args in
          let _, lpatt, lexpr = tuple ~param:lprefix nargs
          and _, rpatt, rexpr = tuple ~param:rprefix nargs in
	  let patt = <:patt< ($uid:name$ $lpatt$, $uid:name$ $rpatt$) >> in
	  let eq =
	    <:expr< $self#call_expr ctxt (`Tuple args) "eq"$ $lexpr$ $rexpr$ >> in
          <:match_case< $patt$ -> $eq$ >>

    method sum ?eq ctxt tname params constraints summands =
      let wildcard =
	match summands with
	| [_] -> []
	| _ -> [ <:match_case< _ -> false >>] in
      wrap (List.map (self#case ctxt) summands @ wildcard)


    method field ctxt (name, (vars, ty), mut) =
      assert(mut <> `Mutable);
      if vars <> [] then
	raise (Underivable (classname ^ " cannot be derived for record types "
			    ^ "with polymorphic fields"));
      <:expr< $self#call_expr ctxt ty "eq"$ $lid:lprefix ^ name$ $lid:rprefix ^ name$ >>

    method record ?eq ctxt tname params constraints fields =
      if List.exists (function (_,_,`Mutable) -> true | _ -> false) fields then
	wrap [ <:match_case< (l,r) -> l==r >> ]
      else
	let lpatt = record_pattern ~prefix:lprefix fields in
	let rpatt = record_pattern ~prefix:rprefix fields in
	let test_and f e = <:expr< $self#field ctxt f$ && $e$ >> in
	let expr = List.fold_right test_and fields <:expr< true >> in
	wrap [ <:match_case< (($lpatt$), ($rpatt$)) -> $expr$ >> ]


    method polycase ctxt : Pa_deriving_common.Type.tagspec -> Ast.match_case = function
      | Tag (name, []) -> <:match_case< `$name$, `$name$ -> true >>
      | Tag (name, es) ->
	  <:match_case< `$name$ l, `$name$ r -> $self#call_expr ctxt (`Tuple es) "eq"$ l r >>
      | Extends t ->
          let lpatt, lguard, lcast = cast_pattern ctxt.argmap ~param:"l" t in
          let rpatt, rguard, rcast = cast_pattern ctxt.argmap ~param:"r" t in
	  let patt = <:patt< ($lpatt$, $rpatt$) >> in
	  let eq = <:expr< $self#call_expr ctxt t "eq"$ $lcast$ $rcast$ >> in
          <:match_case< $patt$ when $lguard$ && $rguard$ -> $eq$ >>

    method variant ctxt tname params constraints (spec, tags) =
      wrap (List.map (self#polycase ctxt) tags @ [ <:match_case< _ -> false >> ])

  end

  let make_module_expr = instance#rhs
  let generate = default_generate ~make_module_expr ~make_module_type
  let generate_sigs = default_generate_sigs ~make_module_sig
  let generate_expr = instance#expr

end

module Eq = Pa_deriving_common.Base.Register(Description)(InContext)
let depends = (module InContext : ClassDependency)
