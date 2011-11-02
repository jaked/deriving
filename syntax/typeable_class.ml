(* Copyright Jeremy Yallop 2007.
   This file is free software, distributed under the MIT license.
   See the file COPYING for details.
*)

open Pa_deriving_common.Defs

module Description : ClassDescription = struct
  let classname = "Typeable"
  let runtimename = "Deriving_Typeable"
  let default_module = Some "Defaults"
  let allow_private = true
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

  let mkName tname =
    let file_name, sl, _, _, _, _, _, _ = Loc.to_tuple _loc in
    Printf.sprintf "%s_%d_%f_%s" file_name sl (Unix.gettimeofday ()) tname

  let wrap type_rep = [ <:str_item< let type_rep = $type_rep$ >> ]

  let instance = object(self)

    inherit make_module_expr

    method tuple ctxt ts =
      let params =
        List.map (fun t -> <:expr< $self#call_expr ctxt t "type_rep"$ >>) ts in
      wrap <:expr< $uid:runtimename$.TypeRep.mkTuple $expr_list params$ >>

    method gen ?eq ctxt tname params constraints =
      let paramList =
	List.fold_right
          (fun p cdr ->
            <:expr< $self#call_expr ctxt (`Param p) "type_rep"$ :: $cdr$ >>)
          ctxt.params
	  <:expr< [] >> in
      wrap <:expr< $uid:runtimename$.TypeRep.mkFresh $str:mkName tname$ $paramList$ >>

    method sum ?eq ctxt tname params constraints _ =
      self#gen ~eq ctxt tname params constraints
    method record ?eq ctxt tname params constraints _ =
      self#gen ~eq ctxt tname params constraints

    method variant ctxt tname params constraints (_,tags) =
      let tags, extends =
	List.fold_left
          (fun (tags, extends) -> function
            | Tag (l, [])  -> <:expr< ($str:l$, None) :: $tags$ >>, extends
            | Tag (l, ts) ->
		<:expr< ($str:l$, Some $self#call_expr ctxt (`Tuple ts) "type_rep"$) ::$tags$ >>,
		extends
            | Extends t ->
		tags,
		<:expr< $self#call_expr ctxt t "type_rep"$::$extends$ >>)
          (<:expr< [] >>, <:expr< [] >>) tags in
      wrap <:expr< $uid:runtimename$.TypeRep.mkPolyv $tags$ $extends$ >>
  end

  let make_module_expr = instance#rhs
  let generate = default_generate ~make_module_expr ~make_module_type
  let generate_sigs = default_generate_sigs ~make_module_sig
  let generate_expr = instance#expr

end

module Typeable = Pa_deriving_common.Base.Register(Description)(InContext)
let depends = (module InContext : ClassDependency)
