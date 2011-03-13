(* Copyright Grégoire Henry 2010.
   This file is free software, distributed under the MIT license.
   See the file COPYING for details.
*)

(* Registering type derivers into type-conv. *)

open Camlp4.PreCast

open Pa_deriving_common.Base
open Pa_deriving_common.Type
open Pa_deriving_common.Extend

let derive deriver types =
  let loc = Ast.loc_of_ctyp types in
  let decls = display_errors loc Translate.decls types in
  display_errors loc deriver (loc, decls)

let register name (deriver, sigderiver) =
  let name = String.uncapitalize name in
  Pa_type_conv.add_generator name (derive deriver);
  Pa_type_conv.add_sig_generator name (derive sigderiver)

let _ = add_register_hook register
