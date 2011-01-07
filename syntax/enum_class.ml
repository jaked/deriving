(*pp camlp4of *)

(* Copyright Jeremy Yallop 2007.
   This file is free software, distributed under the MIT license.
   See the file COPYING for details.
*)

open Defs

module Description : ClassDescription = struct
  let classname = "Enum"
  let runtimename = "Deriving_Enum"
  let default_module = Some "Defaults"
  let allow_private = false
  let predefs = [
    ["int"], "int";
    ["bool"], "bool";
    ["unit"], "unit";
    ["char"], "char";
  ]
  let depends = []
end

module InContext (L : Loc) : Class = struct

  open Base
  open Utils
  open Type
  open Camlp4.PreCast

  open L
  module Helpers = Base.InContext(L)(Description)
  open Helpers

  let wrap numbering = [ <:str_item< let numbering = $numbering$ >> ]

  let instance = object(self)
    inherit make_module_expr

    method sum ?eq ctxt (tname,_,_,_,_) summands =
    let numbering = 
      List.fold_right2
        (fun n ctor rest -> 
           match ctor with
             | (name, []) -> <:expr< ($uid:name$, $`int:n$) :: $rest$ >>
             | (name,_) -> raise (Underivable ("Enum cannot be derived for the type "^
                                  tname ^" because the constructor "^
                                  name^" is not nullary")))
        (List.range 0 (List.length summands))
        summands
        <:expr< [] >> in
    wrap numbering

    method variant ctxt decl (_, tags) = 
    let numbering = 
      List.fold_right2
        (fun n tagspec rest -> 
           match tagspec with
             | Tag (name, None) -> <:expr< (`$name$, $`int:n$) :: $rest$ >>
             | Tag (name, _) -> raise (Underivable ("Enum cannot be derived because the tag "^
                                                      name^" is not nullary"))
             | _ -> raise (Underivable ("Enum cannot be derived for this "
                                        ^"polymorphic variant type")))
        (List.range 0 (List.length tags))
        tags
        <:expr< [] >> in
    wrap numbering

    method tuple context _ = raise (Underivable "Enum cannot be derived for tuple types")
    method record ?eq _ (tname,_,_,_,_) = raise (Underivable
                                                 ("Enum cannot be derived for record types (i.e. "^
                                                    tname^")"))
  end

  let make_module_expr = instance#rhs
  let generate = default_generate ~make_module_expr ~make_module_type
  let generate_sigs = default_generate_sigs ~make_module_sig
  let generate_expr = instance#expr

end

module Enum = Base.Register(Description)(InContext)
