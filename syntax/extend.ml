(* Copyright Jeremy Yallop 2007.
   This file is free software, distributed under the MIT license.
   See the file COPYING for details.
*)

(* Extend the OCaml grammar to include the `deriving' clause after
   type declarations in structure and signatures. *)

open Utils

open Camlp4.PreCast

let fatal_error loc msg =
  Syntax.print_warning loc msg;
  exit 1

let display_errors loc f p =
  try
    f p
  with
    Base.Underivable msg | Failure msg ->
      fatal_error loc msg

let derive_str loc (tdecls : Type.decl list) classname : Ast.str_item =
  display_errors loc
    (fun name -> fst (Base.find name) (loc, tdecls)) classname

let derive_sig loc tdecls classname : Ast.sig_item =
  display_errors loc
    (fun name -> snd (Base.find name) (loc, tdecls)) classname

module Deriving (S : Camlp4.Sig.Camlp4Syntax) =
struct

  include Syntax

  open Ast

  EXTEND Gram
  expr: LEVEL "simple"
  [
  [ TRY [e1 = val_longident ; "<" ; t = ctyp; ">" ->
     match e1 with
       | <:ident< $uid:classname$ . $lid:methodname$ >> ->
         if not (Base.is_registered classname) then
           fatal_error _loc ("deriving: "^ classname ^" is not a known `class'")
         else
           let module U = Type.Untranslate(struct let _loc = _loc end) in
           let binding = Ast.TyDcl (_loc, "inline", [], t, []) in
           let decls = display_errors _loc Type.Translate.decls binding in
             if List.exists Base.contains_tvars_decl decls then
               fatal_error _loc ("deriving: type variables cannot be used in `method' instantiations")
             else
               let tdecls = List.map U.decl decls in
               let m = derive_str _loc decls classname in
                 <:expr< let module $uid:classname$ =
                             struct
                               type $list:tdecls$
                               $m$
                               include $uid:classname ^ "_inline"$
                             end
                          in $uid:classname$.$lid:methodname$ >>
       | _ ->
           fatal_error _loc ("deriving: this looks a bit like a method application, but "
                            ^"the syntax is not valid");
  ]]];
  END

end

module M = Camlp4.Register.OCamlSyntaxExtension(Id)(Deriving)
