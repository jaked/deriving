(* Copyright Jeremy Yallop 2007.
   Copyright Grégoire Henry 2011.
   This file is free software, distributed under the MIT license.
   See the file COPYING for details.
*)

exception Underivable of string
exception NoSuchClass of string

open Camlp4.PreCast

val fatal_error : Loc.t -> string -> 'a
val display_errors : Loc.t -> ('a -> 'b) -> 'a -> 'b

open Defs

val derive_str : Loc.t -> Type.decl list -> generator -> Ast.str_item
val derive_sig : Loc.t -> Type.decl list -> generator -> Ast.sig_item

module RegisterClass(Desc : ClassDescription)(MakeClass : ClassBuilder) : sig
  val register_predefs : Type.qname -> Type.qname -> unit
end

module RegisterFullClass(Desc : ClassDescription)(MakeClass : FullBuilder) : sig
  val depends : (module DepClassBuilder)
  val register_predefs : Type.qname -> Type.qname -> unit
end

val is_registered : Type.name -> bool
val add_register_hook:
  ((module ClassDescription) -> generator -> unit) -> unit

val find : Type.name -> generator

(**/**)

module Register(Desc : ClassDescription)(MakeClass : InnerClassBuilder) : sig
  (* Side effects only *)
end

module Generator(Loc : Loc)(Desc : ClassDescription) : Generator
module AstHelpers(Loc : Loc) : AstHelpers
