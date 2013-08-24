(* Copyright Jeremy Yallop 2007.
   Copyright Grégoire Henry 2011.
   This file is free software, distributed under the MIT license.
   See the file COPYING for details.
*)

open Camlp4.PreCast

module type Loc = sig
  val _loc : Loc.t (* location of the type definition being derived *)
end

module type AstHelpers = sig

  module Untranslate : Type.Untranslate

  val seq: Ast.expr -> Ast.expr -> Ast.expr
  val seq_list: Ast.expr list -> Ast.expr

  val record_pattern: ?prefix:string -> Type.field list -> Ast.patt
  val record_expr: (string * Ast.expr) list -> Ast.expr
  val record_expression: ?prefix:string -> Type.field list -> Ast.expr

  val expr_list: Ast.expr list -> Ast.expr
  val patt_list: Ast.patt list -> Ast.patt

  val tuple_expr: Ast.expr list -> Ast.expr
  val tuple: ?param:string -> int -> string list * Ast.patt * Ast.expr

  val cast_pattern:
    Type.qname Type.NameMap.t  -> ?param:string -> Type.expr ->
    Ast.patt * Ast.expr * Ast.expr

  val modname_from_qname:
    qname:string list -> classname:string -> Ast.ident

end


module type Generator = sig

  type context

  module Loc : Loc
  module AstHelpers : AstHelpers

  val cast_pattern:
    context -> ?param:string -> Type.expr ->
    Ast.patt * Ast.expr * Ast.expr

  val instantiate_modargs_repr: context -> Type.repr -> Type.repr

  class virtual generator : object

    method pack:
      Type.qname Type.NameMap.t -> Type.expr -> Ast.module_expr -> Ast.expr
    method unpack:
      Type.qname Type.NameMap.t -> Type.expr -> Ast.expr -> Ast.module_expr

    method class_sig: Type.qname Type.NameMap.t -> Type.expr -> Ast.module_type

    method rhs: context -> Type.subst -> Type.decl -> Ast.module_expr
    method expr: context -> Type.expr -> Ast.module_expr

    method constr: context -> Type.qname * Type.expr list -> Ast.module_expr

    method param: context -> Type.param -> Ast.module_expr
    method gparam: context -> Type.param * Type.expr -> Ast.module_expr

    method wrap: context -> ?default:Type.name option -> Type.expr -> Ast.str_item list -> Ast.module_expr

    method call_expr: context -> Type.expr -> string -> Ast.expr
    method call_poly_expr: context -> Type.poly_expr -> string -> Ast.expr

    method virtual proxy: unit -> Type.name option * Ast.ident list
    method virtual sum:
	?eq:Type.expr -> context ->
	  Type.name -> Type.expr list -> Type.constraint_ list ->
	    Type.summand list -> Ast.str_item list
    method gsum:
	?eq:Type.expr -> context ->
	  Type.name -> Type.expr list -> Type.constraint_ list ->
	    Type.gsummand list -> Ast.str_item list
    method virtual tuple: context -> Type.expr list -> Ast.str_item list
    method virtual variant:
	context ->
	  Type.name -> Type.expr list -> Type.constraint_ list ->
	    Type.variant -> Ast.str_item list
    method virtual record:
	?eq:Type.expr -> context ->
	  Type.name -> Type.expr list -> Type.constraint_ list ->
	    Type.field list -> Ast.str_item list

    method class_: context -> [ `NYI ] -> Ast.str_item list
    method function_: context -> Type.expr * Type.expr -> Ast.str_item list
    method label:
      context ->
	[ `NonOptional | `Optional ] * Type.name * Type.expr * Type.expr ->
	  Ast.str_item list
    method object_: context -> [ `NYI ] -> Ast.str_item list

  end

  val generate: generator -> Type.decl list -> Ast.str_item
  val generate_sigs: generator -> Type.decl list -> Ast.sig_item
  val generate_expr:
    generator ->
    Ast.module_expr Type.EMap.t ->
    Type.qname Type.NameMap.t ->
    Type.expr -> Ast.module_expr

end

(** *)

module type Class = sig
  val generate: Type.decl list -> Ast.str_item
  val generate_sigs: Type.decl list -> Ast.sig_item
end

module type ClassBuilder = functor (Generator : Generator) -> Class
module type InnerClassBuilder = functor (Loc: Loc) -> Class

module type FullClass = sig
  val classname: Type.name
  val runtimename: Type.name
  include Class
  val generate_expr:
    Ast.module_expr Type.EMap.t ->
    Type.qname Type.NameMap.t ->
    Type.expr -> Ast.module_expr
end

module type FullClassBuilder = functor (Generator: Generator) -> FullClass
module type DepClassBuilder = functor (Loc: Loc) -> FullClass

module type ClassDescription = sig
  val classname: Type.name
  val runtimename: Type.name
  val default_module: Type.name option
  val alpha: Type.name option
  val allow_private: bool
  val predefs: (Type.qname * Type.qname) list
  val depends: (module DepClassBuilder) list
end

type generator = (module InnerClassBuilder)
