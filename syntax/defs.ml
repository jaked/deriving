
open Type
open Camlp4.PreCast

type context = {
  (* mapping from type parameters to functor arguments *)
  argmap : Type.name Type.NameMap.t;
  (* ordered list of type parameters *)
  params : Type.param list;
  (* type names *)
  tnames : Type.NameSet.t;
  (* For class dependencies, e.g. Pickle. *)
  toplevel : (Type.name * Type.expr) option
}

module type Loc = sig
  val _loc : Loc.t (* location of the type definition being derived *)
end

type dependency = {
    d_classname: string;
    d_generate_expr: context -> Type.expr -> Ast.module_expr;
  }

module type ClassDescription = sig
  val classname: name
  val runtimename: name
  val default_module: name option
  val allow_private: bool
  val predefs: (Type.qname * Type.name) list
  val depends:  (Loc.t -> dependency) list
end

module type Class = sig
  val generate: Type.decl list -> Ast.str_item
  val generate_sigs: Type.decl list -> Ast.sig_item
  val generate_expr: context -> Type.expr -> Ast.module_expr
end

class type virtual generator = object

      method rhs: context -> Type.decl -> Ast.module_expr
      method expr: context -> Type.expr -> Ast.module_expr

      method mapply: context -> Ast.module_expr -> Type.expr list -> Ast.module_expr

      method constr: context -> Type.qname * Type.expr list -> Ast.module_expr
      method param: context -> Type.param -> Ast.module_expr

      method wrap: context -> Type.expr -> Ast.str_item list -> Ast.module_expr

      method call_expr: context -> Type.expr -> string -> Ast.expr

      method virtual sum:
	  ?eq:Type.expr -> context ->
	    Type.name -> Type.param list -> Type.constraint_ list ->
	      Type.summand list -> Ast.str_item list
      method virtual tuple: context -> Type.expr list -> Ast.str_item list
      method virtual variant:
	  context ->
	    Type.name -> Type.param list -> Type.constraint_ list ->
	      Type.variant -> Ast.str_item list
      method virtual record:
	  ?eq:Type.expr -> context ->
	    Type.name -> Type.param list -> Type.constraint_ list ->
	      Type.field list -> Ast.str_item list

      method class_: context -> [ `NYI ] -> Ast.str_item list
      method function_: context -> Type.expr * Type.expr -> Ast.str_item list
      method label:
          context ->
	    [ `NonOptional | `Optional ] * Type.name * Type.expr * Type.expr ->
	      Ast.str_item list
      method object_: context -> [ `NYI ] -> Ast.str_item list

    end

module type ClassHelpers = sig

  include Loc
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
      Type.name Type.NameMap.t -> ?param:string ->
	Type.expr -> Ast.patt * Ast.expr * Ast.expr

  (* For Functor only *)
  val modname_from_qname:  qname:string list -> classname:string -> Ast.ident
  val substitute: Type.name Type.NameMap.t -> Type.expr -> Type.expr
  val setup_context: Type.decl list -> context

  (* For Pickle only *)
  val instantiate_modargs_repr: Type.name Type.NameMap.t -> Type.repr -> Type.repr

  class virtual make_module_expr : generator
  val make_module_sig: context -> Type.decl -> Ast.module_type
  val make_module_type: context -> Type.decl -> Ast.module_type

  val default_generate:
      make_module_expr:(context -> Type.decl -> Ast.module_expr) ->
      make_module_type:(context -> Type.decl -> Ast.module_type) ->
	Type.decl list -> Ast.str_item
  val default_generate_sigs:
      make_module_sig:(context -> Type.decl -> Ast.module_type) ->
	Type.decl list -> Ast.sig_item

end
