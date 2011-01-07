
open Camlp4.PreCast

type context = {
  (* mapping from type parameters to functor arguments *)
  argmap : Type.name Type.NameMap.t;
  (* ordered list of type parameters *)
  params : Type.param list;
  (* type names *)
  tnames : Type.NameSet.t;
}

module type Loc = sig
  val loc : Loc.t (* location of the type definition being derived *)
end

module type ClassDescription = sig
  val classname: Type.name
  val default_module: Type.name option
  val allow_private: bool
end

module type Class = sig
  val generate: context -> Type.decl list -> Ast.str_item
  val generate_sigs: context -> Type.decl list -> Ast.sig_item
end

class type virtual generator = object

      method rhs: context -> Type.decl -> Ast.module_expr
      method expr: context -> Type.expr -> Ast.module_expr

      method mapply: context -> Ast.module_expr -> Type.expr list -> Ast.module_expr

      method constr: context -> Type.qname * Type.expr list -> Ast.module_expr
      method param: context -> Type.param -> Ast.module_expr

      method virtual sum:
	  ?eq:Type.expr -> context ->
	    Type.decl -> Type.summand list -> Ast.module_expr
      method virtual tuple: context -> Type.expr list -> Ast.module_expr
      method virtual variant:
          context ->
	    Type.decl -> Type.variant -> Ast.module_expr
      method virtual record:
	  ?eq:Type.expr -> context ->
	    Type.decl -> Type.field list -> Ast.module_expr

      method class_: context -> [ `NYI ] -> Ast.module_expr
      method function_: context -> Type.expr * Type.expr -> Ast.module_expr
      method label:
          context ->
	    [ `NonOptional | `Optional ] * Type.name * Type.expr * Type.expr ->
	      Ast.module_expr
      method object_: context -> [ `NYI ] -> Ast.module_expr

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
  val tuple: ?param:string -> int -> Ast.patt * Ast.expr

  val cast_pattern:
      context -> ?param:string -> Type.expr -> Ast.patt * Ast.expr * Ast.expr

  (* Should not be exported once "call_expr" is defined... *)
  val mproject: Ast.module_expr -> string -> Ast.expr
  val atype_expr: context -> Type.expr -> Ast.ctyp
  val atype: context -> Type.decl -> Ast.ctyp
  val apply_functor: Ast.module_expr -> Ast.module_expr list -> Ast.module_expr
  val modname_from_qname:  qname:string list -> classname:string -> Ast.ident
  val substitute: Type.name Type.NameMap.t -> Type.expr -> Type.expr

  (* For Pickle only *)
  val instantiate_modargs_repr: context -> Type.repr -> Type.repr

  class virtual make_module_expr : generator
  val make_module_sig: context -> Type.decl -> Ast.module_type
  val make_module_type: context -> Type.decl -> Ast.module_type

  val default_generate:
      make_module_expr:(context -> Type.decl -> Ast.module_expr) ->
      make_module_type:(context -> Type.decl -> Ast.module_type) ->
	context -> Type.decl list -> Ast.str_item
  val default_generate_sigs:
      make_module_sig:(context -> Type.decl -> Ast.module_type) ->
	context -> Type.decl list -> Ast.sig_item

end
