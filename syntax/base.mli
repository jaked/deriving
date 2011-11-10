
exception Underivable of string
exception NoSuchClass of string

open Camlp4.PreCast

val fatal_error : Loc.t -> string -> 'a
val display_errors : Loc.t -> ('a -> 'b) -> 'a -> 'b

val contains_tvars : Type.expr -> bool
val contains_tvars_decl : Type.decl -> bool

open Defs

module InContext(L : Loc)(D : ClassDescription) : ClassHelpers

val register : generator -> unit
val find : Type.name -> generator
val is_registered : Type.name -> bool

val add_register_hook: (generator -> unit) -> unit

module Register(Desc : ClassDescription)(MakeClass : ClassBuilder) : sig end

val derive_str : Loc.t -> Type.decl list -> generator -> Ast.str_item
val derive_sig : Loc.t -> Type.decl list -> generator -> Ast.sig_item
