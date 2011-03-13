(* Copyright Jeremy Yallop 2007.
   This file is free software, distributed under the MIT license.
   See the file COPYING for details.
*)

open Pa_deriving_common.Defs

module Description : ClassDescription = struct
  let classname = "Bounded"
  let runtimename = "Deriving_Bounded"
  let default_module = None
  let allow_private = false
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
    ["open_flag"], "open_flag";
    ["fpclass"], "fpclass";
  ]
  let depends = []
end

module InContext (L : Loc) : Class = struct

  open Pa_deriving_common.Base
  open Pa_deriving_common.Utils
  open Pa_deriving_common.Type
  open Camlp4.PreCast

  open L
  module Helpers = Pa_deriving_common.Base.InContext(L)(Description)
  open Helpers

  let wrap min max =
    [ <:str_item< let min_bound = $min$ >>; <:str_item< let max_bound = $max$ >> ]

  let instance = object (self)
    inherit make_module_expr

    method tuple ctxt ts =
      let expr t =
	let e = self#expr ctxt t in
        <:expr< let module M = $e$ in M.min_bound >>,
        <:expr< let module M = $e$ in M.max_bound >> in
      let minBounds, maxBounds = List.split (List.map expr ts) in
      wrap (tuple_expr minBounds) (tuple_expr maxBounds)

    method sum ?eq ctxt tname params constraints summands =
      let extract_name = function
        | (name,[]) -> name
        | (name,_) -> raise (Underivable ("Bounded cannot be derived for the type "
                                          ^ tname ^ " because the constructor "
                                          ^ name ^ " is not nullary")) in
      let names = List.map extract_name summands in
      wrap <:expr< $uid:List.hd names$ >> <:expr< $uid:List.last names$ >>

    method variant ctxt tname params constraints (_, tags) =
      let extract_name = function
        | Tag (name, None) -> name
        | Tag (name, _) -> raise (Underivable ("Bounded cannot be derived because "
					       ^ "the tag " ^ name^" is not nullary"))
        | _ -> raise (Underivable ("Bounded cannot be derived for this "
                                   ^ "polymorphic variant type")) in
      let names = List.map extract_name tags in
      wrap <:expr< `$List.hd names$ >> <:expr< `$List.last names$ >>

    (* should perhaps implement this one *)
    method record ?eq _ tname params constraints =
      raise (Underivable ("Bounded cannot be derived for record types (i.e. "
                          ^ tname ^ ")"))

  end

  let make_module_expr = instance#rhs
  let generate = default_generate ~make_module_expr ~make_module_type
  let generate_sigs = default_generate_sigs ~make_module_sig
  let generate_expr = instance#expr

end

module Bounded = Pa_deriving_common.Base.Register(Description)(InContext)
