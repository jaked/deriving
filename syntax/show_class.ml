(*pp camlp4of *)

(* Copyright Jeremy Yallop 2007.
   This file is free software, distributed under the MIT license.
   See the file COPYING for details.
*)

open Defs

module Description : ClassDescription = struct
  let classname = "Show"
  let runtimename = "Deriving_Show"
  let default_module = Some "Defaults"
  let allow_private = true
  let predefs = [
    ["int"      ], "int";
    ["bool"     ], "bool";
    ["unit"     ], "unit";
    ["char"     ], "char";
    ["int32"    ], "int32";
    ["Int32";"t"], "int32";
    ["int64"    ], "int64";
    ["Int64";"t"], "int64";
    ["nativeint"], "nativeint";
    ["float"    ], "float";
    ["num"], "num";
    ["string"   ], "string";
    ["list"     ], "list";
    ["ref"      ], "ref";
    ["option"   ], "option";
    ["array"    ], "array";
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
  open Description

  let wrap formatter =
    [ <:str_item< let format formatter = function $list:formatter$ >> ]

  let in_a_box box i e =
    <:expr<
      Format.$lid:box$ formatter $`int:i$;
      $e$;
      Format.pp_close_box formatter () >>

  let in_hovbox ?(indent = 0) = in_a_box "pp_open_hovbox" indent
  and in_box ?(indent = 0) = in_a_box "pp_open_box" indent

  let instance = object (self)

    inherit make_module_expr


    method nargs ctxt tvars args =
      match tvars, args with
      | id::ids, ty::tys ->
	  let format_expr id ty =
            <:expr< $self#call_expr ctxt ty "format"$ formatter $lid:id$ >> in
	  let format_expr' id ty =
	    <:expr< Format.pp_print_string formatter ",";
	            Format.pp_print_space formatter ();
	            $format_expr id ty$>> in
	  let exprs = format_expr id ty :: List.map2 format_expr' ids tys in
	  in_hovbox ~indent:1 (seq_list exprs)
      | _ -> assert false

    method tuple ctxt args =
      let n = List.length args in
      let tvars, tpatt, _ = tuple n in
      wrap [ <:match_case< $tpatt$ -> $self#nargs ctxt tvars args$ >> ]


    method case ctxt (name, args) =
      match args with
      | [] ->
	  <:match_case< $uid:name$ -> Format.pp_print_string formatter $str:name$ >>
      | _ ->
          let tvars, patt, exp = tuple (List.length args) in
	  let format_expr =
	    <:expr< Format.pp_print_string formatter $str:name$;
                    Format.pp_print_break formatter 1 2;
                    $self#nargs ctxt tvars args$ >> in
          <:match_case< $uid:name$ $patt$ -> $in_hovbox format_expr$ >>

    method sum ?eq ctxt tname params constraints summands =
      wrap (List.map (self#case ctxt) summands)


    method field ctxt (name, (vars, ty), mut) =
      if vars <> [] then
	raise (Underivable (classname ^ " cannot be derived for record types "
			    ^ "with polymorphic fields"));
      <:expr< Format.pp_print_string formatter $str:name ^ " = "$;
              $self#call_expr ctxt ty "format"$ formatter $lid:name$ >>

    method record ?eq ctxt tname params constraints fields =
      let format_fields =
	List.fold_left1
          (fun l r -> <:expr< $l$; Format.pp_print_string formatter "; "; $r$ >>)
          (List.map (self#field ctxt) fields) in
      let format_record =
	<:expr<
          Format.pp_print_char formatter '{';
          $format_fields$;
          Format.pp_print_char formatter '}'; >> in
      wrap [ <:match_case< $record_pattern fields$ -> $in_hovbox format_record$ >>]

    method polycase ctxt : Type.tagspec -> Ast.match_case = function
      | Tag (name, None) ->
	  let format_expr =
	    <:expr< Format.pp_print_string formatter $str:"`" ^ name ^" "$ >> in
          <:match_case< `$uid:name$ -> $format_expr$ >>
      | Tag (name, Some e) ->
	  let format_expr =
	    <:expr< Format.pp_print_string formatter $str:"`" ^ name ^" "$;
                    $self#call_expr ctxt e "format"$ formatter x >> in
          <:match_case< `$uid:name$ x -> $in_hovbox format_expr$ >>
      | Extends t ->
          let patt, guard, cast = cast_pattern ctxt t in
	  let format_expr =
	    <:expr< $self#call_expr ctxt t "format"$ formatter $cast$ >> in
          <:match_case< $patt$ when $guard$ -> $in_hovbox format_expr$ >>

    method variant ctxt tname params constraints (_,tags) =
      wrap (List.map (self#polycase ctxt) tags @ [ <:match_case< _ -> assert false >> ])

  end

  let make_module_expr = instance#rhs
  let generate = default_generate ~make_module_expr ~make_module_type
  let generate_sigs = default_generate_sigs ~make_module_sig
  let generate_expr = instance#expr

end

module Show = Base.Register(Description)(InContext)
