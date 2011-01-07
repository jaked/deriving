(*pp camlp4of *)

(* Copyright Jeremy Yallop 2007.
   This file is free software, distributed under the MIT license.
   See the file COPYING for details.
*)

open Defs

module Description : ClassDescription = struct
  let classname = "Show"
  let default_module = Some "Defaults"
  let allow_private = true
end

module InContext (L : Loc) : Class = struct

  open Base
  open Utils
  open Type
  open Camlp4.PreCast

  open L
  module Helpers = Base.InContext(L)(Description)
  open Helpers

  let wrap formatter =
    [ <:str_item< let format formatter = function $list:formatter$ >> ]

  let in_a_box box e =
    <:expr< 
      Format.$lid:box$ formatter 0;
      $e$;
      Format.pp_close_box formatter () >>

  let in_hovbox = in_a_box "pp_open_hovbox" and in_box = in_a_box "pp_open_box"


  let instance = object (self)
    inherit make_module_expr
    
    method polycase ctxt : Type.tagspec -> Ast.match_case = function
      | Tag (name, None) -> 
          <:match_case< `$uid:name$ -> 
                        Format.pp_print_string formatter $str:"`" ^ name ^" "$ >>
      | Tag (name, Some e) ->
          <:match_case< `$uid:name$ x ->
                         $in_hovbox <:expr< 
                            Format.pp_print_string formatter $str:"`" ^ name ^" "$;
                            $self#call_expr ctxt e "format"$ formatter x >>$ >>
      | Extends t -> 
          let patt, guard, cast = cast_pattern ctxt t in
            <:match_case<
              $patt$ when $guard$ -> 
              $in_hovbox <:expr< $self#call_expr ctxt t "format"$ formatter $cast$ >>$ >>

    method nargs ctxt (exprs : (name * Type.expr) list) : Ast.expr =
      match exprs with
        | [id,t] -> 
              <:expr< $self#call_expr ctxt t "format"$ formatter $lid:id$ >>
        | exprs ->
            let fmt = 
              "@[<hov 1>("^ String.concat ",@;" (List.map (fun _ -> "%a") exprs) ^")@]" in
              List.fold_left
                (fun f (id, t) ->
                   <:expr< $f$ $self#call_expr ctxt t "format"$ $lid:id$ >>)
                <:expr< Format.fprintf formatter $str:fmt$ >>
                exprs

    method tuple ctxt args = 
      let n = List.length args in
      let tvars, tpatt, _ = tuple n in
      wrap [ <:match_case< $tpatt$ -> $self#nargs ctxt (List.zip tvars args)$ >> ]

    method case ctxt : Type.summand -> Ast.match_case = 
      fun (name, args) ->
        match args with 
          | [] -> <:match_case< $uid:name$ -> Format.pp_print_string formatter $str:name$ >>
          | _ -> 
              let tvars, patt, exp = tuple (List.length args) in
                <:match_case<
                  $uid:name$ $patt$ ->
                  $in_hovbox <:expr<
                    Format.pp_print_string formatter $str:name$;
                Format.pp_print_break formatter 1 2;
                $self#nargs ctxt (List.zip tvars args)$ >>$ >>
    
    method field ctxt : Type.field -> Ast.expr = function
      | (name, ([], t), _) -> <:expr< Format.pp_print_string formatter $str:name ^ " ="$;
                                      $self#call_expr ctxt t "format"$ formatter $lid:name$ >>
      | f -> raise (Underivable ("Show cannot be derived for record types with polymorphic fields")) 

    method sum ?eq ctxt decl summands = wrap (List.map (self#case ctxt) summands)

    method record ?eq ctxt decl fields = wrap [ <:match_case<
      $record_pattern fields$ -> $in_hovbox
       <:expr<
          Format.pp_print_char formatter '{';
          $List.fold_left1
            (fun l r -> <:expr< $l$; Format.pp_print_string formatter "; "; $r$ >>)
            (List.map (self#field ctxt) fields)$;
          Format.pp_print_char formatter '}'; >>$ >>]

    method variant ctxt decl (_,tags) = wrap  (List.map (self#polycase ctxt) tags
                                                        @ [ <:match_case< _ -> assert false >> ])
  end

  let make_module_expr = instance#rhs
  let generate = default_generate ~make_module_expr ~make_module_type
  let generate_sigs = default_generate_sigs ~make_module_sig

end

module Show = Base.Register(Description)(InContext)
