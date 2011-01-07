(*pp camlp4of *)

(* Copyright Jeremy Yallop 2007.
   This file is free software, distributed under the MIT license.
   See the file COPYING for details.
*)

open Defs

module Description : ClassDescription = struct
  type t
  let classname = "Dump"
  let default_module = Some "Defaults"
  let allow_private = false
end

module InContext (L : Loc) : Class = struct

  open Base
  open Utils
  open Type
  open Camlp4.PreCast

  open L
  module Helpers = Base.InContext(L)(Description)
  open Helpers

  let wrap ?(buffer="buffer") ?(stream="stream") to_buffer from_stream =
    [ <:str_item< let to_buffer $lid:buffer$ = function $list:to_buffer$ >> ;
      <:str_item< let from_stream $lid:stream$ = $from_stream$ >> ]

  let instance = object (self)
    inherit make_module_expr

    method nargs ctxt (exprs : (name * Type.expr) list) : Ast.expr * Ast.expr =
      List.fold_right
        (fun (id,t) (p,u) -> 
           <:expr< $self#call_expr ctxt t "to_buffer"$ buffer $lid:id$; $p$ >>,
           <:expr< let $lid:id$ = $self#call_expr ctxt t "from_stream"$ stream in $u$ >>)
        exprs (<:expr<>>, <:expr< $tuple_expr (List.map (fun (id,_) -> <:expr< $lid:id$ >>) exprs)$>>)

    method tuple ctxt ts = 
      let dumpers, undump = 
        let n = List.length ts in 
        let pinner, undump = self#nargs ctxt (List.mapn (fun t n -> (Printf.sprintf "v%d" n, t)) ts) in
        let patt, expr = tuple n in
          [ <:match_case< $patt$ -> $pinner$ >> ], undump in
        <:module_expr< $wrap dumpers undump$ >>

    method polycase ctxt tagspec n : Ast.match_case * Ast.match_case = 
      let dumpn = <:expr< Dump_int.to_buffer buffer $`int:n$ >> in
        match tagspec with
          | Tag (name, args) -> (match args with 
              | None   -> <:match_case< `$name$ -> $dumpn$ >>,
                          <:match_case< $`int:n$ -> `$name$ >>
              | Some e -> <:match_case< `$name$ x -> $dumpn$;
                                         $self#call_expr ctxt e "to_buffer"$ buffer x >>,
                          <:match_case< $`int:n$ -> 
                                        `$name$ ($self#call_expr ctxt e "from_stream"$ stream) >>)
          | Extends t -> 
              let patt, guard, cast = cast_pattern ctxt t in
                <:match_case< $patt$ when $guard$ -> 
                               $dumpn$; $self#call_expr ctxt t "to_buffer"$ buffer $cast$ >>,
                <:match_case< $`int:n$ -> ($self#call_expr ctxt t "from_stream"$ stream :> a) >>

    method case ctxt (ctor,args) n =
      match args with 
        | [] -> (<:match_case< $uid:ctor$ -> Dump_int.to_buffer buffer $`int:n$ >>,
                 <:match_case< $`int:n$ -> $uid:ctor$ >>)
        | _ -> 
        let nargs = List.length args in
        let patt, exp = tuple nargs in
        let dump, undump = self#nargs ctxt (List.mapn (fun t n -> (Printf.sprintf "v%d" n, t)) args) in
        <:match_case< $uid:ctor$ $patt$ -> 
                      Dump_int.to_buffer buffer $`int:n$;
                      $dump$ >>,
        <:match_case< $`int:n$ -> let $patt$ = $undump$ in $uid:ctor$ $exp$  >>
    
    method field ctxt : Type.field -> Ast.expr * Ast.expr = function
      | (name, _, `Mutable) -> 
          raise (Underivable ("Dump cannot be derived for record types with mutable fields ("^name^")"))
      | (name, ([], t), _) -> 
          <:expr< $self#call_expr ctxt t "to_buffer"$ buffer $lid:name$ >>,
          <:expr< $self#call_expr ctxt t "from_stream"$ stream >>
      | f -> raise (Underivable ("Dump cannot be derived for record types with polymorphic fields")) 

    method sum ?eq ctxt (tname,_,_,_,_) summands = 
      let msg = "Dump: unexpected tag %d at character %d when deserialising " ^ tname in
      let dumpers, undumpers = 
        List.split (List.mapn (self#case ctxt) summands) in
      let undumpers =
        <:expr< match Dump_int.from_stream stream with $list:undumpers$ 
                | n -> raise (Dump_error
				(Printf.sprintf $str:msg$ n (Stream.count stream))) >>
      in
      wrap dumpers undumpers

    method record ?eq ctxt decl fields = 
       let dumpers, undumpers = 
         List.split (List.map (self#field ctxt) fields) in
       let undump = 
         List.fold_right2
           (fun (field,_,_) undumper e -> 
              <:expr< let $lid:field$ = $undumper$ in $e$ >>)
           fields
           undumpers
           (record_expression fields) in
       let dumpers =
	 [ <:match_case< $record_pattern fields$ -> $List.fold_left1 seq dumpers$ >>] in
       wrap dumpers undump
   
    method variant ctxt decl (_, tags) = 
      let msg = "Dump: unexpected tag %d at character %d when deserialising polymorphic variant" in
      let dumpers, undumpers = 
        List.split (List.mapn (self#polycase ctxt) tags) in
      let undumpers =
          <:expr< match Dump_int.from_stream stream with $list:undumpers$ 
                  | n -> raise (Dump_error
                                  (Printf.sprintf $str:msg$ n (Stream.count stream))) >>
      in
      wrap (dumpers @ [ <:match_case< _ -> assert false >>]) undumpers
  end

  let make_module_expr = instance#rhs
  let generate = default_generate ~make_module_expr ~make_module_type
  let generate_sigs = default_generate_sigs ~make_module_sig

end

module Dump = Base.Register(Description)(InContext)
