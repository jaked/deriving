(* Copyright Jeremy Yallop 2007.
   This file is free software, distributed under the MIT license.
   See the file COPYING for details.
*)

open Pa_deriving_common.Defs

module Description : ClassDescription = struct
  let classname = "Dump"
  let runtimename = "Deriving_Dump"
  let default_module = Some "Defaults"
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
    ["float"], "float";
    ["num"], "num";
    ["string"], "string";
    ["list"], "list";
    ["option"], "option";
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
  open Description

  let wrap ?(buffer="buffer") ?(stream="stream") to_buffer from_stream =
    [ <:str_item< let to_buffer $lid:buffer$ = function $list:to_buffer$ >> ;
      <:str_item< let from_stream $lid:stream$ = $from_stream$ >> ]

  let instance = object (self)

    inherit make_module_expr

    method dump_int ctxt n =
      <:expr< $self#call_expr ctxt (`Constr (["int"],[])) "to_buffer"$
                 buffer $`int:n$ >>

    method read_int ctxt =
      <:expr< $self#call_expr ctxt (`Constr (["int"],[])) "from_stream"$ stream >>


    method nargs ctxt tvars args =
      let to_buffer id ty =
	<:expr< $self#call_expr ctxt ty "to_buffer"$ buffer $lid:id$ >> in
      let from_stream id ty e =
        <:expr< let $lid:id$ = $self#call_expr ctxt ty "from_stream"$ stream in
                $e$ >> in
      seq_list (List.map2 to_buffer tvars args),
      (fun expr -> List.fold_right2 from_stream tvars args expr)

    method tuple ctxt tys =
      let tvars, patt, expr = tuple (List.length tys) in
      let dumper, undump = self#nargs ctxt tvars tys in
      wrap [ <:match_case< $patt$ -> $dumper$ >> ] (undump expr)


    method case ctxt (ctor,args) n =
      match args with
      | [] ->
	  <:match_case< $uid:ctor$ -> $self#dump_int ctxt n$ >>,
          <:match_case< $`int:n$ -> $uid:ctor$ >>
      | _ ->
        let tvars, patt, expr = tuple (List.length args) in
	let expr = <:expr< $uid:ctor$ $expr$ >> in
        let dumper, undumper = self#nargs ctxt tvars args in
	<:match_case< $uid:ctor$ $patt$ -> $self#dump_int ctxt n$; $dumper$ >>,
	<:match_case< $`int:n$ -> $undumper expr$ >>

    method sum ?eq ctxt tname params constraints summands =
      let msg = "Dump: unexpected tag %d at character %d when deserialising " ^ tname in
      let dumpers, undumpers = List.split (List.mapn (self#case ctxt) summands) in
      let undumpers =
        <:expr< match $self#read_int ctxt$ with
	        $list:undumpers$
                | n -> raise ($uid:runtimename$.$uid:classname^ "_error"$
				(Printf.sprintf $str:msg$ n (Stream.count stream))) >>
      in
      wrap dumpers undumpers


    method field ctxt (name, (vars, ty), mut) =
      if mut = `Mutable then
        raise (Underivable (classname ^ " cannot be derived for record types "
			    ^ " with mutable fields (" ^ name ^ ")"));
      if vars <> [] then
	raise (Underivable (classname ^ " cannot be derived for record types "
			    ^ "with polymorphic fields"));
      <:expr< $self#call_expr ctxt ty "to_buffer"$ buffer $lid:name$ >>,
      <:binding< $lid:name$ = $self#call_expr ctxt ty "from_stream"$ stream >>

    method record ?eq ctxt tname params constraints fields =
       let dumpers, undumpers = List.split (List.map (self#field ctxt) fields) in
       let bind b e = <:expr< let $b$ in $e$ >> in
       let undump = List.fold_right bind undumpers (record_expression fields) in
       let dumper = <:match_case< $record_pattern fields$ -> $seq_list dumpers$ >> in
       wrap [dumper] undump


    method polycase ctxt tagspec n : Ast.match_case * Ast.match_case =
      match tagspec with
      | Tag (name, args) -> begin match args with
        | None ->
	    <:match_case< `$name$ -> $self#dump_int ctxt n$ >>,
            <:match_case< $`int:n$ -> `$name$ >>
        | Some e ->
	    let to_buffer =
	      <:expr< $self#call_expr ctxt e "to_buffer"$ buffer x >> in
	    let from_stream =
	      <:expr< $self#call_expr ctxt e "from_stream"$ stream >> in
	    <:match_case< `$name$ x -> $self#dump_int ctxt n$; $to_buffer$ >>,
            <:match_case< $`int:n$ -> `$name$ ($from_stream$) >> end
      | Extends t ->
          let patt, guard, cast = cast_pattern ctxt.argmap t in
	  let to_buffer =
	    <:expr< $self#call_expr ctxt t "to_buffer"$ buffer $cast$ >> in
	  let from_stream =
	    <:expr< $self#call_expr ctxt t "from_stream"$ stream >> in
          <:match_case< $patt$ when $guard$ -> $self#dump_int ctxt n$; $to_buffer$ >>,
          <:match_case< $`int:n$ -> ($from_stream$ :> a) >>

    method variant ctxt tname params constraints (_, tags) =
      let msg = "Dump: unexpected tag %d at character %d "
	        ^ "when deserialising polymorphic variant" in
      let dumpers, undumpers = List.split (List.mapn (self#polycase ctxt) tags) in
      let undumpers =
        <:expr< match $self#read_int ctxt$ with
	        $list:undumpers$
                | n -> raise ($uid:runtimename$.$uid:classname^ "_error"$
                                (Printf.sprintf $str:msg$ n (Stream.count stream))) >>
      in
      wrap (dumpers @ [ <:match_case< _ -> assert false >>]) undumpers

  end

  let make_module_expr = instance#rhs
  let generate = default_generate ~make_module_expr ~make_module_type
  let generate_sigs = default_generate_sigs ~make_module_sig
  let generate_expr = instance#expr

end

module Dump = Pa_deriving_common.Base.Register(Description)(InContext)
