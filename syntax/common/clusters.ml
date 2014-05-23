(* Copyright Grégoire Henry 2011.
   This file is free software, distributed under the MIT license.
   See the file COPYING for details.
*)

open Utils
open Type

(* See cluster.mli for a description of "clusters". *)

let extract_recursive_calls decls : ESet.t list =
  let names = List.map (fun (name,_,_,_,_) -> name) decls in
  let obj = (object (self)
    inherit [ESet.t] fold as default
    method crush sets = List.fold_left ESet.union ESet.empty sets
    method expr e =
      match e with
      | `Constr ([name], args) as e when List.mem name names ->
	ESet.add (name, args) (default#expr e)
      | e -> default#expr e
    method decl d =
      match d with
      | (tname, params, `Fresh (_, GSum _, _), _, _) ->
          (* All GADT are considered recursives... cf. base.ml*)
          ESet.add (tname, List.map (fun p -> `Param p) params) (default#decl d)
      | _  -> default#decl d
  end) in
  List.map obj#decl decls

(** The function [close_decls decls] computes, for the set of type
    declarations [decls], the actual instances of these types that are
    used in their definitions.  It throws an exception if the set is
    known to be infinite (a.k.a. non-regural types). *)
let close_decls (decls: Type.decl list) : (Type.decl * ESet.t) list =

  let check_regular_instance name (name', args') =
    name <> name' ||
      List.for_all
      (function (`Constr _ | `Tuple _ | `Function _) as e -> not (contains_tvars e)
	| _ -> true) args' in

  let expand (tys : (Type.decl * ESet.t) list) name ty_set (name', args') =
    let ((_, params',_,_,_), ty_set') = List.find (fun ((n,_,_,_,_),_) -> n = name') tys in
    let subst = NameMap.fromList (List.map2 (fun (p, _) a -> p, a) params' args') in
    ESet.fold
      (fun (name'', args'') acc ->
	let new_ty = name'', List.map (substitute_expr subst) args'' in
	if not (check_regular_instance name new_ty) then
          failwith ("The following types contain non-regular recursion:\n   "
                    ^String.concat ", " (List.map (fun ((n,_,_,_,_),_)->n) tys)
                    ^"\nderiving does not support non-regular types");
	if ESet.mem new_ty ty_set then acc else ESet.add new_ty acc)
      ty_set' ESet.empty in

  let expands (tys : (Type.decl * ESet.t) list) =
    List.map
      (fun ((name,_,_,_,_),ty_set) ->
	ESet.fold
	  (fun ty acc -> ESet.union (expand tys name ty_set ty) acc)
	  ty_set ESet.empty)
      tys in

  let aggregate_new_tys (tys : (Type.decl * ESet.t) list) new_tys =
    List.map2 (fun (d,set) new_set -> d, ESet.union set new_set) tys new_tys in

  let rec loop_close_decls (tys : (Type.decl * ESet.t) list) new_tys =
    if List.for_all (fun l -> l = ESet.empty) new_tys then tys
    else
      let tys = aggregate_new_tys tys new_tys in
      let new_tys = expands tys in
      loop_close_decls tys new_tys
  in
  loop_close_decls
    (List.map (fun d -> d, ESet.empty) decls)
    (extract_recursive_calls decls)

(** The function [rename_param decl] rename the type parameters with
    'a 'b 'c ... *)
let rename_params (name, params, rhs, constraints, deriving as decl) =
  if deriving then decl else
    let map =
      List.mapn
        (fun (o, v) n ->
           let n' =
             if o.[0] = '_'
             then "_" ^ typevar_of_int n
             else typevar_of_int n in
           (o, (n', v)))
        params in
    let subst = NameMap.fromList (List.map (fun (o, (n, _)) -> o, n) map) in
    ((name, List.map snd map, rename_rhs subst rhs,
      List.map (rename_constraint subst) constraints, false))

(** Group type declaration (and the associated instances involved in
    recursion) by the set of freevars in there "associated recursives
    instances". *)
let aggregate_clusters decls =
  let add_instances acc (((name,params,_,_,_ : Type.decl) as decl), insts) =
    (* Determine types variables involved in recursion. *)
    let freevars = ESet.fold
      (fun (name, args) acc ->
	ParamSet.union (Type.free_tvars (`Constr ([name], args))) acc)
      insts ParamSet.empty in
    ParamSet.iter (* TODO error message instead of assert (unknown variable) *)
      (fun (n, _ as var) -> if not (List.exists (fun p -> var = p) params) then
          failwith ("Unkown variable " ^ n)
      ) freevars;
    assert (ParamSet.for_all (* TODO error message instead of assert (unknown variable) *)
	      (fun var -> List.exists (fun p -> var = p) params) freevars);
  (* Then regroups with instances that shares effective parameters. *)
    let rec loop acc =
      match acc with
	| [] -> [insts, freevars, [decl]]
	| (insts', vars, decls) :: acc when ParamSet.equal freevars vars ->
	  (ESet.union insts insts', vars, decl :: decls) :: acc
	| e :: acc -> e :: loop acc
    in
    loop acc in
  List.fold_left add_instances [] decls

let sort_freevars (fv: ParamSet.t) : param list =
  List.sort compare (ParamSet.fold (fun v acc -> v :: acc) fv [])

type cluster = {
  params: Type.param list;
  decls: Type.decl list;
  instances: (Type.name * Type.expr list) list;
}

let ( >>> ) x f = f x

let make decls =
  let sets =
    List.map rename_params decls
    >>> close_decls
    >>> aggregate_clusters
  in
  List.map
    (fun (insts, fv, decls) -> { instances = ESet.toList insts;
				 params = sort_freevars fv;
				 decls; })
    sets
