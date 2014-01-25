(* OASIS_START *)
(* OASIS_STOP *)

let _ =
  Ocamlbuild_plugin.dispatch
    (fun hook ->
       dispatch_default hook;
       match hook with
       | After_rules ->
           (* Internal syntax extension *)
           List.iter
             (fun dir ->
                let tag = "use_pa_deriving_" ^ dir and file = "syntax/" ^ dir ^ "/pa_deriving_" ^ dir ^ ".cma" in
                flag ["ocaml"; "compile"; tag] & S[A"-ppopt"; A file];
                flag ["ocaml"; "ocamldep"; tag] & S[A"-ppopt"; A file];
                flag ["ocaml"; "doc"; tag] & S[A"-ppopt"; A file];
                dep ["ocaml"; "ocamldep"; tag] [file])
             ["common"; "std"; "tc"; "classes"];
       | _ -> ())
