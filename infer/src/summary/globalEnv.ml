open SemanticSummaryDomain

let get_glob_pvars () = []
  (*let globals = [] PreForGlobal.Storage.load () in
  PreForGlobal.NameType.fold
    (fun pvar typ res -> (pvar, typ) :: res)
    globals [] *)

let get_glob_vars () = []
  (*Caml.List.fold_right 
    (fun (pvar, typ) res -> (Var.of_pvar pvar, typ) :: res)
    (get_glob_pvars ()) []*)

let get_all_procs () = Procedures.get_all () 
