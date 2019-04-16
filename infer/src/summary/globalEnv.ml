open SemanticSummaryDomain
open Pervasives

(* don't use this function outside of this module *)
let internal_get_glob_pvars () = 
  let globals = PreForGlobal.Storage.load () in
  PreForGlobal.NameType.fold (fun pvar typ res -> (pvar, typ) :: res) globals []

let init_fun_prefix = "__infer_globals_initializer_"

let init_fun_prefix_length = (String.length init_fun_prefix)

let is_global_var_init_fun pdesc = 
  let proc_name = Typ.Procname.to_string (Procdesc.get_proc_name pdesc) in
  String.is_prefix proc_name ~prefix:init_fun_prefix

let get_initialized_global_ext pdesc =
  let proc_name = Typ.Procname.to_string (Procdesc.get_proc_name pdesc) in
  let proc_tunit = (Procdesc.get_attributes pdesc).ProcAttributes.translation_unit in
  let var_name = String.sub proc_name init_fun_prefix_length (String.length proc_name - init_fun_prefix_length) in
  Caml.List.find
    (fun (pvar, _) -> 
      match Pvar.get_translation_unit pvar with
      | Some tunit ->
        ((Pvar.to_string pvar) = var_name) && (tunit = proc_tunit)
      | None ->
          false)
    (internal_get_glob_pvars ())

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
