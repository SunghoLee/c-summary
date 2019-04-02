open OUnit
open Core
open Pervasives
open SemanticSummaryDomain

module JSON2Domain = struct
  open Yojson

  let to_string = Basic.to_string

  module VarMaker = struct
    let convert ?proc jvar = 
      match proc with
      | Some p ->
          Var.of_string ~proc:(Var.mk_scope p) jvar
      | None ->
          Var.of_string jvar
  end

  module JLoc2Loc = struct
    let convert ?proc jloc =
      if String.equal jloc "ret" then
        (match proc with
        | Some p ->
            Loc.mk_ret p
        | None ->
            failwith "Cannot make a return address for a non-specific function.")
      else if String.is_prefix jloc ~prefix:"loc_" then
        let name = String.sub jloc 4 ((String.length jloc) - 4) in
        let var = VarMaker.convert ?proc name in
        Loc.mk_const var
      else
        failwith "handle dynamic locations."
  end

  module JVal2AVS = struct
    let convert ?proc (jval: Basic.json) =
      let v = 
        (match jval with
        | `Null ->
            failwith "not supported type Null."
        | `Bool b ->
            failwith "not supported type bool."
        | `Int i ->
            Val.of_int (IInt.of_int i)
        | `Float f ->
            failwith "not supported type float."
        | `String s ->
            Val.of_str (SStr.of_string s)
        | `Assoc assoc ->
            failwith "not supported type assoc."
        | `List l ->
            failwith "not supported type list.")
      in
      AVS.singleton (v, Cst.cst_true)
      
  end

  module JEnv = struct
    let convert ?proc json =
      let jvars = Basic.Util.keys json in
      (fun env jvar ->
        let jloc = Basic.Util.to_string (Basic.Util.member jvar json) in
        let var = VarMaker.convert ?proc jvar in
        let loc = JLoc2Loc.convert ?proc jloc in
        Env.add var loc env)
      |> (fun f -> Caml.List.fold_left f Env.empty jvars)
  end

  module JHeap = struct
    let convert ?proc json =
      let jval_list = Basic.Util.to_assoc json in
      (fun heap (jloc, jval) ->
        let loc = JLoc2Loc.convert ?proc jloc in
        let avs = JVal2AVS.convert ?proc jval in
        Heap.add loc avs heap)
      |> (fun f -> Caml.List.fold_left f Heap.empty jval_list)
  end

  module JCallLogs = struct
    let convert ?proc json =
      CallLogs.empty
  end

  module JDomain = struct 
    let convert ?proc json = 
      let env = Basic.Util.member "env" json in 
      let heap = Basic.Util.member "heap" json in
      let logs = Basic.Util.member "logs" json in
      {env = JEnv.convert ?proc env; heap = JHeap.convert ?proc heap; logs = JCallLogs.convert ?proc logs}
  end

  let sol_name = "result.sol"

  let load () = Basic.from_file ((Caml.Sys.getenv "INFER_CWD") ^ "/" ^ sol_name)

  let get_funs json = Basic.Util.keys json

  let get_jdomain func json = Basic.Util.member func json

  let to_domain ?proc json = JDomain.convert ?proc json
end

let get_all_procs () =
  InferBase.ResultsDir.assert_results_dir "";
  Procedures.get_all (fun x y -> true) () 
  
let get_semantic_summary p =
  let summ = Summary.get p in 
  match summ with
  | Some s ->(
    match s.Summary.payloads.Payloads.semantic_summary with
    | Some _ as o -> 
        o
    | None -> 
        None)
  | None -> 
      None

let leq_avs lhs_avs rhs_avs =
  (fun (lhs_val, _) ->
    (fun ((rhs_val: Val.t), _) ->
      Val.(lhs_val <= rhs_val))
    |> (fun f -> AVS.exists f rhs_avs))
  |> (fun f -> AVS.for_all f lhs_avs)
    
let leq_heap lhs_heap rhs_heap =
  (fun loc lhs_avs ->
    match Heap.find_opt loc rhs_heap with
    | Some rhs_avs ->
        leq_avs lhs_avs rhs_avs
    | None ->
        false)
  |> (fun f -> Heap.for_all f lhs_heap)

let leq_dom {env=lhs_env; heap=lhs_heap; logs=lhs_logs} {env=rhs_env; heap=rhs_heap; logs=rhs_logs} =
  Env.(lhs_env <= rhs_env) &&
  leq_heap lhs_heap rhs_heap &&
  CallLogs.(lhs_logs <= rhs_logs)

let tests = "test suite for c-summary analyer" >::: [
  "Analysis result tests" >:: (fun _ -> 
    let procs = get_all_procs () in
    let json = JSON2Domain.load () in
    (fun proc ->
      let summ = get_semantic_summary proc in
      let proc_name = InferIR.Typ.Procname.to_string proc in
      let sol = JSON2Domain.to_domain ~proc:proc_name (JSON2Domain.get_jdomain proc_name json) in
      (match summ with
      | Some s ->
          let msg = Format.asprintf "Failed to compare\n#Res\n%a\n\n#Sol\n%a\n" Domain.pp s Domain.pp sol in
          assert_equal (leq_dom sol s) true ~msg
      | None ->
          Format.printf "%s -> NONE\n" (InferIR.Typ.Procname.to_string proc)))
    |> (fun f -> Caml.List.iter f procs))
]

let _ = run_test_tt_main tests
