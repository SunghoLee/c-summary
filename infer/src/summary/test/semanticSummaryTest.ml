open OUnit
open Core
open Pervasives
open SemanticSummaryDomain

exception NotMatched of string

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
    let parse name = 
      try
        Lexing.from_string name
        |> LocParser.main LocLexer.token
      with Parsing.Parse_error -> failwith ("parsing failed: " ^ name) 

    let convert ?proc jloc =
      let name = String.sub jloc 3 ((String.length jloc) - 3) in
      let yloc = parse name in
      let res = Yloc.to_domain ?proc yloc in
      res
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
            if String.is_prefix s ~prefix:"<L>" then
              let loc = JLoc2Loc.convert ?proc s in
              Val.of_loc loc
            else 
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
      (fun jlog ->
        let rloc = Basic.Util.member "rloc" jlog |> Basic.Util.to_string |> JLoc2Loc.convert ?proc in
        let jfun = Basic.Util.member "jfun" jlog |> Basic.Util.to_string |> JNIFun.of_string in
        let args = Basic.Util.member "args" jlog |> Basic.Util.convert_each (fun jarg -> Basic.Util.to_string jarg |> JLoc2Loc.convert ?proc) in
        let heap = Basic.Util.member "heap" jlog |> JHeap.convert ?proc in
        LogUnit.mk rloc jfun args heap)
      |> (fun f -> Basic.Util.convert_each f json)
      |> Caml.List.to_seq
      |> CallLogs.of_seq
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
    
let leq_env lhs_env rhs_env =
  (fun key value -> 
    match Env.find_opt key rhs_env with
    | Some value' -> 
        if (Loc.compare value value') = 0 then
          true
        else
          raise (NotMatched (Format.asprintf "Not matched loc of var %a: %a <> %a" Var.pp key Loc.pp value Loc.pp value'))
    | None ->
        raise (NotMatched (Format.asprintf "No Location of var: %a" Var.pp key)))
  |> (fun f -> Env.for_all f lhs_env)

let leq_heap lhs_heap rhs_heap =
  (fun loc lhs_avs ->
    match Heap.find_opt loc rhs_heap with
    | Some rhs_avs ->
        if leq_avs lhs_avs rhs_avs then
          true
        else
          raise (NotMatched (Format.asprintf "%a is not included in %a" AVS.pp lhs_avs AVS.pp rhs_avs))
    | None ->
        raise (NotMatched (Format.asprintf "Not matched location: %a" Loc.pp loc)))
  |> (fun f -> Heap.for_all f lhs_heap)

let leq_unit: LogUnit.t -> LogUnit.t -> bool =
  fun {rloc=lhs_rloc; jfun=lhs_jfun; args=lhs_args; heap=lhs_heap} {rloc=rhs_rloc; jfun=rhs_jfun; args=rhs_args; heap=rhs_heap} ->
    if not JNIFun.(lhs_jfun <= rhs_jfun) then
      raise (NotMatched (Format.asprintf "Not mathced JNIFun: %a <> %a" JNIFun.pp lhs_jfun JNIFun.pp rhs_jfun))
    else 
      (fun lhs_loc rhs_loc ->
        if Loc.(lhs_loc <= rhs_loc) then 
          true
        else 
          raise (NotMatched (Format.asprintf "Not mathced arg: %a <> %a" Loc.pp lhs_loc Loc.pp rhs_loc)))
      |> (fun f -> Caml.List.for_all2 f (lhs_rloc :: lhs_args) (rhs_rloc :: rhs_args)) &&
      leq_heap lhs_heap rhs_heap

  
let leq_logs lhs_logs rhs_logs =
  (fun lhs_log ->
    let matched_logs = CallLogs.find_all lhs_log.LogUnit.rloc rhs_logs in
    if (CallLogs.cardinal matched_logs) = 0 then
      raise (NotMatched (Format.asprintf "Not mathced logs returning the location: %a" Loc.pp lhs_log.LogUnit.rloc))
    else 
      CallLogs.exists (fun rhs_log -> leq_unit lhs_log rhs_log) matched_logs)
  |> (fun f -> CallLogs.for_all f lhs_logs)


let leq_dom {env=lhs_env; heap=lhs_heap; logs=lhs_logs} {env=rhs_env; heap=rhs_heap; logs=rhs_logs} =
    leq_env lhs_env rhs_env &&
    leq_heap lhs_heap rhs_heap &&
    leq_logs lhs_logs rhs_logs

let tests = "test suite for c-summary analyer" >::: [
  "Analysis result tests" >:: (fun _ -> 
    let procs = get_all_procs () in
    let json = JSON2Domain.load () in
    (fun proc ->
      let summ = get_semantic_summary proc in
      let proc_name = InferIR.Typ.Procname.to_string proc in
      let proc_json = JSON2Domain.get_jdomain proc_name json in
      (match summ, proc_json with
      | _, `Null ->
          let msg = Format.asprintf "No solution for %s\n" proc_name in
          (* Only match function results described in the solution: just ignore this case *)
          assert_equal true true ~msg
      | Some s, _ ->
          let sol = JSON2Domain.to_domain ~proc:proc_name proc_json in
          let msg = Format.asprintf "Failed to compare\n#Res\n%a\n\n#Sol\n%a\n" Domain.pp s Domain.pp sol in
          try
            assert_equal (leq_dom sol s) true ~msg
          with NotMatched err ->
            assert_failure err))
    |> (fun f -> Caml.List.iter f @@ Caml.List.filter (fun p -> not(JniModel.is_jni p)) procs))
]

let _ = run_test_tt_main tests
