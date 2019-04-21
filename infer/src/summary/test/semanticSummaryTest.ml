open OUnit
open Core
open Pervasives
open SemanticSummaryDomain

exception NotMatched of string

module JSON2Domain = struct
  open Yojson

  let to_string = Basic.to_string

  module JLoc2Loc = struct
    let parse name = 
      try
        Lexing.from_string name
        |> LocParser.main LocLexer.token
      with Parsing.Parse_error -> failwith ("parsing failed: " ^ name) 

    let convert ?file ?proc ?ln jloc =
      let yloc = parse jloc in
      let res = Yloc.to_domain ?file ?proc ?ln yloc in
      res
  end

  module JVal2Val = struct
    let convert ?file ?proc ?ln (jval: Basic.json) =
      let v = 
        (match jval with
        | `String s ->
            JLoc2Loc.convert ?file ?proc ?ln s
        | `Int i ->
            JLoc2Loc.convert ?file ?proc ?ln (string_of_int i)
        | _ ->
            failwith (Format.asprintf "not supported type list: %s" (Basic.to_string jval)))
      in
      Val.singleton (v, Cst.cst_true)
  end

  module JHeap = struct
    let convert ?file ?proc ?ln json =
      let jval_list = Basic.Util.to_assoc json in
      Caml.List.fold_left 
        (fun heap (jloc, jval) ->
          heap |> (JVal2Val.convert ?file ?proc ?ln jval |> (JLoc2Loc.convert ?file ?proc ?ln jloc |> Heap.add)))
        Heap.empty jval_list
  end

  module JCallLogs = struct
    let to_lncol location =
      let open Caml in
      let sep_index = String.index location ':' in
      let ln = String.sub location 0 sep_index |> int_of_string in
      let col = String.sub location (sep_index + 1) ((String.length location) - (sep_index + 1)) |> int_of_string in
      ln, col

    let convert ?file ?proc json =
      (fun jlog ->
        let ln = Basic.Util.member "ln" jlog |> Basic.Util.to_string in
        let iln, icol = to_lncol ln in
        let pos = [](* CallSite.list_of_string ln *) in
        let rloc = Loc.mk_implicit (ln ^ ":ret") in
        let jfun = Basic.Util.member "jfun" jlog |> Basic.Util.to_string |> JNIFun.of_string in
        let args = Basic.Util.member "args" jlog |> Basic.Util.convert_each (fun jarg -> Basic.Util.to_string jarg |> JLoc2Loc.convert ?file ?proc ~ln) in
        let heap = Basic.Util.member "heap" jlog |> JHeap.convert ?file ?proc ~ln in
        LogUnit.mk pos rloc jfun args heap)
      |> (fun f -> Basic.Util.convert_each f json)
      |> Caml.List.to_seq
      |> CallLogs.of_seq
  end

  module JDomain = struct 
    let convert ?proc json = 
      let file = Basic.Util.member "file" json |> Basic.Util.to_string in
      let heap = Basic.Util.member "heap" json in
      let logs = Basic.Util.member "logs" json in
      {heap = JHeap.convert ~file ?proc heap; logs = JCallLogs.convert ~file ?proc logs}
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
  
let get_semantic_summary f =
  let proc_opt = get_all_procs () |> Caml.List.find_opt (fun p -> (InferIR.Typ.Procname.to_string p) = f) in
  match proc_opt with
  | Some p ->(
    let summ = Summary.get p in 
    match summ with
    | Some s ->(
      match s.Summary.payloads.Payloads.semantic_summary with
      | Some _ as o -> 
          o
      | None -> 
          None)
    | None -> 
        None)
  | None ->
      None

let leq_val lhs_val rhs_val =
  (fun (lhs_loc, _) ->
    (fun (rhs_loc, _) ->
      Loc.(lhs_loc <= rhs_loc))
    |> (fun f -> Val.exists f rhs_val))
  |> (fun f -> Val.for_all f lhs_val)
    
let leq_heap lhs_heap rhs_heap =
  (fun loc lhs_val ->
    match Heap.find_opt loc rhs_heap with
    | Some rhs_val ->
        if leq_val lhs_val rhs_val then
          true
        else
          raise (NotMatched (Format.asprintf "%a is not included in %a\n==RES==\n%a\n==SOL==\n%a\n" Val.pp lhs_val Val.pp rhs_val Heap.pp rhs_heap Heap.pp lhs_heap))
    | None ->
        raise (NotMatched (Format.asprintf "Not matched location: %a in \n==RES==\n%a\n==SOL==\n%a\n" Loc.pp loc Heap.pp rhs_heap Heap.pp lhs_heap)))
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

let leq_dom {heap=lhs_heap; logs=lhs_logs} {heap=rhs_heap; logs=rhs_logs} =
    leq_heap lhs_heap rhs_heap &&
    leq_logs lhs_logs rhs_logs

let tests = "test suite for c-summary analyer" >::: [
  "Analysis result tests" >:: (fun _ -> 
    let procs = get_all_procs () in
    let json = JSON2Domain.load () in
    (fun proc ->
      let summ = get_semantic_summary proc in
      let proc_json = JSON2Domain.get_jdomain proc json in
      (match summ, proc_json with
      | None, _ ->
          let msg = Format.asprintf "No result for %s\n" proc in
          (* Only match function results described in the solution: just ignore this case *)
          assert_failure msg
      | Some s, _ ->
          let sol = JSON2Domain.to_domain ~proc:proc proc_json in
          let msg = Format.asprintf "Failed to compare\n#Res\n%a\n\n#Sol\n%a\n" Domain.pp s Domain.pp sol in
          try
            assert_equal (leq_dom sol s) true ~msg
          with NotMatched err ->
            assert_failure err))
    |> (fun f -> Caml.List.iter f @@ JSON2Domain.get_funs json))
]

let _ = run_test_tt_main tests
