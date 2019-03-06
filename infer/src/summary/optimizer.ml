open! IStd
open Core
module F = Format
module L = Logging
module Domain = SemanticSummaryDomain
open Domain

module SMTSolver = struct
  open Z3
  open Cst

  let ctx = 
    let cfg = [("model", "true")] in
    mk_context cfg

  let is_sat cst = 
    let expr = Z3Encoder.encode ctx cst in
    let sim_expr = Expr.simplify expr None in
    let sim_cst = Z3Encoder.decode sim_expr in
    let solver = Solver.mk_solver ctx None in
    let res = Solver.check solver [sim_expr] in
    match res with
    | UNSATISFIABLE -> 
      (false, sim_cst)
    | UNKNOWN -> 
      (true, sim_cst)
    | _ -> 
      (true, sim_cst)
end 

module LocSet = struct 
  include PrettyPrintable.MakePPSet(Loc)
end

let opt_heap heap locs = 
  let opt_avs_heap = fun loc avs heap ->
    let opt_avs = fun (value, cst) avs ->
      match SMTSolver.is_sat cst with
      | true, cst' ->
          AVS.add (value, cst') avs
      | _ ->
          avs
    in
    let value' = AVS.fold opt_avs avs AVS.empty in
    if AVS.is_empty value' then
      heap
    else
      Heap.add loc value' heap
  in
  let heap' = Heap.fold opt_avs_heap heap Heap.empty in
  let f_heap_closure = fun locset loc ->
    let rec f_avs_closure = fun (value, _) locset ->
      if Val.is_loc value then
        let loc = Val.to_loc value in
        let locset' = LocSet.add loc locset in
        match Heap.find_opt loc heap' with
        | Some avs -> 
            AVS.fold f_avs_closure avs locset'
        | None ->
            locset'
      else 
        locset
    in
    let locset' = LocSet.add loc locset in
    match Heap.find_opt loc heap' with
    | Some avs -> 
        AVS.fold f_avs_closure avs locset'
    | None ->
        locset'
  in
  let loc_closure = Caml.List.fold_left f_heap_closure LocSet.empty locs in
  Heap.filter (fun loc _ -> LocSet.mem loc loc_closure) heap'

let opt_logs logs = 
  let opt_unit u = 
    let args = LogUnit.get_args u in
    let heap = LogUnit.get_heap u in
    { u with LogUnit.heap = (opt_heap heap args) }
  in
  CallLogs.map opt_unit logs

let rm_redundant state = 
  let env = Domain.get_env state in
  let heap = Domain.get_heap state in
  let logs = Domain.get_logs state in 
  let env' = Env.filter (fun v _ -> not (Var.is_temporal v)) env in
  let non_temp_locs = Env.fold (fun _ loc loclist -> loc :: loclist) env' [] in
  let heap' = opt_heap heap non_temp_locs in
  let logs' = opt_logs logs in
  Domain.make env' heap' logs'

let optimize astate = 
  rm_redundant astate
