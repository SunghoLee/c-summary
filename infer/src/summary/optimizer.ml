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

(* Env is reversable, because it has only one-to-one mapping. *)
module RevEnv = struct
  include PrettyPrintable.MakePPMap(Loc)

  let rev env = 
    Env.fold (fun x y e -> add y x e) env empty

  let pp = pp ~pp_value: Loc.pp
end

module RevDG = struct
  include PrettyPrintable.MakePPMap(Loc)

  let add from_loc to_loc dg = 
    match find_opt to_loc dg with
    | Some s -> 
      let from_set = LocSet.add from_loc s in
      add to_loc from_set dg
    | None -> 
      let from_set = LocSet.add from_loc LocSet.empty in
      add to_loc from_set dg

  let construct heap = 
    let f = fun from to_set dg ->
      let collect_locs = fun (value, _) locset -> 
        if Val.is_loc value then
          let loc = Val.to_loc value in
          LocSet.add loc locset
        else if Val.is_struct value then
          let str = Val.to_struct value in
          Struct.fold (fun field f_loc locset -> LocSet.add f_loc locset) str locset
        else 
          locset
      in 
      let locset = AVS.fold collect_locs to_set LocSet.empty in
      LocSet.fold (fun loc dg -> add from loc dg) locset dg
    in
    Heap.fold f heap empty

  let pp = pp ~pp_value: LocSet.pp
end

let closure loc dg = 
  let rec impl = fun loc acc dg ->
    match RevDG.find_opt loc dg with
    | Some from_set -> 
      LocSet.fold (fun from_loc acc -> 
          if LocSet.mem from_loc acc then acc 
          else impl from_loc (LocSet.add from_loc acc) dg) from_set acc
    | None -> 
      acc
  in
  impl loc (LocSet.add loc LocSet.empty) dg

let opt_heap heap env =
  let rev_env = RevEnv.rev env in
  let rev_dg = RevDG.construct heap in
  let f_red = fun loc _ ->
    closure loc rev_dg |>
    LocSet.for_all (fun loc -> 
      match RevEnv.find_opt loc rev_env with 
      | Some v -> 
        Var.is_temporal v
      | None -> 
        false )
  in
  let red_heap = Heap.filter f_red heap in
  let f_remove = fun loc _ ->
    match Heap.find_opt loc red_heap with
    | Some _ ->
        false
    | None ->
        true 
  in
  Heap.filter f_remove heap 

let opt_logs logs = 
  let opt_unit u = 
    let args = LogUnit.get_args u in
    let heap = LogUnit.get_heap u in
    let rev_dg = RevDG.construct heap in
    let f = fun loc _ -> 
      closure loc rev_dg |>
      LocSet.for_all (fun loc -> 
        if Caml.List.mem loc args then true 
        else false )
    in
    { u with LogUnit.heap = Heap.filter f heap }
  in
  CallLogs.map opt_unit logs

let rm_redundant state = 
  let env = Domain.get_env state in
  let heap = Domain.get_heap state in
  let logs = Domain.get_logs state in 
  let heap' = opt_heap heap env in
  let env' = Env.filter (fun v _ -> not (Var.is_temporal v)) env in
  let logs' = opt_logs logs in
  Domain.make env' heap' logs'

let optimize astate = 
  let astate' = rm_redundant astate in
  let env = Domain.get_env astate' in
  let heap = Domain.get_heap astate' in
  let logs = Domain.get_logs astate' in
  let opt_heap = fun loc avs heap ->
    let opt_avs = fun (value, cst) avs ->
      match SMTSolver.is_sat cst with
      | (true, cst') -> 
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
  let opt_logs = fun log -> 
    let heap = LogUnit.get_heap log in
    let heap' = Heap.fold opt_heap heap Heap.empty in
    LogUnit.update_heap heap' log
  in
  let heap' = Heap.fold opt_heap heap Heap.empty in
  let logs' = CallLogs.map opt_logs logs in
  Domain.make env heap' logs'
