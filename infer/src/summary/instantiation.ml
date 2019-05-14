open! IStd
open Core
module F = Format
module L = Logging
module Helper = HelperFunction
open SemanticSummaryDomain
open Pervasives
open SUtils

let print_progress total cur = 
  let total = float_of_int total in
  let cur = float_of_int cur in
  let base = if total <. 100.0 then 1.0 else total /. 100.0 in
  let c = cur /. base in
  let t = ( total /. base ) -. c in
  let str = ref "" in
  for i = 1 to (int_of_float c) do 
      str := !str ^ "="
  done;
  for i = 1 to (int_of_float t) do
      str := !str ^ " "
  done;
  L.progress "%s (%d / %d)\r" (!str ^ "|") (int_of_float cur) (int_of_float total)

let rec fp_mk_ienv caller_scope callee_heap caller_heap ienv =
  let module Cache = PrettyPrintable.MakePPMap(Loc) in
  let cache = ref Cache.empty in
  let rec expand ((loc: Loc.t), _) ienv =
    try InstEnv.add loc (Cache.find loc !cache) ienv with _ ->
      match loc with
      | Offset (base, index) ->
          let ienv' = expand (base, Cst.cst_true) ienv |> expand (index, Cst.cst_true) in
          let ins_base = InstEnv.find base ienv' in
          let ins_index = InstEnv.find index ienv' in
          Helper.(ins_base * ins_index) 
          |> Caml.List.fold_left (fun ienv ((b, b_cst), (i, i_cst)) -> 
              let v = Val.singleton (Loc.mk_offset b i, Cst.cst_and b_cst i_cst) in
              cache := Cache.add loc v !cache;
              InstEnv.add loc v ienv) ienv'
      | Pointer base ->
          let ienv' = expand (base, Cst.cst_true) ienv in
          let ins_base = InstEnv.find base ienv' in
          Val.fold 
            (fun (b, cst) ienv ->
              (match Heap.find_opt b caller_heap with
              | Some v ->
                  let v' = Helper.(v ^ cst) in
                  cache := Cache.add loc v' !cache;
                  InstEnv.add loc v' ienv
              | None ->
                  if Loc.is_in caller_scope b || Loc.is_in VVar.glob_scope b then
                    let ptr = Loc.mk_pointer b in
                    let v = (Val.singleton (ptr, cst)) in
                    cache := Cache.add loc v !cache;
                    InstEnv.add loc v ienv
                  else ienv))
            ins_base ienv'
      | _ -> 
          ienv
  in
  let fold_f loc value ienv = expand (loc, Cst.cst_true) ienv |> Val.fold expand value in
  Heap.fold fold_f callee_heap ienv

(* construct an instantiation environment at call sites. *)
let mk_ienv tenv caller_scope params args callee_heap caller_heap = 
    let start_gettimeofday = Unix.gettimeofday () in
    let () = L.progress "#Start making IEnv\n@." in
  let all_params = (GlobalEnv.get_glob_pvars () 
      |> Caml.List.map (fun (glob, typ) -> (Loc.of_pvar glob), Typ.mk (Tptr (typ, Pk_pointer))))
    @ Caml.List.map (fun (param, typ) -> (Loc.mk_explicit param), Typ.mk (Tptr (typ, Pk_pointer))) params
  in
  let all_param_gettimeofday = Unix.gettimeofday () in
    let () = L.progress "\t AllParams: %f\n@." (all_param_gettimeofday -. start_gettimeofday) in
  let heap' = Caml.List.fold_left2
    (fun heap (param, typ) arg -> Heap.add param arg heap)
    caller_heap all_params args
  in
  let ienv = Caml.List.fold_left
    (fun ienv (loc, typ) -> InstEnv.add loc (Val.singleton (loc, Cst.cst_true)) ienv)
    InstEnv.empty all_params
  in
  let res = fp_mk_ienv caller_scope callee_heap heap' ienv |> InstEnv.optimize in
  let end_gettimeofday = Unix.gettimeofday () in
  let () = L.progress "\tDone.: %f\n@." (end_gettimeofday -. start_gettimeofday) in
  res

(* instantiate a constraint *)
let rec inst_cst : Cst.t -> InstEnv.t -> Cst.t =
  fun cst ienv -> 
    match cst with
    | Or (cst1, cst2) ->
        Or (inst_cst cst1 ienv, inst_cst cst2 ienv)
    | And (cst1, cst2) ->
        And (inst_cst cst1 ienv, inst_cst cst2 ienv)
    | Not cst ->
        Not (inst_cst cst ienv)
    | Eq (loc1, loc2) ->
        let iloc1_v = InstEnv.find loc1 ienv in
        let iloc2_v = InstEnv.find loc2 ienv in
        let iter_iloc1 = fun (iloc1, cst1) cst ->
          let iter_iloc2 = fun (iloc2, cst2) cst ->
            Cst.cst_and (Cst.cst_and (Cst.cst_eq iloc1 iloc2) (Cst.cst_and cst1 cst2)) cst
          in
          Val.fold iter_iloc2 iloc2_v cst
        in
        Val.fold iter_iloc1 iloc1_v Cst.cst_true
    | _ as cst ->
        cst

(* instantiate an Abstract Value Set *)
let inst_value v ienv = 
  let inst_val = fun (loc, cst) res -> 
    let cst' = inst_cst cst ienv in
    Helper.(((InstEnv.find loc ienv) ^ cst') + res)
  in
  Val.fold inst_val v Val.empty

(* compose caller and callee heaps at call instructions. *)
let comp_heap base caller callee ienv = 
  let f = fun (loc: Loc.t) v base -> 
    match loc with
    | Explicit _ ->
        base
    | Implicit _ | Ret _ ->
        Heap.add loc (inst_value v ienv) base 
    | _ ->
        let ival = inst_value v ienv in
        let update_val = fun (loc', cst) base -> 
          let pre_v = Heap.find loc' caller in
          let merged_v = Helper.((ival ^ cst) 
            + (pre_v ^ (Cst.cst_not cst))) in
          let merged_v' = Val.optimize merged_v in
          if (Val.is_empty merged_v') then
            base
          else
            Heap.weak_update loc' merged_v' base
        in
        let iloc_v = InstEnv.find loc ienv in
        Val.fold update_val iloc_v base
  in
  Heap.fold f callee base
  (*
  let new_heap = Heap.fold f callee base in
  Heap.fold Heap.add new_heap base*)

(* compose caller and callee logs at call instructions. *)
let comp_log call_site base callee_logs caller_heap ienv = 
  let f = fun log base ->
    let heap' = 
      comp_heap caller_heap caller_heap (LogUnit.get_heap log) ienv 
    in
    let log' = LogUnit.optimize (LogUnit.update_heap heap' log) in
    let log'' = LogUnit.append_call_sites call_site log' in
    CallLogs.add log'' base
  in
  CallLogs.fold f callee_logs base 
