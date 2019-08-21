open! IStd
open Core
module F = Format
module L = Logging
module Helper = HelperFunction
open SemanticSummaryDomain
open Pervasives
open SUtils

let rec is_typed_global (loc: Loc.t) = 
  match loc with
  | LocTop ->
      false
  | Explicit v ->
      let globals = GlobalEnv.internal_get_glob_pvars () in
      let global_locs = Caml.List.map (fun (pvar, typ) -> (Loc.of_pvar pvar, typ)) globals in
      (try (let _ = Caml.List.find (fun (gloc, typ) -> gloc = loc) global_locs in true) with _ -> false)
  | Implicit _ -> 
      false
  | Const _ ->
      false
  | Pointer (a, b, c) ->
      is_typed_global a
  | FunPointer _ ->
      false
  | Offset (a, b) ->
      is_typed_global a
  | Ret _ ->
      false

let rec infer_global_typ (loc: Loc.t) =
  match loc with
  | LocTop ->
      failwith (F.asprintf "[Top] It is not a global : %a" Loc.pp loc)
  | Explicit v ->
      let globals = GlobalEnv.internal_get_glob_pvars () in
      let global_locs = Caml.List.map (fun (pvar, typ) -> (Loc.of_pvar pvar, typ)) globals in
      let (_, t) = Caml.List.find (fun (gloc, typ) -> gloc = loc) global_locs in
      Typ.mk (Tptr (t, Pk_pointer))
  | Implicit _ -> 
      failwith (F.asprintf "[Impl] It is not a global : %a" Loc.pp loc)
  | Const _ ->
      failwith (F.asprintf "[Const] It is not a global : %a" Loc.pp loc)
  | Pointer (a, b, c) ->
      let t = infer_global_typ a in
      if Typ.is_pointer t then
        Typ.strip_ptr t
      else 
        failwith (F.asprintf "[Pointer] Is not a pointer? %a: %a\n@." Loc.pp loc (Typ.pp_full Pp.text) t)
  | FunPointer _ ->
      failwith (F.asprintf "[FunP] It is not a global : %a" Loc.pp loc)
  | Offset (a, b) ->(
      match (infer_global_typ a).Typ.desc with
      | Tarray {elt} ->
          elt
      | _ -> 
          failwith (F.asprintf "[Offset] It is not a global : %a" Loc.pp loc)
  )
  | Ret _ ->
      failwith (F.asprintf "[Ret] It is not a global : %a" Loc.pp loc)

let print_progress total cur = ()
  (*
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
  L.progress "%s (%d / %d)\r" (!str ^ "|") (int_of_float cur) (int_of_float total)*)

let rec expand_glob_loc tenv preloc nloc typ ienv = 
  match typ.Typ.desc with
  | Tptr (typ', _) -> 
      let nloc' = Loc.mk_concrete_pointer nloc in
      let preloc' = Loc.mk_concrete_pointer preloc in
      let ienv' = InstEnv.add preloc' (Val.singleton (nloc', Cst.cst_true)) ienv in
      expand_glob_loc tenv preloc' nloc' typ' ienv'

  | Tstruct name -> (
      try
        HelperFunction.get_fld_and_typs name tenv
        |> Caml.List.fold_left
            (fun ienv' (field, typ) ->
              let noffset = Loc.mk_offset nloc (Loc.to_const_typ_of_string field) in
              let poffset = Loc.mk_offset preloc (Loc.to_const_typ_of_string field) in
              let ienv'' = InstEnv.add poffset (Val.singleton (noffset, Cst.cst_true)) ienv' in
              expand_glob_loc tenv poffset noffset (Typ.mk (Tptr (typ, Pk_pointer))) ienv'') ienv
      with _ ->
        ienv
  )

  | Tarray {elt; length = Some i} -> (* fixed size arrays *)
      let index = (IntLit.to_int_exn i) - 1 in
      let rec mk_array i ienv' = 
        if i = -1 then ienv'
        else
          let noffset = Loc.mk_offset nloc (Loc.to_const_typ_of_z (Z.of_int i)) in
          let poffset = Loc.mk_offset preloc (Loc.to_const_typ_of_z (Z.of_int i)) in
          let ienv'' = InstEnv.add poffset (Val.singleton (noffset, Cst.cst_true)) ienv' in
          expand_glob_loc tenv poffset noffset (Typ.mk (Tptr (elt, Pk_pointer))) ienv''
          |> mk_array (i - 1)
      in
      mk_array index ienv
       
  | _ ->
      ienv

let rec fp_mk_ienv tenv caller_scope callee_heap caller_heap ienv =
  let module Cache = PrettyPrintable.MakePPMap(Loc) in
  let cache = ref Cache.empty in
  let inst_base finst base = Val.fold (fun base_v v -> Val.union v (finst base_v)) base Val.empty in
  let rec expand ((loc: Loc.t), _) ienv =
    try InstEnv.add loc (Cache.find loc !cache) ienv with _ -> (
      match loc with
      | Offset (base, index) ->
          let ienv' = expand (base, Cst.cst_true) ienv in
          let ins_base = InstEnv.find base ienv' in
          let ins_offset = inst_base (fun (b, cst) -> Val.singleton (Loc.mk_offset b index, cst)) ins_base in
          InstEnv.add loc ins_offset ienv'

      | Pointer (base, LocVar, _) ->
          let ienv' = expand (base, Cst.cst_true) ienv in
          let ins_base = InstEnv.find base ienv' in
          let ins_ptr = inst_base (fun (b, cst) -> Heap.find b caller_heap) ins_base in
          InstEnv.add loc ins_ptr ienv'

      | _ -> 
          ienv
    )
  in
  let fold_f loc value ienv = expand (loc, Cst.cst_true) ienv |> Val.fold expand value in
  Heap.fold fold_f callee_heap ienv

(* construct an instantiation environment at call sites. *)
let mk_ienv tenv caller_scope params args callee_heap caller_heap = 
    let start_gettimeofday = Unix.gettimeofday () in
    (*let () = L.progress "#Start making IEnv\n@." in*)
  let all_params = (GlobalEnv.get_glob_pvars () 
      |> Caml.List.map (fun (glob, typ) -> (Loc.of_pvar glob), Typ.mk (Tptr (typ, Pk_pointer))))
    @ Caml.List.map (fun (param, typ) -> (Loc.mk_explicit param), Typ.mk (Tptr (typ, Pk_pointer))) params
  in
  let all_param_gettimeofday = Unix.gettimeofday () in
    (*let () = L.progress "\t AllParams: %f\n@." (all_param_gettimeofday -. start_gettimeofday) in*)
  let heap' = Caml.List.fold_left2
    (fun heap (param, typ) arg -> Heap.add param arg heap)
    caller_heap all_params args
  in
  let ienv = Caml.List.fold_left
    (fun ienv (loc, typ) -> InstEnv.add loc (Val.singleton (loc, Cst.cst_true)) ienv)
    InstEnv.empty all_params
  in
  let res = fp_mk_ienv tenv caller_scope callee_heap heap' ienv |> InstEnv.optimize in
  let end_gettimeofday = Unix.gettimeofday () in
  (*let () = L.progress "\tDone.: %f\n@." (end_gettimeofday -. start_gettimeofday) in*)
  (*let () = L.progress "IENV: %a\n@." InstEnv.pp res in*)
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
            Cst.cst_or (Cst.cst_and (Cst.cst_eq iloc1 iloc2) (Cst.cst_and cst1 cst2)) cst
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
          (* TODO: need to strong update! because caller's value remain after instantiation! *)
          let pre_v = Heap.find loc' caller in
          (* Strange point! this cst is callee's one *)
          let merged_v = Helper.((ival ^ cst) 
            + (pre_v ^ (Cst.cst_not cst))) in
          let merged_v' = Val.optimize merged_v in
          (*let () = L.progress "Loc: %a\nCST: %a\nIVAL: %a\n PRE: %a\n Merged: %a\n@." Loc.pp loc' Cst.pp cst Val.pp ival Val.pp pre_v Val.pp merged_v' in*)
          if (Val.is_empty merged_v') then
            base
          else
            Heap.add loc' merged_v' base
        in
        let iloc_v = InstEnv.find loc ienv in
        Val.fold update_val iloc_v base
  in
  let res = Heap.fold f callee base in
  res
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
  let res = CallLogs.fold f callee_logs base in
  res
