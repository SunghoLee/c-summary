open! IStd
open Core
module F = Format
module L = Logging
module Helper = HelperFunction
open SemanticSummaryDomain
open Pervasives
open SUtils

(* return variables defined as parameters *)
let fun_params pdesc =
    let attrs = Procdesc.get_attributes pdesc in
    let scope = Var.mk_scope (Typ.Procname.to_string (Procdesc.get_proc_name pdesc)) in
    let args = attrs.formals in
    Caml.List.map (fun (m, typ) -> ((Var.of_string (Mangled.to_string m) ~proc:scope), typ)) args

(* return a location mapped by the location `loc' in the instantiation environment. 
 * because each mapping of the ienv is one-to-one, there is one and only one location mapped by `loc'. *)
let loc_from_ienv loc ienv =
  let avs = InstEnv.find loc ienv in
  if (AVS.cardinal avs = 1) then
    let (value, cst) = AVS.min_elt avs in
    Val.to_loc value
  else
    failwith "The location does not have an one-to-one mapping in the instantiation environment."

let rec mk_ienv_param_loc: Tenv.t -> Loc.t -> Typ.t -> Heap.t -> Heap.t -> InstEnv.t -> InstEnv.t =
    fun tenv loc typ callee caller ienv ->
      let expand_offset loc ins_avs offset_typ ienv =
        let offsets = Heap.find_offsets_of loc callee in
        let () = L.progress "Offsets of %a: %a\n@." Loc.pp loc LocSet.pp offsets in
        (fun (loc: Loc.t) ienv -> (
          match loc with
          | Offset (_, index) ->
              (fun ((value: Val.t), cst) ienv -> (
                match value with
                | Loc ins_loc ->
                    (AVS.singleton ((Val.of_loc (Loc.mk_offset ins_loc index)), cst))
                    |> (fun x -> InstEnv.add loc x ienv)
                | _ ->
                    failwith "must be a location."))
              |> (fun f -> AVS.fold f ins_avs ienv)))
        |> (fun f -> LocSet.fold f offsets ienv)
      in
      match typ.desc, loc with
      | Tptr (typ', _), ConstLoc _ -> (* [*l | l ] : t*, where l is a constant location in the callee environment. *)
          let () = L.progress "PTR * ConstLoc: %a: %a\n@." Loc.pp loc (Typ.pp_full Pp.text) typ in

          mk_ienv_param_loc tenv (Loc.mk_pointer loc) typ' callee caller ienv
      | Tptr (typ', _), Pointer loc' ->
          let () = L.progress "PTR * Pointer : %a: %a\n@." Loc.pp loc (Typ.pp_full Pp.text) typ in
          let ins_avs = InstEnv.find loc' ienv in
          (fun ((value: Val.t), cst) ienv ->
            (match value with
            | Loc ins_loc ->
                InstEnv.add loc (Heap.find ins_loc caller) ienv
                |> mk_ienv_param_loc tenv (Loc.mk_pointer loc) typ' callee caller
            | _ -> failwith "value must be a location."))
          |> (fun f -> AVS.fold f ins_avs ienv)
      | Tptr (typ', _), Offset (loc', index) ->
          let () = L.progress "PTR * Offset: %a: %a\n@." Loc.pp loc (Typ.pp_full Pp.text) typ in
          failwith "struct cannot be propagated as a constant location."
      | Tstruct _, ConstLoc _ when not (JniModel.is_jni_obj_typ typ) -> (* [*l | l] : struct t *)
          let () = L.progress "Struct * ConstLoc: %a: %a\n@." Loc.pp loc (Typ.pp_full Pp.text) typ in
          failwith "struct cannot be propagated as a constant location."
      | Tstruct name, Pointer loc' when not (JniModel.is_jni_obj_typ typ) -> (* *l : struct t *)
          let () = L.progress "Struct * Pointer: %a: %a\n@." Loc.pp loc (Typ.pp_full Pp.text) typ in
          let ins_avs = InstEnv.find loc' ienv in
          let iter_avs = fun ((value: Val.t), cst) ienv ->
            (match value with
            | Loc ins_loc ->
                let ienv' = InstEnv.add loc (Heap.find ins_loc caller) ienv in
                let fld_typs = Helper.get_fld_and_typs name tenv in
                let ienv'' = (fun (fld, typ) ienv ->
                  let index = Loc.mk_index_of_string fld in
                  let ins_avs = InstEnv.find loc ienv in
                  (fun ((value: Val.t), cst) ienv ->
                    (match value with
                    | Loc ins_loc ->
                        let ins_offset = Loc.mk_offset ins_loc index in
                        let param_offset = Loc.mk_offset loc index in
                        InstEnv.add param_offset (AVS.singleton (Val.of_loc ins_offset, cst)) ienv
                    | _ -> failwith "value must be a location."))
                  |> (fun f -> AVS.fold f ins_avs ienv))
                |> (fun f -> Caml.List.fold_right f fld_typs ienv')
                in
                (fun (fld, typ) ienv ->
                  let param_offset = Loc.mk_offset loc (Loc.mk_index_of_string fld) in
                  mk_ienv_param_loc tenv (Loc.mk_pointer param_offset) typ callee caller ienv)
                |> (fun f -> Caml.List.fold_right f fld_typs ienv'')
            | _ -> 
                failwith "value must be a location.")
          in
          AVS.fold iter_avs ins_avs ienv
      | Tstruct name, Offset (loc', index) ->
          let () = L.progress "Struct * Offset: %a: %a\n@." Loc.pp loc (Typ.pp_full Pp.text) typ in
          failwith "struct cannot be propagated as a constant location."
      | Tarray _, ConstLoc _ ->
          let () = L.progress "Array * Pointer: %a: %a\n@." Loc.pp loc (Typ.pp_full Pp.text) typ in
          failwith "struct cannot be propagated as a constant location."
      | Tarray _, Pointer loc' ->
          let () = L.progress "Array * Pointer: %a: %a\n@." Loc.pp loc (Typ.pp_full Pp.text) typ in
          failwith "struct cannot be propagated as a constant location."
      | Tarray _, Offset (loc', index) ->
          let () = L.progress "Array * Offset: %a: %a\n@." Loc.pp loc (Typ.pp_full Pp.text) typ in
          failwith "struct cannot be propagated as a constant location."
      | _, ConstLoc _ -> (* l : t *)
          let () = L.progress "Other * ConstLoc: %a: %a\n@." Loc.pp loc (Typ.pp_full Pp.text) typ in
          failwith "struct cannot be propagated as a constant location."
      | _, Pointer loc' -> (* *l : t *)
          let () = L.progress "Other * Pointer: %a: %a\n@." Loc.pp loc (Typ.pp_full Pp.text) typ in
          let ins_avs = InstEnv.find loc' ienv in
          (fun ((value: Val.t), cst) ienv ->
            (match value with
            | Loc ins_loc ->
                let caller_avs = Heap.find ins_loc caller in
                (InstEnv.add loc caller_avs ienv)
                |> expand_offset loc caller_avs typ
            | _ -> failwith "value must be a location."))
          |> (fun f -> AVS.fold f ins_avs ienv)
      | _, Offset (loc', index) ->
          let () = L.progress "Other * Offset : %a: %a\n@." Loc.pp loc (Typ.pp_full Pp.text) typ in
          failwith "does not handle this case!"
      | _ -> 
          let () = L.progress "???: %a: %a\n@." Loc.pp loc (Typ.pp_full Pp.text) typ in
          failwith "does not handle this case!"

let rec mk_ienv_loc: Tenv.t -> Loc.t -> Typ.t -> Heap.t -> Heap.t -> InstEnv.t -> InstEnv.t =
    fun tenv loc typ callee_heap caller_heap ienv ->
      match typ.desc, loc with
      | Tptr (typ', _), ConstLoc _ -> (* [*l | l ] : t*, where l is a constant location in the callee environment. *)
          let () = L.progress "PTR * ConstLoc: %a: %a\n@." Loc.pp loc (Typ.pp_full Pp.text) typ in
          mk_ienv_loc tenv (Loc.mk_pointer loc) typ' callee_heap caller_heap ienv
      | Tptr (typ', _), Pointer loc' ->
          let ins_avs = InstEnv.find loc' ienv in
          let iter_avs = fun ((value: Val.t), cst) ienv ->
            (match value with
            | Loc ins_loc ->
                InstEnv.add loc (Heap.find ins_loc caller_heap) ienv
                |> mk_ienv_loc tenv (Loc.mk_pointer loc) typ' callee_heap caller_heap
            | _ -> failwith "value must be a location.")
          in
          AVS.fold iter_avs ins_avs ienv
      | Tstruct _, ConstLoc _ when not (JniModel.is_jni_obj_typ typ) -> (* [*l | l] : struct t *)
          let () = L.progress "Struct * ConstLoc: %a: %a\n@." Loc.pp loc (Typ.pp_full Pp.text) typ in
          failwith "struct cannot be propagated as a constant location."
      | Tstruct name, Pointer loc' when not (JniModel.is_jni_obj_typ typ) -> (* *l : struct t *)
          let () = L.progress "!!!!!Struct * Pointer: %a: %a\n@." Loc.pp loc (Typ.pp_full Pp.text) typ in
          failwith "struct cannot be propagated as a constant location."
      | Tarray _, ConstLoc _ ->
          let () = L.progress "Struct * Pointer: %a: %a\n@." Loc.pp loc (Typ.pp_full Pp.text) typ in
          failwith "struct cannot be propagated as a constant location."
      | Tarray _, Pointer loc' ->
          let () = L.progress "Struct * Pointer: %a: %a\n@." Loc.pp loc (Typ.pp_full Pp.text) typ in
          failwith "struct cannot be propagated as a constant location."
      | _, ConstLoc _ -> (* l : t *)
          let () = L.progress "Other * ConstLoc: %a: %a\n@." Loc.pp loc (Typ.pp_full Pp.text) typ in
          failwith "struct cannot be propagated as a constant location."
      | _, Pointer loc' -> (* *l : t *)
          let () = L.progress "Other * Pointer: %a: %a\n@." Loc.pp loc (Typ.pp_full Pp.text) typ in
          let ins_avs = InstEnv.find loc' ienv in
          (fun ((value: Val.t), cst) ienv ->
            (match value with
            | Loc ins_loc ->
                InstEnv.add loc (Heap.find ins_loc caller_heap) ienv
            | _ -> failwith "value must be a location."))
          |> (fun f -> AVS.fold f ins_avs ienv)
      | _ -> 
          let () = L.progress "???: %a: %a\n@." Loc.pp loc (Typ.pp_full Pp.text) typ in
          failwith "does not handle this case!"

let mk_ienv_glob tenv glob_locs glob_heap caller_heap =
  let f_glob = fun ienv (loc, _, typ) -> 
    let avsfied = AVS.singleton (Val.of_loc loc, Cst.cst_true) in
    let ienv' = InstEnv.add loc avsfied ienv in
    mk_ienv_loc tenv (Loc.mk_pointer loc) typ glob_heap caller_heap ienv'
  in
  Caml.List.fold_left f_glob InstEnv.empty glob_locs  

(* construct an instantiation environment at call sites. *)
let mk_ienv tenv pdesc args callee_env callee_heap caller_heap glob_heap = 
  let params = 
    fun_params pdesc |>
    Caml.List.map (fun (param, typ) -> (Loc.mk_const param), Typ.mk (Tptr (typ, Pk_pointer)))
  in
  let heap' =
    (fun heap (param, typ) arg -> Heap.add param arg heap)
    |> (fun f -> Caml.List.fold_left2 f caller_heap params args)
  in
  (fun ienv (loc, typ) ->
    let ienv' =
      AVS.singleton (Val.of_loc loc, Cst.cst_true)
      |> (fun x -> InstEnv.add loc x ienv)
    in
    mk_ienv_param_loc tenv loc typ callee_heap heap' ienv')
  |> (fun f -> Caml.List.fold_left f InstEnv.empty params)

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
        let iloc1_avs = InstEnv.find loc1 ienv in
        let iloc2_avs = InstEnv.find loc2 ienv in
        let iter_iloc1 = fun (iloc1, cst1) cst ->
          let iter_iloc2 = fun (iloc2, cst2) cst ->
            Cst.cst_and (Cst.cst_and (Cst.cst_eq (Val.to_loc iloc1) (Val.to_loc iloc2)) (Cst.cst_and cst1 cst2)) cst
          in
          AVS.fold iter_iloc2 iloc2_avs cst
        in
        AVS.fold iter_iloc1 iloc1_avs Cst.cst_true
    | _ as cst ->
        cst

(* instantiate an Abstract Value Set *)
let inst_avs avs ienv = 
  let inst_val = fun ((value: Val.t), cst) avs -> 
    let cst' = inst_cst cst ienv in
    match value with
    | Loc loc -> (
      match InstEnv.find_opt loc ienv with
      | Some s -> 
          Helper.((s ^ cst') + avs)
      | None ->
          Helper.((AVS.singleton (value, cst')) + avs))
    |_ -> 
      Helper.((AVS.singleton (value, cst')) + avs)
  in
  AVS.fold inst_val avs AVS.empty

let rec expand_ienv_for_offset ienv caller callee =
  (*
  let locset = flatten_heap_locs callee in
  let rec expand = (fun loc ins_avs ienv ->

  in 
  InstEnv.fold expand ienv ienv
  *)
  ienv

(* compose caller and callee heaps at call instructions. *)
let comp_heap base caller callee ienv = 
  let ienv = expand_ienv_for_offset ienv caller callee in
  let () = L.progress "IENV: %a\n@." InstEnv.pp ienv in
  let () = L.progress "OldHeap: %a\n@." Heap.pp callee in
  let f = fun (loc: Loc.t) avs base -> 
    match loc with
    | ConstLoc _ ->
        Heap.add loc (inst_avs avs ienv) base 
    | _ ->
        let ival_avs = inst_avs avs ienv in
        let update_avs = fun ((value: Val.t), cst) base -> (
          match value with
          | Loc loc' ->
              let pre_avs = Heap.find loc' caller in
              let merged_avs = Helper.((ival_avs ^ cst) 
                + (pre_avs ^ (Cst.cst_not cst))) in
              let () = L.progress "%a - %a: %a -> %a\n@." Loc.pp loc Loc.pp loc' AVS.pp pre_avs AVS.pp merged_avs in
              Heap.weak_update loc' merged_avs base
          | _ -> 
              Heap.weak_update loc (AVS.singleton (value, cst)) base)
        in
        let iloc_avs = InstEnv.find loc ienv in
        AVS.fold update_avs iloc_avs base
  in
  let new_heap = Heap.fold f callee Heap.empty in
  let iter_new_heap = fun loc avs base ->
    Heap.add loc avs base
  in
  let () = L.progress "NewHeap: %a\n@." Heap.pp new_heap in
  Heap.fold iter_new_heap new_heap base

(* compose caller and callee logs at call instructions. *)
let comp_log base callee_logs caller_heap ienv = 
  let f = fun log base ->
    let heap' = 
      comp_heap Heap.empty caller_heap (LogUnit.get_heap log) ienv 
    in
    CallLogs.add (LogUnit.update_heap heap' log) base
  in
  CallLogs.fold f callee_logs base 
