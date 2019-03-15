open! IStd
open Core
module F = Format
module L = Logging
module Helper = HelperFunction
open SemanticSummaryDomain
open Pervasives

(* return variables defined as parameters *)
let fun_params pdesc =
    let attrs = Procdesc.get_attributes pdesc in
    let args = attrs.formals in
    Caml.List.map (fun (m, typ) -> ((Var.of_string (Mangled.to_string m)), typ)) args

(* return a location mapped by the location `loc' in the instantiation environment. 
 * because each mapping of the ienv is one-to-one, there is one and only one location mapped by `loc'. *)
let loc_from_ienv loc ienv =
  let avs = InstEnv.find loc ienv in
  if (AVS.cardinal avs = 1) then
    let (value, cst) = AVS.min_elt avs in
    Val.to_loc value
  else
    failwith "The location does not have an one-to-one mapping in the instantiation environment."

(* update an instantiation environment for a parameter location *)
let rec mk_ienv_loc: Tenv.t -> Loc.t -> Typ.t -> AVS.t Heap.t -> AVS.t Heap.t -> AVS.t InstEnv.t -> AVS.t InstEnv.t =
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
          let () = L.progress "Struct * Pointer: %a: %a\n@." Loc.pp loc (Typ.pp_full Pp.text) typ in
          let module FieldTypMap = Caml.Map.Make(String) in
          let ins_avs = InstEnv.find loc' ienv in
          let () = L.progress "Loc: %a, Typ: %a, INS: %a\n@." Loc.pp loc (Typ.pp_full Pp.text) typ AVS.pp ins_avs in
          let cur_ins_avs = Helper.indirect_load ins_avs caller_heap in
          let ienv' = InstEnv.add loc cur_ins_avs ienv in 
          let param_str = Heap.find loc callee_heap
            |> AVS.min_elt  (* param_str_avs has only one element *)
            |> (fun (value, _) -> Val.to_struct value)
          in
          let fields = Helper.get_struct typ tenv |> (fun x -> x.Typ.Struct.fields) in
          let typ_map = 
            (fun name _ typ_map -> 
              Helper.find_field_typ name fields 
              |> (fun x -> FieldTypMap.add name x typ_map))
            |> (fun x -> Struct.fold x param_str FieldTypMap.empty)
          in
          let rec iter_ptr_avs = fun ((value_ptr: Val.t), cst_ptr) ienv ->
            (match value_ptr with
            | Loc l ->
                let iter_avs = fun (value, cst) ienv ->
                  let arg_avs = Heap.find l caller_heap in
                  let f_field = fun field f_loc ienv ->
                    let f_arg = fun (arg_v, cst) avs ->
                      let arg_str = Val.to_struct arg_v in
                      let () = L.progress "Finding %s in %a\n@." field Struct.pp arg_str in
                      let f_arg_loc = Struct.find field arg_str in
                      let f_arg_avs = Heap.find f_arg_loc caller_heap in
                      Helper.(((f_arg_avs ^ cst) ^ cst_ptr) + avs)
                    in
                    let f_arg_avs = AVS.fold f_arg arg_avs AVS.empty in
                    let caller_heap' = Heap.add f_loc f_arg_avs caller_heap in
                    let f_loc_avs = AVS.singleton (Val.of_loc f_loc, Cst.cst_true) in
                    let ienv' = InstEnv.add f_loc f_loc_avs ienv in
                    let f_typ = FieldTypMap.find field typ_map in
                    mk_ienv_loc tenv (Loc.mk_pointer f_loc) f_typ callee_heap caller_heap' ienv'
                  in
                  Struct.fold f_field param_str ienv
                in
                let ptr = Val.to_loc value_ptr in
                let avs = Heap.find ptr caller_heap in
                AVS.fold iter_avs avs ienv
            | Struct str -> (* array *)
                let fields = Helper.get_struct_fields str in
                let handle_field = fun ienv field ->
                  let field_loc = Struct.find field str in
                  ((Val.of_loc field_loc)
                    |> (fun x -> x, cst_ptr)
                    |> AVS.singleton (*TODO: need to week update? *)
                    |> (fun x -> InstEnv.add (Loc.mk_offset loc (int_of_string field)) x ienv))
                  |> ((Heap.find field_loc caller_heap) (* TODO: handle each index seperately?? *)
                    |> (fun x y -> AVS.fold (fun value ienv -> iter_ptr_avs value ienv) x y))
                in
                let ienv'' = Caml.List.fold_left handle_field ienv' @@ Helper.get_struct_fields str in
                ienv''
            | _ -> 
                failwith ("must be a loc: " ^ (F.asprintf "%a" Val.pp value_ptr))
                )
          in
          AVS.fold iter_ptr_avs cur_ins_avs ienv'
      | _, ConstLoc _ -> (* l : t *)
          let () = L.progress "Other * ConstLoc: %a: %a\n@." Loc.pp loc (Typ.pp_full Pp.text) typ in
          ienv
      | _, Pointer loc' -> (* *l : t *)
          let () = L.progress "Other * Pointer: %a: %a\n@." Loc.pp loc (Typ.pp_full Pp.text) typ in
          let ins_avs = InstEnv.find loc' ienv in
          let iter_avs = fun ((value: Val.t), cst) ienv ->
            (match value with
            | Loc l -> 
                let arg_avs = Heap.find l caller_heap in
                let handle_arr = fun ((arg_val: Val.t), arg_cst) ienv ->
                  (match arg_val with
                  | Struct str ->
                      let fields = Helper.get_struct_fields str in
                      let handle_field = fun ienv field ->
                        Struct.find field str
                        |> Val.of_loc 
                        |> (fun x -> x, (Cst.cst_and cst arg_cst))
                        |> AVS.singleton (*TODO: need to week update? *)
                        |> (fun x -> InstEnv.add (Loc.mk_offset loc (int_of_string field)) x ienv)
                      in
                      Caml.List.fold_left handle_field ienv @@ Helper.get_struct_fields str
                  | _ -> 
                      ienv)
                in
                InstEnv.add loc arg_avs ienv
                |> AVS.fold handle_arr arg_avs
            | _ ->
                failwith "value must be a location.")
          in
          AVS.fold iter_avs ins_avs ienv

let mk_ienv_glob tenv glob_locs glob_heap caller_heap =
  let f_glob = fun ienv (loc, _, typ) -> 
    let avsfied = AVS.singleton (Val.of_loc loc, Cst.cst_true) in
    let ienv' = InstEnv.add loc avsfied ienv in
    mk_ienv_loc tenv (Loc.mk_pointer loc) typ glob_heap caller_heap ienv'
  in
  Caml.List.fold_left f_glob InstEnv.empty glob_locs  

(* construct an instantiation environment at call sites. *)
let mk_ienv tenv pdesc args callee_env callee_heap caller_heap glob_locs glob_heap = 
  let params = 
    fun_params pdesc |>
    Caml.List.map (fun (param, typ) -> (Env.find param callee_env), typ)
  in
  let f_args = fun heap (param, typ) arg ->
   Heap.add param arg heap
  in
  let heap' = Caml.List.fold_left2 f_args caller_heap params args in
  let f_param = fun ienv (loc, typ) -> 
    let avsfied = AVS.singleton (Val.of_loc loc, Cst.cst_true) in
    let ienv' = InstEnv.add loc avsfied ienv in
    mk_ienv_loc tenv (Loc.mk_pointer loc) typ callee_heap heap' ienv'
  in
  let glob_ienv = mk_ienv_glob tenv glob_locs glob_heap caller_heap in
  Caml.List.fold_left f_param glob_ienv params

(* instantiate a constraint *)
let rec inst_cst : Cst.t -> AVS.t InstEnv.t -> Cst.t =
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
  let inst_val = fun (value, cst) avs -> 
    let cst' = inst_cst cst ienv in
    if Val.is_loc value then
      match InstEnv.find_opt (Val.to_loc value) ienv with
      | Some s -> 
          Helper.((s ^ cst') + avs)
      | None ->
          Helper.((AVS.singleton (value, cst')) + avs)
    else 
      Helper.((AVS.singleton (value, cst')) + avs)
  in
  AVS.fold inst_val avs AVS.empty

(* compose caller and callee heaps at call instructions. *)
let comp_heap base caller callee ienv = 
  let f = fun loc avs base -> 
    if Loc.is_const loc || Loc.is_ret loc then 
      Heap.add loc (inst_avs avs ienv) base 
    else (* *l *)
      let () = L.progress "Finding %a\n@." Loc.pp loc in
      let iloc_avs = InstEnv.find loc ienv in
      let ival_avs = inst_avs avs ienv in
      let update_avs = fun (value, cst) base ->
        if Val.is_loc value then
          let iloc = Val.to_loc value in
          let pre_avs = Heap.find iloc caller in
          let merged_avs = Helper.((ival_avs ^ cst) 
            + (pre_avs ^ (Cst.cst_not cst))) in
          Heap.weak_update iloc merged_avs base
        else (* *ptr = NULL *)
          Heap.weak_update loc (AVS.singleton (value, cst)) base
      in
      AVS.fold update_avs iloc_avs base
  in
  let new_heap = Heap.fold f callee Heap.empty in
  let iter_new_heap = fun loc avs base ->
    Heap.add loc avs base
  in
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
