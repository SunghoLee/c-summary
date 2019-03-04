open! IStd
open Core
module F = Format
module L = Logging
module Domain = SemanticSummaryDomain
open Domain
open Pervasives

let get_struct typ tenv =
  match Typ.name typ with
  | Some s -> 
    (match Tenv.lookup tenv s with
    | Some s -> 
        s
    | None -> 
        failwith ("The structure cannot be found in a type environment: " ^ (Typ.to_string typ)))
  | _ -> failwith ("this typ is not a struct type: " ^ (Typ.to_string typ))

let find_field_typ: String.t -> Typ.Struct.fields -> Typ.t = 
  fun name fields ->
    let rec iter_field : (Typ.Fieldname.t * Typ.t * Annot.Item.t) list -> Typ.t = 
      function
        h :: t ->
          (match h with
          | (fn, typ, _) when (Typ.Fieldname.to_string fn) = name -> typ
          | _ -> iter_field t)
        | [] -> 
            failwith "The field does not exist."
    in
    iter_field fields

(* {(v1, cst1) ... (vn, cstn)} ^ (cst) = {(v1, cst1 & cst) ... (vn, cstn & cst)} *)
let ( ^ ) avs cst =
  AVS.map (fun (value, cst') -> (value, Cst.cst_and cst' cst)) avs


(* {(v11, cst11) ... (v1n, cst1n)} + {(v21, cst21) ... (v2m, cst2m)}  = 
  {(v11, cst11) ... (v1n, cst1n), (v21, cst21) ... (v2m, cst2m)}*)
let ( + ) lhs rhs = AVS.union lhs rhs

(* x = *y  *)
let load avs heap =
  let f = fun (value, cst) avs ->
    let loc = Val.to_loc value in
    let avs' = Heap.find loc heap in
    let avs'' = avs' ^ cst in
    avs'' + avs
  in
  AVS.fold f avs AVS.empty

(* *x = y *)
let store lhs rhs heap =
  let f = fun (value, cst) heap ->
    let loc = Val.to_loc value in
    let pre_avs = 
      match Heap.find_opt loc heap with
      | Some v -> 
          v
      | None -> (* first assignment cases: no exising pre-assigned value *)
          AVS.empty 
    in
    let pre_avs' = pre_avs ^ (Cst.cst_not cst) in
    let new_avs = rhs ^ cst in
    let merged_avs = pre_avs' + new_avs in
    Heap.add loc merged_avs heap 
  in
  AVS.fold f lhs heap

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
let rec mk_ienv_loc tenv loc typ callee_heap caller_heap ienv =
  (if Typ.is_pointer typ then (* [*l | l ] : t*, where l is a constant location in the callee environment. *)
    if Loc.is_const loc then (* l : t* *)
      mk_ienv_loc tenv (Loc.mk_pointer loc) (Typ.strip_ptr typ) callee_heap caller_heap ienv
    else (* *l : t* *)
      let unwrapped = Loc.unwrap_ptr loc in
      let ins_avs = InstEnv.find unwrapped ienv in
      let iter_avs = fun (value, cst) ienv ->
        let ins_loc = Val.to_loc value in
        let ienv' = InstEnv.add loc (Heap.find ins_loc caller_heap) ienv in
        mk_ienv_loc tenv (Loc.mk_pointer loc) (Typ.strip_ptr typ) callee_heap caller_heap ienv'
      in
      AVS.fold iter_avs ins_avs ienv
  else if Typ.is_struct typ && not (JniModel.is_jni_obj_typ typ) then (* [*l | l] : struct t *)
    if Loc.is_const loc then (* l : struct t *)
      failwith "struct cannot be propagated as a constant location."
    else (* *l : struct t *)
      let module FieldTypMap = Caml.Map.Make(String) in
      let loc = Loc.unwrap_ptr loc in
      let ins_avs = InstEnv.find loc ienv in
      let iter_avs = fun (value, cst) ienv ->
        let ins_loc = Val.to_loc value in
        let arg_avs = Heap.find ins_loc caller_heap in
        let param_avs = Heap.find loc callee_heap in
        let param_val, _ = AVS.min_elt param_avs in (* param_avs have only one element *)
        let param_str = Val.to_struct param_val in
        let str_typ = get_struct typ tenv in
        let fields = str_typ.Typ.Struct.fields in
        let iter_field = fun name _ typ_map -> 
          let f_typ = find_field_typ name fields in
          FieldTypMap.add name f_typ typ_map
        in
        let typ_map = Struct.fold iter_field param_str FieldTypMap.empty in
        let f_field = fun field f_loc ienv ->
          let f_arg = fun (arg_v, cst) avs ->
            let arg_str = Val.to_struct arg_v in
            let f_arg_loc = Struct.find field arg_str in
            let f_arg_avs = Heap.find f_arg_loc caller_heap in
            (f_arg_avs ^ cst) + avs
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
      AVS.fold iter_avs ins_avs ienv
  else (* [*l | l ]: t  *)
    if Loc.is_const loc then (* l : t *)
      ienv
    else (* *l : t *)
      let unwrapped = Loc.unwrap_ptr loc in
      let ins_avs = InstEnv.find unwrapped ienv in
      let iter_avs = fun (value, cst) ienv ->
        let ins_loc = Val.to_loc value in
        InstEnv.add loc (Heap.find ins_loc caller_heap) ienv 
      in
      AVS.fold iter_avs ins_avs ienv)

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
          (s ^ cst') + avs
      | None ->
          (AVS.singleton (value, cst')) + avs
    else 
      (AVS.singleton (value, cst')) + avs
  in
  AVS.fold inst_val avs AVS.empty

(* compose caller and callee heaps at call instructions. *)
let comp_heap base caller callee ienv = 
  let f = fun loc avs base -> 
    if Loc.is_const loc || Loc.is_ret loc then 
      Heap.add loc (inst_avs avs ienv) base 
    else (* *l *)
      let iloc_avs = InstEnv.find loc ienv in
      let ival_avs = inst_avs avs ienv in
      let update_avs = fun (value, cst) base ->
        let iloc = Val.to_loc value in
        let pre_avs = Heap.find iloc caller in
        let merged_avs = (ival_avs ^ cst) 
          + (pre_avs ^ (Cst.cst_not cst)) in
        Heap.weak_update iloc merged_avs base
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
