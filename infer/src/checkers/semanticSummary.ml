(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open Core
module F = Format
module L = Logging
module Domain = SemanticSummaryDomain
open Domain

let var_index = ref 0

let procs = ref PreForGlobal.Procs.empty

module SMTSolver : sig
    val is_sat : Cst.t -> bool * Cst.t
end = struct
    include Z3

    let debug = false 

    let print cst expr sim_expr sim_cst result = 
        if debug then
           L.progress "\tEXPR: %s \n\t -> %s \n\t -> %s \n\t -> %s \n\t -> %s\n@." (Cst.pp cst) (Expr.to_string expr) (Expr.to_string sim_expr) (Cst.pp sim_cst) result

    let ctx = 
        let cfg = [("model", "true")] in
        mk_context cfg

    let is_sat cst = 
        let expr = Cst.encode ctx cst in
        let sim_expr = Expr.simplify expr None in
        let sim_cst = Cst.decode sim_expr in
        let solver = Solver.mk_solver ctx None in
        let res = Solver.check solver [sim_expr] in
        match res with
        | UNSATISFIABLE -> print cst expr sim_expr sim_cst "UNSAT"; (false, sim_cst)
        | UNKNOWN -> print cst expr sim_expr sim_cst "UNKNOWN"; (true, sim_cst)
        | _ -> print cst expr sim_expr sim_cst "SAT"; (true, sim_cst)
end 

module LocationHandler = struct
    let create_new_constloc () = var_index := !var_index + 1; AbsLoc.ct_const(!var_index)
    let create_new_symbol () = var_index := !var_index + 1; AbsLoc.ct_symbol(!var_index)
end

module I2D : sig
    val get_var_from_id : Ident.t -> Var.t
    val get_var_from_pvar : Pvar.t -> Var.t
end = struct
    let get_var_from_id id = Var.create ((Ident.name_to_string (Ident.get_name id)) ^ "$" ^ (Pervasives.string_of_int (Ident.get_stamp id)))

    let get_var_from_pvar pvar = 
        let name = 
            if Pvar.is_global pvar then
                let tu = Pvar.get_translation_unit pvar in
                match tu with
                | Some s -> "#GB_" ^ (SourceFile.to_string s) ^ "_" ^ (Pvar.to_string pvar)
                | None -> Pvar.to_string pvar
            else if Pvar.is_local pvar then
                Mangled.to_string (Pvar.get_name pvar)
            else failwith "no other variable types in C"
        in
        Var.create name 
end

let get_inst_type (i: Sil.instr) = 
    match i with
    | Load _ -> "Load"
    | Store _ -> "Store"
    | Prune _ -> "Prune"
    | Call _ -> "Call"
    | Nullify _ -> "Nullify"
    | Abstract _ -> "Abstract"
    | ExitScope _ -> "ExitScope"

let is_struct = Typ.is_cpp_class

let pp_inst node i = L.progress "%a: [%s] %a\n@." Procdesc.Node.pp_id (Procdesc.Node.get_id node) (get_inst_type i) (Sil.pp_instr ~print_types:true Pp.text) i 
let pp_domain s d = L.progress "%s %a\n@." s Domain.pp d
let pp_fun attr = L.progress "%a\n@." ProcAttributes.pp attr
let is_jni_fun f = 
    let str_name = Typ.Procname.to_string f in
    String.is_prefix str_name "JNIEnv__" || String.is_prefix str_name "JavaVM"

module TransferFunctions (CFG : ProcCfg.S) = struct
    module CFG = CFG
    module Domain = Domain
    module RetAddrMap = Caml.Map.Make(String)
    module InstEnv = struct
    include Caml.Map.Make(AbsLoc)
    let find x m =
        match find_opt x m with
        | None -> AbstractValueSet.singleton (AbsValWCst.create (AbsVal.loc_to_val x) Cst.cst_t)
        | Some s -> s
  end

    let pp_inst_env env = 
        InstEnv.fold (fun x y i -> i ^ (AbsLoc.pp x) ^ " -> " ^ (AbstractValueSet.pp y) ^ "\n") env ""

    module Helper : sig
        val get : AbstractValueSet.t -> Heap.t -> AbstractValueSet.t
        val store : AbstractValueSet.t -> AbstractValueSet.t -> Heap.t -> Heap.t
        val ( + ) : AbstractValueSet.t -> AbstractValueSet.t -> AbstractValueSet.t
        val ( ^ ) : AbstractValueSet.t -> Cst.t -> AbstractValueSet.t
        val map_args : Procdesc.t -> AbstractValueSet.t list -> Env.t -> Env.t -> Heap.t -> Heap.t -> AbstractValueSet.t InstEnv.t
        val compose_heap : Heap.t -> Heap.t -> Heap.t -> AbstractValueSet.t InstEnv.t -> Heap.t 
        val compose_log : CallLogs.t -> CallLogs.t -> Heap.t -> AbstractValueSet.t InstEnv.t -> CallLogs.t
    end = struct
        let ( ^ ) (v: AbstractValueSet.t) (cst: Cst.t) =
            if AbstractValueSet.is_top v then
                AbstractValueSet.top
            else
                let f (abs_val, cst') i = AbstractValueSet.add (abs_val, Cst.cst_and cst' cst) i in
                AbstractValueSet.fold f v AbstractValueSet.empty

        let ( + ) (lhs: AbstractValueSet.t) (rhs: AbstractValueSet.t) = AbstractValueSet.union lhs rhs

        let get (v: AbstractValueSet.t) (heap: Heap.t) =
            let f (abs_val, cst) i =
                let abs_loc = AbsVal.val_to_loc abs_val in
                let abs_val_set = Heap.find abs_loc heap in
                let nabs_val_set = abs_val_set ^ cst in
                nabs_val_set + i
            in
            AbstractValueSet.fold f v AbstractValueSet.empty

        let store (lhs: AbstractValueSet.t) (rhs: AbstractValueSet.t) (heap: Heap.t) =
            let f1 (abs_val, cst) i =
                let abs_loc = AbsVal.val_to_loc abs_val in
                let pre_abs_val_set = 
                    match Heap.find_opt abs_loc heap with
                    | None -> AbstractValueSet.empty (* first assignment cases *)
                    | Some v -> v
                in
                let npre_abs_val_set = pre_abs_val_set ^ (Cst.cst_not cst) in
                let new_abs_val_set = rhs ^ cst in
                let abs_val_set = npre_abs_val_set + new_abs_val_set in
                Heap.add abs_loc abs_val_set i
            in
            AbstractValueSet.fold f1 lhs heap

        let get_singleton_loc loc heap =
            let v = Heap.find loc heap in
            if phys_equal (AbstractValueSet.cardinal v) 1 then
                match (List.nth (AbstractValueSet.elements v) 0) with
                | None -> failwith String.("only one location can be allocated per each parameter: " ^ (AbsLoc.pp loc))
                | Some s -> AbsVal.val_to_loc (AbsValWCst.get_val s)
            else 
                failwith (String.("only one location can be allocated per each parameter: " ^ (AbsLoc.pp loc)))

        let rec map_loc heap callee_heap ienv preloc loc typ =
            if not (Typ.is_pointer typ) then 
                if AbsLoc.is_const loc then ienv (* not a pointer, but a constant location *)
                else (* not a pointer, but a symbolic location *) 
                    match preloc with
                    | None -> failwith "Preloc does not exist only when loc is constloc"
                    | Some pl -> 
                        let v = get (InstEnv.find pl ienv) heap in
                        InstEnv.add loc v ienv
            else (* a pointer and const location *)
                if AbsLoc.is_const loc then 
                    map_loc heap callee_heap ienv (Some loc) (get_singleton_loc loc callee_heap) (Typ.strip_ptr typ)
                else (* a pointer and symbolic location *)
                    match preloc with
                    | None -> failwith "Preloc does not exist only when loc is constloc"
                    | Some pl -> 
                        let ienv' = InstEnv.add loc (get (InstEnv.find pl ienv) heap) ienv in
                        map_loc heap callee_heap ienv' (Some loc) (AbsLoc.ct_pointer loc) (Typ.strip_ptr typ)

        let get_params pdesc =
            let attrs = Procdesc.get_attributes pdesc in
            let args = attrs.formals in
            let f = function
                (m, typ) -> ((Var.create (Mangled.to_string m)), typ)
            in
            Caml.List.map f args

        let map_args pdesc args callee_env env callee_heap heap = 
            let params = get_params pdesc in
            let f h (param, typ) arg_v = 
                let param_loc = Env.find param callee_env in
                Heap.add param_loc arg_v h
            in
            let heap' = Caml.List.fold_left2 f heap params args in
            let f ienv (param, typ) =
                let param_loc = Env.find param callee_env in
                let abs_val = AbstractValueSet.singleton (AbsValWCst.create (AbsVal.loc_to_val param_loc) Cst.cst_t) in
                let ienv' = InstEnv.add param_loc abs_val ienv in
                map_loc heap' callee_heap ienv' None param_loc typ
            in
            Caml.List.fold_left f InstEnv.empty params
        
        let inst_val vs ienv =
            let f (v, cst) i = 
                if AbsVal.is_loc v then
                    let loc = AbsVal.val_to_loc v in
                    let iv = InstEnv.find loc ienv in
                    i + (iv ^ cst)
                else
                    i + (AbstractValueSet.singleton (AbsValWCst.create v cst))
            in
            AbstractValueSet.fold f vs AbstractValueSet.empty

        let compose_partial_heap base_heap cur_heap ienv from_loc to_v = 
            let v1 = InstEnv.find from_loc ienv in
            let v2 = inst_val to_v ienv in
            let f (v, cst) h = 
                let loc = AbsVal.val_to_loc v in
                let t_v = v2 ^ cst in
                let f_v = (Heap.find loc cur_heap) ^ (Cst.cst_not cst) in
                let newv = t_v + f_v in
                Heap.add loc newv h
            in
            AbstractValueSet.fold f v1 base_heap

        let compose_heap base_heap target_heap cur_heap ienv = 
            let f loc v h = 
                if AbsLoc.is_const loc then Heap.add loc v h (* anyway, merge? *)
                else compose_partial_heap h cur_heap ienv loc v in
            Heap.fold f target_heap base_heap

        let compose_log base_logs target_logs cur_heap ienv = 
            let f log base = 
                let heap' = compose_heap Heap.empty (LogUnit.get_heap log) cur_heap ienv in
                CallLogs.add {log with LogUnit.heap = heap'} base
            in
            CallLogs.fold f target_logs base_logs
    end

    let ret_addr_map = ref RetAddrMap.empty

    type extras = ProcData.no_extras

    let get_method_name (pdata: extras ProcData.t) = Typ.Procname.to_string (Procdesc.get_proc_name pdata.pdesc)

    let get_field_name fn = Typ.Fieldname.to_string fn

    let rec exec_expr env heap (expr : Exp.t) : AbstractValueSet.t = 
        match expr with
        | Var i -> 
            let var = I2D.get_var_from_id i in
            let addr = Env.find var env in
            let res = Heap.find addr heap in
            L.progress "[Exp]Var: %a -> %s\n@." Exp.pp expr (AbstractValueSet.pp res); res
        | UnOp (op, e, typ) ->
            L.progress "[Exp]Unop: %a -> T\n@." Exp.pp expr;
            (* do not handle unary operations *)
            (* TODO: propagate AbstractValueTop for Integer type *)
            AbstractValueSet.singleton ((AbsVal.int_to_val Int.top), Cst.cst_t)
        | BinOp (op, e1, e2) -> 
            L.progress "[Exp]Binop: %a -> T\n@." Exp.pp expr;
            (* do not handle binary operations *)
            (* TODO: propagate AbstractValueTop for Integer type *)
            AbstractValueSet.singleton ((AbsVal.int_to_val Int.top), Cst.cst_t)
            (*AbstractValueSet.top*)
        | Exn typ ->
            L.progress "[Exp]Exn: %a -> T\n@." Exp.pp expr;
            (* do not handle exceptions *)
            AbstractValueSet.top
        | Closure f -> 
            L.progress "[Exp]Closure: %a -> err!\n@." Exp.pp expr;
            failwith "C does not support anonymous functions"
        | Const c -> 
            L.progress "[Exp]Const: %a\n@." Exp.pp expr;
            (* only handle string constants *)
            (match c with
            | Cint s -> 
                let abs_val = AbsVal.int_to_val (Int.create (IntLit.to_int_exn s)) in
                AbstractValueSet.singleton (AbsValWCst.create abs_val Cst.cst_t)
            | Cfun _ ->
                AbstractValueSet.top
            | Cstr s -> 
                let abs_val = AbsVal.str_to_val (Str.create s) in
                AbstractValueSet.singleton (AbsValWCst.create abs_val Cst.cst_t)
            | Cfloat _ -> AbstractValueSet.top
            | Cclass _ -> AbstractValueSet.top)
        | Cast (typ, e) ->
            L.progress "[Exp]Cast: %a\n@." Exp.pp expr;
            (* do not handle type casts *)
            AbstractValueSet.top
        | Lvar pvar -> (* Location of a variable *)
            let var = I2D.get_var_from_pvar pvar in
            let addr = Env.find var env in
            let abs_val = AbsVal.loc_to_val addr in
            let res = AbstractValueSet.singleton (AbsValWCst.create abs_val Cst.cst_t) in
            L.progress "[Exp]Lvar: %a -> %s\n@." Exp.pp expr (AbstractValueSet.pp res); res
        | Lfield (e, fn, typ) -> (* Location of a field *)
            L.progress "[Exp]Lfield: (base : %a, field: %s)\n@." Exp.pp e (Typ.Fieldname.to_string fn);
            let base_locv = exec_expr env heap e in
            let f (bv, bcst) i = 
                let loc = AbsVal.val_to_loc bv in
                let base = Heap.find loc heap in
                let ff (sv, scst) i =
                    let str = AbsVal.val_to_struct sv in
                    let floc = Struct.find fn str in
                    let fval = AbsVal.loc_to_val floc in
                    let fabs = AbstractValueSet.singleton (fval, scst) in
                    Helper.(i + Helper.(fabs ^ bcst))
                in
                AbstractValueSet.fold ff base i
            in
            AbstractValueSet.fold f base_locv AbstractValueSet.empty
        | Lindex (e1, e2) -> 
            let arr_loc_set = exec_expr env heap e1 in
            let arr_set = AbstractValueSet.fold (fun (av_arr_loc, cst_arr_loc) abs -> 
                let arr_loc = AbsVal.val_to_loc av_arr_loc in
                let av_arr = Heap.find arr_loc heap in
                Helper.(abs + Helper.(av_arr ^ cst_arr_loc))) arr_loc_set AbstractValueSet.empty
            in
            let index_set = exec_expr env heap e2 in
            L.progress "[Exp]Lindex: %a\n@." Exp.pp expr;
            if (AbstractValueSet.is_top arr_set || AbstractValueSet.is_top index_set) then
                AbstractValueSet.top
            else
                let fold_arr (av_arr, cst_arr) abs = 
                    let fold_index (av_index, cst_index) abs = 
                        let int_index = AbsVal.val_to_int av_index in
                        if Int.is_top int_index then 
                            (* TODO: join all the elements of the array *)
                            abs
                        else
                            let array_arr = AbsVal.val_to_array av_arr in
                            let addr_elem = Array.nth array_arr (Int.unwrap int_index) in
                            let cst_elem = Cst.cst_and cst_arr cst_index in
                            let av_elem = AbsVal.loc_to_val addr_elem in
                            AbstractValueSet.add (av_elem, cst_elem) abs
                    in
                    AbstractValueSet.fold fold_index index_set abs
                in
                AbstractValueSet.fold fold_arr arr_set AbstractValueSet.empty
        | Sizeof data -> 
            L.progress "[Exp]Sizeof: %a\n@." Exp.pp expr;
            (* TODO: propagate AbstractValueTop for Integer type *)
            AbstractValueSet.singleton ((AbsVal.int_to_val Int.top), Cst.cst_t)
            (*AbstractValueSet.top*)

    let is_ret (expr: Exp.t) = 
        match expr with
        | Lvar pvar ->
            let var = I2D.get_var_from_pvar pvar in
            Var.is_ret var
        | _ -> false

    let get_fun_desc name =
        PreForGlobal.Procs.find_opt name !procs

    let get_fun_name (expr: Exp.t) =
        match expr with
        | Const c -> 
            (match c with
            | Cfun s -> s
            | _ -> failwith "Not possible in C")
        | _ -> failwith "Not possible in C"

    let exec_instr astate proc_data node (instr: Sil.instr) =
        pp_domain "PRE:" astate;
        pp_inst node instr;
        let (env, heap, logs) = Domain.to_triple astate in
        let post = 
            (match instr with
            | Load (id, e1, typ, loc) -> 
                let lhs_var = I2D.get_var_from_id id in
                let env' = 
                    (match Env.find_opt lhs_var env with
                    | Some _ -> env
                    | _ -> Env.add lhs_var (LocationHandler.create_new_constloc ()) env)
                in
                let lhs_addr = Env.find lhs_var env' in
                let rhs_val = exec_expr env heap e1 in
                let nval = Helper.get rhs_val heap in
                let heap' = Heap.add lhs_addr nval heap in
                Domain.make env' heap' logs
            | Store (e1, typ, e2, loc) -> 
                if is_ret e1 then
                    let rhs_val = exec_expr env heap e2 in
                    let mname = Typ.Procname.to_string (Procdesc.get_proc_name proc_data.ProcData.pdesc) in
                    match RetAddrMap.find_opt mname !ret_addr_map with
                    | None ->
                        let ret_addr = LocationHandler.create_new_constloc() in
                        let nret_addr_map = RetAddrMap.add mname ret_addr !ret_addr_map in
                        let heap' = Heap.add ret_addr rhs_val heap in
                        (ret_addr_map := nret_addr_map; Domain.make env heap' logs)
                    | Some s -> 
                        let prev_val = Heap.find s heap in
                        let nv = Helper.(prev_val + rhs_val) in
                        let heap' = Heap.add s nv heap in
                        Domain.make env heap' logs
                else 
                    let lhs_val = exec_expr env heap e1 in
                    let rhs_val = exec_expr env heap e2 in
                    let heap' = Helper.store lhs_val rhs_val heap in
                    Domain.make env heap' logs
            | Prune (e, loc, b, i) -> astate
            | Call ((id, typ_e1), e1, args, loc, flag) -> 
                let lhs_var = I2D.get_var_from_id id in
                let env' = 
                    (match Env.find_opt lhs_var env with
                    | Some _ -> env
                    | _ -> Env.add lhs_var (LocationHandler.create_new_constloc ()) env)
                in
                let lhs_addr = Env.find lhs_var env' in
                let fun_name = get_fun_name e1 in
                if is_jni_fun fun_name then (
                    (* handle JNI funtion calls *)
                    let jfun = JNIFun.create (Typ.Procname.to_string fun_name) in
                    let (heap', rev_arg_locs) = 
                        let f (ih, il) (e, _) = 
                            let new_loc = LocationHandler.create_new_constloc () in
                            let v = exec_expr env' heap e in
                            (Heap.add new_loc v ih, LocList.cons new_loc il)
                        in
                        Caml.List.fold_left f (heap, LocList.empty) args
                    in
                    let arg_locs = LocList.rev rev_arg_locs in
                    let ret_symbol = LocationHandler.create_new_symbol () in
                    let heap'' = Heap.add lhs_addr (AbstractValueSet.singleton (AbsValWCst.create (AbsVal.loc_to_val ret_symbol) Cst.cst_t)) heap' in
                    let log_unit = LogUnit.create ret_symbol jfun arg_locs heap'' in
                    let logs' = CallLogs.add log_unit logs in  
                    Domain.make env' heap'' logs'
                ) else (
                    (* handle normal function calls *)
                    (match get_fun_desc fun_name with
                    | None -> 
                        L.progress "The callee function is missing. use 'bottom' instead the summary.:%s\n@." (Typ.Procname.to_string fun_name);
                        Domain.make env' (Heap.add lhs_addr AbstractValueSet.empty heap) logs
                    | Some fun_desc -> 
                        let args_v = Caml.List.map (fun (e, _) -> exec_expr env' heap e) args in
                        (match Summary.get fun_name with
                        | Some s ->
                            (match s.Summary.payloads.Payloads.semantic_summary with
                            | Some (before, after) -> 
                                let (benv, bheap, blogs) = Domain.to_triple before in
                                let (aenv, aheap, alogs) = Domain.to_triple after in
                                let ienv = Helper.map_args fun_desc args_v benv env' bheap heap in
                                let heap' = Helper.compose_heap heap aheap heap ienv in
                                let logs' = Helper.compose_log logs alogs heap ienv in
                                (match RetAddrMap.find_opt (Typ.Procname.to_string fun_name) !ret_addr_map with
                                | None -> Domain.make env' (Heap.add lhs_addr AbstractValueSet.empty heap') logs' (* if there is no return values, the lhs value cannot be used in following statements *)
                                | Some ret -> 
                                    let ret_val = Heap.find ret heap' in
                                    Domain.make env' (Heap.add lhs_addr ret_val heap') logs')
                            | None -> 
                                L.progress "It might be a recursive call. For the first time, use 'bottom' instead of the missing summary.: %s\n@." (Typ.Procname.to_string fun_name);
                                Domain.make env' (Heap.add lhs_addr AbstractValueSet.empty heap) logs
                                    (* failwith "Semantic Summary must exist." *)
                            )
                        | None -> 
                            L.progress "Summary of the function does not exist. use 'bottom' instead the summary.:%s\n@." (Typ.Procname.to_string fun_name);
                            Domain.make env' (Heap.add lhs_addr AbstractValueSet.empty heap) logs))
                )
            | Nullify (pid, loc) -> astate
            | Abstract loc -> astate
            | ExitScope (id_list, loc) -> astate
            ) in
            pp_domain "POST: " post;
            post

    let pp_session_name _node fmt = F.pp_print_string fmt "C/C++ semantic summary analysis"
end

module Analyzer = AbstractInterpreter.MakeRPO (TransferFunctions (ProcCfg.Exceptional))

module LocSet = Caml.Set.Make(AbsLoc)

module TypMap : sig 
    type t
    val find_opt : Typ.t -> t -> LocSet.t option
    val add : Typ.t -> LocSet.t -> t -> t
    val cardinal : t -> int
    val empty : t
end = struct
    open Typ
    type t = (Typ.t * LocSet.t) list

    let rec equal t1 t2 = 
        match t1, t2 with
        | Tptr (st1, _), Tptr (st2, _) -> equal st1.desc st2.desc
        | _, _ -> Typ.equal_desc t1 t2

    let find_opt k m = 
        match Caml.List.find_opt (fun (i, _) -> equal k.desc i.desc) m with
        | Some (h, r) -> Some r
        | _ -> None
    let rec add k v m =  
        match m with
        | (i, j) :: t -> 
            if equal k.desc i.desc then
                add k v t
            else 
                (i, j) :: (add k v t)
        | [] -> (k, v) :: []
    let cardinal x = Caml.List.length x
    let empty = []
end

module DTyp = struct
    include Typ

    let get_length typ = 
        match typ.desc with
        | Tarray {length = Some len} -> IntLit.to_int_exn len
        | _ -> -1

    let is_array typ = 
        match typ.desc with 
        | Tarray {elt} -> true
        | _ -> false

    let array_elem typ = 
        match typ.desc with 
        | Tarray {elt} -> elt
        | _ -> 
                let real_typ = Typ.to_string typ in
                failwith ("This type is not an array type: " ^ real_typ)
end

let is_glob_init = ref false

let glob_env = ref Env.empty

let glob_heap = ref Heap.empty

let glob_tmap = ref TypMap.empty

let init_env vars env : Env.t =
    let f i v = Env.add v (LocationHandler.create_new_constloc ()) i in
    Caml.List.fold_left f env vars 

let get_struct typ tenv =
    match Typ.name typ with
    | Some s -> 
        (match Tenv.lookup tenv s with
        | Some s -> 
                s
        | None -> 
            failwith ("The structure cannot be found in a type environment: " ^ (Typ.to_string typ)))
    | _ -> failwith "this typ is not a struct type: %s." (Typ.to_string typ)

(* Make a structure : a map from string (fieldname) to AbsLoc (a location of the field) *)
let mk_new_const_struct tenv typ = 
    let rec new_struct_impl tenv typ heap =
        let s = get_struct typ tenv in
        let fill_up_fields (s, h) (x, typ, _) = 
            let nloc = LocationHandler.create_new_constloc () in
            if is_struct typ then 
                let (sub_str, h') = new_struct_impl tenv typ h in
                let str_v = AbsVal.struct_to_val sub_str in
                let str_abs = AbstractValueSet.singleton (str_v, Cst.cst_t) in
                (Struct.add x nloc s, Heap.add nloc str_abs h')
            else (Struct.add x (LocationHandler.create_new_constloc ()) s, h)
        in
        Caml.List.fold_left fill_up_fields (Struct.empty, heap) s.Typ.Struct.fields
    in
    new_struct_impl tenv typ Heap.empty


let mk_new_const_array_opt tenv typ = 
    let rec new_array_impl tenv typ heap =
        let elem_typ = DTyp.array_elem typ in 
        let length = DTyp.get_length typ in
        if (phys_equal length (-1)) then None (* do not handle dynamic arrays *)
        else
            let rec mk_elem_locs locs length = 
                if (phys_equal length 0) then Caml.List.rev locs
                else 
                    let nloc = LocationHandler.create_new_constloc () in
                    mk_elem_locs (nloc :: locs) (length - 1)
            in
            let elem_locs = mk_elem_locs [] length in
            if is_struct elem_typ then
                (* TODO: handle struct type arrays *)
                None
            else if DTyp.is_array elem_typ then
                (* TODO: handle two or more dimension arrays *)
                None
            else
                Some (elem_locs, heap)
    in
    new_array_impl tenv typ Heap.empty


let init_heap_for_locals vars typs tenv env heap =
    let f h v t = 
        let assign_loc heap from typ =
            if is_struct typ then
                let (str, str_heap) = mk_new_const_struct tenv typ in
                let str_v = AbsVal.struct_to_val str in
                let str_abs = AbstractValueSet.singleton (str_v, Cst.cst_t) in
                let merged_heap = Heap.disjoint_union heap str_heap in
                Heap.add from str_abs merged_heap
            else if DTyp.is_array typ then
                let () = L.progress "This is array type: %a\n@." (Typ.pp_full Pp.text) typ in
                match mk_new_const_array_opt tenv typ with
                | None -> heap
                | Some (arr, heap') -> 
                        let arr_v = AbsVal.array_to_val arr in
                        let arr_abs = AbstractValueSet.singleton (arr_v, Cst.cst_t) in
                        let merged_heap = Heap.disjoint_union heap heap' in
                        Heap.add from arr_abs merged_heap
            else
                let newloc = (LocationHandler.create_new_symbol ()) in
                Heap.add from (AbstractValueSet.singleton (AbsVal.loc_to_val newloc, Cst.cst_t)) heap
        in
        assign_loc h (Env.find v env) t
    in
    Caml.List.fold_left2 f heap vars typs

module OriginMap = Caml.Map.Make(AbsLoc)
let glob_originMap = ref OriginMap.empty 
let originMap = ref OriginMap.empty

let init_heap arg_vars arg_typs tenv env h tmap = 
    let add_origin from v = 
        L.progress "Add from: %s to: %s\n@." (AbsLoc.pp from) (AbsVal.pp v);
        L.progress "Before size: %d\n@." (OriginMap.cardinal !originMap);
        (originMap := (OriginMap.add from v !originMap));
        L.progress "After size: %d\n@." (OriginMap.cardinal !originMap)
    in
    let find_origin l = 
        L.progress "Finding %s, size: %d\n@." (AbsLoc.pp l) (OriginMap.cardinal !originMap);
        OriginMap.find l !originMap 
    in
    let add_to_tmap k v m = 
        match TypMap.find_opt k m with
        | None -> TypMap.add k (LocSet.singleton v) m 
        | Some s -> TypMap.add k (LocSet.add v s) m 
    in
    let is_gt l1 l2 = phys_equal (AbsLoc.compare l1 l2) 1 in
    let pos_aliases l t tmap = 
        match TypMap.find_opt t tmap with
        | None -> LocSet.empty 
        | Some s -> LocSet.filter (is_gt l) s
    in
    let f (h, tmap) v t =  
        let rec impl tmap h l t =  
            let handle_struct tmap h l t =
                let s = get_struct t tenv in
                let f (str, h, tmap) (fn, typ, _) = 
                    let nloc = LocationHandler.create_new_constloc () in
                    let (h', tmap') = impl tmap h nloc typ in
                    (Struct.add fn nloc str, h', tmap')
                in
                Caml.List.fold_left f (Struct.empty, h, tmap) s.Typ.Struct.fields
            in
            let handle_array tmap h l t =
                let elem_typ = DTyp.array_elem t in 
                let length = DTyp.get_length t in
                if (phys_equal length (-1)) then (None, h, tmap) (* do not handle dynamic arrays *)
                else
                    let rec mk_elem_locs locs length = 
                        if (phys_equal length 0) then Caml.List.rev locs
                        else 
                            let nloc = LocationHandler.create_new_constloc () in
                            mk_elem_locs (nloc :: locs) (length - 1)
                    in
                    let elem_locs = mk_elem_locs [] length in
                    let f (h, tmap) l = impl tmap h l elem_typ in
                    let (h', tmap') = Array.fold_left f (h, tmap) elem_locs in
                    (Some elem_locs, h', tmap')
            in
            if Typ.is_pointer t then
                let aliases = pos_aliases l t tmap in
                let f a (i, cst) = 
                    let ncst = (Cst.cst_and cst (Cst.cst_eq l a)) in
                    let absval = find_origin a in(* AbsVal.loc_to_val (AbsLoc.ct_pointer a) in*)
                    let avwcst = AbsValWCst.create absval ncst in
                    (AbstractValueSet.add avwcst i, Cst.cst_and cst (Cst.cst_not (Cst.cst_eq l a)))
                in
                let (avs, cst) = LocSet.fold f aliases (AbstractValueSet.empty, Cst.cst_t) in
                let ntyp = Typ.strip_ptr t in
                if is_struct ntyp then
                    let (str, h', tmap') = handle_struct tmap h l ntyp in
                    let fabsval = AbsVal.struct_to_val str in
                    let () = add_origin l fabsval in
                    let favwcst = AbsValWCst.create fabsval cst in
                    let navs = AbstractValueSet.add favwcst avs in
                    (Heap.add l navs h', tmap')
                else 
                    let nloc = AbsLoc.ct_pointer l in
                    let fabsval = AbsVal.loc_to_val nloc in
                    let () = add_origin l fabsval in
                    let favwcst = AbsValWCst.create fabsval cst in
                    let navs = AbstractValueSet.add favwcst avs in
                    let ntmap = add_to_tmap ntyp nloc tmap in
                    let nh = Heap.add l navs h in
                    impl ntmap nh nloc ntyp
            else if is_struct t then
                let (str, h', tmap') = handle_struct tmap h l t in
                let sabv = AbsVal.struct_to_val str in
                let () = add_origin l sabv in
                let sabs = AbstractValueSet.singleton (sabv, Cst.cst_t) in
                let h'' = Heap.add l sabs h' in
                (h'', tmap')
            else if DTyp.is_array t then
                let (arr_opt, h', tmap') = handle_array tmap h l t in
                match arr_opt with
                | None -> (h, tmap) (* do not handle dynamic arrays *)
                | Some arr -> 
                        let arr_abv = AbsVal.array_to_val arr in
                        let () = add_origin l arr_abv in
                        let arr_abs = AbstractValueSet.singleton (arr_abv, Cst.cst_t) in
                        let h'' = Heap.add l arr_abs h' in
                        (h'', tmap')
            else 
                (h, tmap)
        in
        let vl = Env.find v env in
        if is_struct t then
            impl tmap h vl t
        else 
            let nloc = LocationHandler.create_new_symbol() in
            let absval = AbsVal.loc_to_val nloc in
            let avwcst = AbsValWCst.create absval Cst.cst_t in
            let avs = AbstractValueSet.singleton avwcst in
            let nh = Heap.add vl avs h in
            let ntmap = add_to_tmap t nloc tmap in
            impl ntmap nh nloc t
    in
    Caml.List.fold_left2 f (h, tmap) arg_vars arg_typs

let init_for_glob tenv =
    (if not !is_glob_init then
        let () = (procs := PreForGlobal.Storage.load_fun ()) in
        let nm = PreForGlobal.Storage.load () in
        let f k v (vl, tl) =
            let nvar = Var.create k in
            (Caml.List.cons nvar vl, Caml.List.cons v tl)
        in
        let (vars, typs) = PreForGlobal.NameType.fold f nm ([], []) in
        let env = init_env vars Env.empty in
        let (heap, tmap) = init_heap vars typs tenv env Heap.empty TypMap.empty in
        glob_originMap := !originMap;
        (is_glob_init := true; glob_env := env; glob_heap := heap; glob_tmap := tmap));
    (!glob_env, !glob_heap, !glob_tmap)


let init tenv pdesc : Env.t * Heap.t = 
    let attrs = Procdesc.get_attributes pdesc in
    let args = attrs.formals in
    let locals = attrs.locals in
    let (arg_vars, arg_typs) = 
        let f (v, typ) (vl, tl) = ((Var.create (Mangled.to_string v))::vl, typ::tl) in
        Caml.List.fold_right f args ([], []) in
    let (local_vars, local_typs) = 
        let f (v: ProcAttributes.var_data) (vl, tl) = ((Var.create (Mangled.to_string v.name))::vl, (v.typ)::tl) in
        Caml.List.fold_right f locals ([], [])
    in
    let (ei, hi, ti) = init_for_glob tenv in
    let env = init_env (arg_vars @ local_vars) ei in
    let hi' = init_heap_for_locals local_vars local_typs tenv env hi in
    originMap := !glob_originMap;
    let (heap, _) = init_heap arg_vars arg_typs tenv env hi' ti in
    originMap := OriginMap.empty;
    (env, heap)

let print_state pp fmt (s: Domain.t AbstractInterpreter.State.t) =
    F.fprintf fmt "#pre: %a \n #post: %a \n@." pp s.pre pp s.post
   
let print_result res = 
    L.progress "res: %a\n@." (Analyzer.InvariantMap.pp ~pp_value: (print_state Domain.pp)) res

let get_astate (s: Domain.t AbstractInterpreter.State.t) =
    s.post

    (* 
    val fold: (AbsValWCst.t -> 'a -> 'a) -> t -> 'a -> 'a
    *)
    (* (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b *)
let optimize astate = 
    let (env, heap, logs) = Domain.to_triple astate in
    let opt_heap l v h = 
        let opt_abs_set (v, cst) i =
            match SMTSolver.is_sat cst with
            | (true, cst') -> AbstractValueSet.add (v, cst') i
            | _ -> i
        in
        let v' = AbstractValueSet.fold opt_abs_set v AbstractValueSet.empty in
        if AbstractValueSet.is_empty v' then
            h
        else
            Heap.add l v' h
    in 
    let opt_logs l = 
        let h = l.LogUnit.heap in
        let h' = Heap.fold opt_heap h Heap.empty in
        { l with LogUnit.heap = h' }
    in
    let heap' = Heap.fold opt_heap heap Heap.empty in
    let logs' = CallLogs.map opt_logs logs in
    Domain.make env heap' logs' 

let checker {Callbacks.proc_desc; tenv; summary} : Summary.t =
    let proc_name = Procdesc.get_proc_name proc_desc in
    if not (is_jni_fun proc_name) then (
        let () = L.progress "Analyzing a function %s\n@." (Typ.Procname.to_string proc_name) in
        let (env, heap) = init tenv proc_desc in
        let before_astate = Domain.make env heap CallLogs.empty in
        let proc_data = ProcData.make_default proc_desc tenv in 
        match Analyzer.compute_post proc_data ~initial:before_astate with
        | Some p -> 
                let opt_astate = optimize p in
        let session = incr summary.Summary.sessions ; !(summary.Summary.sessions) in
        let summ' = {summary with Summary.payloads = { summary.Summary.payloads with Payloads.semantic_summary = Some (before_astate, opt_astate)}; Summary.proc_desc = proc_desc; Summary.sessions = ref session} in
        Summary.store summ'; 
        pp_domain "FINAL:" opt_astate;
        summ'
        | None -> summary
        (*
        let res = Analyzer.exec_pdesc (ProcData.make_default proc_desc tenv) ~initial:before_astate in
        let exit_node_id = Procdesc.Node.get_id (Procdesc.get_exit_node proc_desc) in
        let after_astate = get_astate (Analyzer.InvariantMap.find exit_node_id res) in
        let opt_astate = optimize after_astate in
        let session = incr summary.Summary.sessions ; !(summary.Summary.sessions) in
        let summ' = {summary with Summary.payloads = { summary.Summary.payloads with Payloads.semantic_summary = Some (before_astate, opt_astate)}; Summary.proc_desc = proc_desc; Summary.sessions = ref session} in
        Summary.store summ'; 
        pp_domain "FINAL:" opt_astate;
        summ'
    )
    *)
    )
    else 
        (L.progress "Skiping analysis for a JNI function %s\n@." (Typ.Procname.to_string proc_name); summary)
