open! IStd
open Core
module F = Format
module L = Logging
module Sem = SemanticFunctions
open SemanticSummaryDomain
open SUtils

module PpSumm = struct
  let get_inst_type (i: Sil.instr) = 
    match i with
    |Load _ -> 
      "Load"
    | Store _ -> 
        "Store"
    | Prune _ -> 
        "Prune"
    | Call _ -> 
        "Call"
    | Nullify _ -> 
        "Nullify"
    | Abstract _ -> 
        "Abstract"
    | ExitScope _ -> 
        "ExitScope"

  let pp_inst fmt ((node, index), inst) = 
    let node_id = Procdesc.Node.get_id node in
    let inst_type = get_inst_type inst in
    F.fprintf fmt "%a: [%s] %a" Procdesc.Node.pp_id node_id inst_type (Sil.pp_instr ~print_types: true Pp.text) inst
end

module TypMap = struct
  module M = PrettyPrintable.MakePPMap(
    struct 
      include Typ
      let pp = pp_full Pp.text
    end
  )

  include (M: module type of M with type 'a t := 'a M.t)
  
  type t = LocSet.t M.t

  let add typ loc map =  
    match find_opt typ map with
    | Some s ->
        let loc_set = LocSet.add loc s in
        add typ loc_set map
    | None ->
        add typ (LocSet.singleton loc) map 

  let find_opt typ map =
    bindings map
    |> Caml.List.find_opt (fun (typ', v) -> Typ.equal_ignore_quals typ typ')

  let pp = pp ~pp_value: LocSet.pp
end

module GlobalEnv = struct
  let glob_env = ref SemanticSummaryDomain.Env.empty

  let glob_heap = ref SemanticSummaryDomain.Heap.empty

  let glob_tmap = ref TypMap.empty

  let is_global_init = ref false
end

let get_global_heap () = !GlobalEnv.glob_heap

let is_struct = Typ.is_cpp_class

module CFG = ProcCfg.NormalOneInstrPerNode

module TransferFunctions = struct
  module CFG = CFG
  module Helper = HelperFunction
  module Domain = SemanticSummaryDomain.Domain
  module Model = SemanticModels
  open SemanticSummaryDomain

  type extras = ProcData.no_extras

  let opt_heap_every_stmt = false

  let mk_domain env heap logs = 
    if opt_heap_every_stmt then
      Domain.make env (Optimizer.opt_cst_in_heap heap) logs
    else
      Domain.make env heap logs

  let get_proc_summary ?caller callee_name = 
    let () = L.progress "Request summary of %s\n@." (Typ.Procname.to_string callee_name) in
    let sum = 
      match caller with
      | Some s -> (
        Ondemand.analyze_proc_name ~caller_pdesc:s callee_name)
      | None -> (
        Ondemand.analyze_proc_name callee_name)
    in
    match sum with
    | Some s -> (
        match s.Summary.payloads.Payloads.semantic_summary with
        | Some _ as o -> 
            o
        | None -> 
            None)
    | None -> 
        None

  let rec exec_expr : Var.var_scope -> Env.t -> Heap.t -> Exp.t -> AVS.t * Heap.t =
    fun scope env heap expr ->
      match expr with
      | Var i -> 
          let var = Var.of_id i ~proc:scope in
          let loc = Env.find var env in
          Heap.find loc heap, heap
      | UnOp (op, e, typ) ->
          let v, heap' = exec_expr scope env heap e in
          AbstractOperators.unop op v, heap'
      | BinOp (op, e1, e2) -> 
          let lhs, heap' = exec_expr scope env heap e1 in
          let rhs, heap'' = exec_expr scope env heap' e2 in
          AbstractOperators.binop op lhs rhs, heap''
      | Exn typ ->
          (* do not handle exceptions *)
          AVS.top, heap
      | Closure f -> 
          failwith "C does not support anonymous functions"
      | Const c -> 
          (* only handle string constants *)
          (match c with
          | Cint s -> 
              let i = IntLit.to_int_exn s |> IInt.of_int in
              AVS.singleton (Val.of_int i, Cst.cst_true), heap
          | Cfun _ ->
              AVS.bot, heap
          | Cstr s -> 
              let nloc = Loc.mk_dyn () in
              let str = SStr.of_string s in
              let heap' = Heap.add nloc (AVS.singleton (Val.of_str str, Cst.cst_true)) heap in
              AVS.singleton (Val.of_loc nloc, Cst.cst_true), heap'
          | Cfloat _ -> AVS.top, heap
          | Cclass _ -> AVS.bot, heap)
      | Cast (typ, e) ->
          (* TODO: need type casting? *)
          exec_expr scope env heap e
      | Lvar pvar -> (* Location of a variable *)
          let var = Var.of_pvar pvar ~proc:scope in
          let loc = Env.find var env in
          AVS.singleton (Val.of_loc loc, Cst.cst_true), heap
      | Lfield (e, fn_tn, typ) -> (* Location of a field *)
          let field = Typ.Fieldname.to_string fn_tn |> Loc.mk_index_of_string in
          let obj_loc_avs, heap' = exec_expr scope env heap e in
          let obj_avs =
            (fun ((obj_loc_val: Val.t), obj_loc_cst) avs -> (
              match obj_loc_val with
              | Loc loc -> 
                  Heap.find loc heap'
                  |> (fun x -> Helper.((x ^ obj_loc_cst) + avs))
              | _ -> 
                  failwith "An object must be assigned to a location."))
            |> (fun x -> AVS.fold x obj_loc_avs AVS.empty)
          in
          let field_loc_avs = 
            (fun ((obj_val: Val.t), obj_cst) ->
              match obj_val with
              | Loc l ->
                  Loc.mk_offset l field
                  |> (fun x -> Val.of_loc x, obj_cst)
              | _ -> 
                  failwith "An object must be a location itself.")
            |> (fun x -> AVS.map x obj_avs)
          in
          field_loc_avs, heap'
      | Lindex (e1, e2) -> (* &(e1[e2]) *)
          let index_avs, heap' = exec_expr scope env heap e2 in (* value of e2 *)
          let arr_avs, heap'' = exec_expr scope env heap' e1 in (* location of e1 *)
          (*
          let arr_avs = (* value of e1 *)
            (fun ((arr_loc_val: Val.t), arr_loc_cst) avs -> (
              match arr_loc_val with
              | Loc loc ->
                  Heap.find loc heap''
                  |> (fun x -> Helper.((x ^ arr_loc_cst) + avs))
              | _ ->
                  failwith "An array must be assigned to a location."))
            |> (fun x -> AVS.fold x arr_loc_avs AVS.empty)
          in
          *)
          let arr_index_pair = Helper.(arr_avs * index_avs) in (* pair of e1 val and e2 val *)
          let arr_elem_loc_avs = (* loc of e1[e2] *)
            (fun (((arr_val: Val.t), arr_cst), ((index_val: Val.t), index_cst)) avs -> (
              match arr_val, index_val with
              | Loc arr_loc, Int (Int i) ->
                  Loc.mk_index_of_int i
                  |> Loc.mk_offset arr_loc 
                  |> (fun x -> Val.of_loc x, Cst.cst_and arr_cst index_cst)
                  |> (fun x -> AVS.add x avs)
              | Loc arr_loc, Int Top ->
                  Loc.mk_offset arr_loc Loc.index_top
                  |> (fun x -> Val.of_loc x, Cst.cst_and arr_cst index_cst)
                  |> (fun x -> AVS.add x avs)
              | Loc arr_loc, Loc index_loc ->
                  Loc.mk_index_of_expr index_loc
                  |> Loc.mk_offset arr_loc
                  |> (fun x -> Val.of_loc x, Cst.cst_and arr_cst index_cst)
                  |> (fun x -> AVS.add x avs)
              | _ -> 
                  failwith ("Unmatched loc index pair: " ^ (F.asprintf "(%a, %a)" Val.pp arr_val Val.pp index_val))))
            |> (fun x -> Caml.List.fold_right x arr_index_pair AVS.empty)
          in
          arr_elem_loc_avs, heap''
      | Sizeof data -> 
          (* TODO: does not always return a constant 1 *)
          AVS.singleton (Val.of_int (IInt.of_int 1), Cst.cst_true), heap
          (* AVS.singleton (Val.of_int (Int.top), Cst.cst_true) *)

  let exec_instr : Domain.t -> extras ProcData.t -> CFG.Node.t -> Sil.instr -> Domain.t = 
    fun {env; heap; logs} {pdesc; tenv; extras} node instr ->
      let () = L.progress "%a\n@." PpSumm.pp_inst (node, instr) in
      let scope = Var.mk_scope (Typ.Procname.to_string @@ Procdesc.get_proc_name pdesc) in
      match instr with
      | Load (id, e1, typ, loc) -> 
          let lhs_var = Var.of_id id ~proc:scope in
          let env' = 
            if Env.mem lhs_var env then 
              env
            else (* for temporal variables *)
              Env.add lhs_var (Loc.mk_const lhs_var) env
          in
          let lhs_addr = Env.find lhs_var env' in
          let rhs_val, heap' = exec_expr scope env heap e1 in
          let rhs_avs = Helper.load rhs_val heap in
          let heap'' = Heap.add lhs_addr rhs_avs heap' in
          mk_domain env' heap'' logs
      | Store (Lvar pvar, typ, e2, loc) when Pvar.is_return pvar -> (* for return statements *)
          let rhs_avs, heap' = exec_expr scope env heap e2 in
          let mname = Typ.Procname.to_string (Procdesc.get_proc_name pdesc) in
          let ret_loc = Loc.mk_ret mname in
          let heap'' = Heap.weak_update ret_loc rhs_avs heap' in
          mk_domain env heap'' logs
      | Store (Lindex (arr, index), typ, e2, loc) -> (* for array statements *)
          let lhs_avs, heap' = exec_expr scope env heap (Lindex (arr, index)) in
          let rhs_avs, heap''  = exec_expr scope env heap' e2 in
          let heap''' = Helper.store lhs_avs rhs_avs heap'' in
          mk_domain env heap''' logs
      | Store (e1 , typ, e2, loc) -> 
          (* let () = L.progress "Domain:%a\n@." Domain.pp (mk_domain env heap logs) in *)
          let lhs_avs, heap' = exec_expr scope env heap e1 in
          let rhs_avs, heap'' = exec_expr scope env heap' e2 in
          let heap''' = Helper.store lhs_avs rhs_avs heap'' in
          mk_domain env heap''' logs
      | Prune (e, loc, b, i) -> (* do not support heap pruning *)
          {env; heap; logs}
      | Call ((id, ret_typ), (Const (Cfun callee_pname)), args, loc, flag) when JniModel.is_jni callee_pname -> (* for jni function calls *)
          let lhs_var = Var.of_id id ~proc:scope in
          let env' = 
            if Env.mem lhs_var env then
              env
            else (* for temporal variables *)
              Env.add lhs_var (Loc.mk_const lhs_var) env
          in
          let lhs_loc = Env.find lhs_var env' in
          let ret_loc = Loc.mk_dyn () in
          let ret_loc_ptr = Loc.mk_pointer ret_loc in
          let heap' = Heap.add lhs_loc (AVS.singleton (Val.of_loc ret_loc, Cst.cst_true)) heap in
          let heap'' = Heap.add ret_loc (AVS.singleton (Val.of_loc ret_loc_ptr, Cst.cst_true)) heap' in
          let jnifun = JNIFun.of_procname callee_pname in
          let dumped_heap, arg_locs = 
            (fun (arg_expr, _) (dumped_heap, arg_locs) ->
              let arg_loc = Loc.mk_dyn () in
              let arg_avs, heap' = exec_expr scope env' dumped_heap arg_expr in
              (Heap.add arg_loc arg_avs heap', arg_loc :: arg_locs))
            |> (fun f -> Caml.List.fold_right f args (heap'', []))
          in
          let log = LogUnit.mk ret_loc jnifun arg_locs dumped_heap in
          let logs' = CallLogs.add log logs in  
          mk_domain env' heap'' logs'
      | Call ((id, ret_typ), (Const (Cfun callee_pname)), args, loc, flag) when Model.is_modeled callee_pname -> (* for modeled functions for summary generation *)
          let lhs_var = Var.of_id id ~proc:scope in
          let env' = 
            if Env.mem lhs_var env then
              env
            else (* for temporal variables *)
              Env.add lhs_var (Loc.mk_const lhs_var) env
          in
          let lhs_loc = Env.find lhs_var env' in
          let args_avs, heap' =
            (fun (arg_expr, _) (avs, heap) -> 
              exec_expr scope env' heap arg_expr
              |> (fun (arg_avs, heap') -> arg_avs :: avs, heap'))
            |> (fun f -> Caml.List.fold_right f args ([], heap))
          in
          let {env; heap; logs} = Model.apply_semantics lhs_var (Typ.Procname.to_string callee_pname) args_avs env' heap' logs in
          mk_domain env heap logs 
      | Call ((id, ret_typ), (Const (Cfun callee_pname)), args, loc, flag) -> 
          let lhs_var = Var.of_id id ~proc:scope in
          let env' = 
            if Env.mem lhs_var env then
              env
            else (* for temporal variables *)
              Env.add lhs_var (Loc.mk_const lhs_var) env
          in
          let lhs_loc = Env.find lhs_var env' in
          (match Ondemand.get_proc_desc callee_pname with 
          | Some callee_desc -> (* no exisiting function: because of functions Infer made *)
              (match get_proc_summary ~caller:pdesc callee_pname with
              | Some ({ env = end_env; heap = end_heap; logs = end_logs}) ->
                let args_avs, heap' = 
                  Caml.List.fold_right (fun (arg_expr, _) (avs, heap) -> 
                    let arg_avs, heap' = exec_expr scope env' heap arg_expr in
                    arg_avs :: avs, heap') args ([], heap)
                in
                let ienv = 
                  Instantiation.mk_ienv tenv callee_desc args_avs end_env end_heap heap' (get_global_heap ())
                in
                let heap'' = 
                  Instantiation.comp_heap heap' heap' end_heap ienv 
                in
                let logs' = 
                  Instantiation.comp_log logs end_logs heap'' ienv 
                in
                let ret_loc = 
                  Loc.mk_ret_of_pname callee_pname 
                in
                let heap''' = 
                  (match Heap.find_opt ret_loc heap'' with
                  | Some avs -> 
                      Heap.add lhs_loc avs heap''
                  | None -> (* kind of passing parameter as a return value *)
                      let ret_param_var = Var.of_string "__return_param" ~proc:scope in
                      (match Env.find_opt ret_param_var end_env with
                        | Some loc -> 
                            (match Heap.find_opt loc heap'' with
                            | Some avs ->
                              Heap.add lhs_loc avs heap''
                            | None ->
                              Heap.add lhs_loc (AVS.singleton (Val.bot, Cst.cst_true)) heap'')
                        | None -> 
                            Heap.add lhs_loc (AVS.singleton (Val.bot, Cst.cst_true)) heap'')
                   )
                in
                mk_domain env' heap''' logs'
              | None -> 
                  let () = L.progress "Not existing callee. Just ignore this call.\n@." in
                  {env; heap; logs}
                  )
          | None -> 
              let () = L.progress "Not existing callee. Just ignore this call.\n@." in
              {env; heap; logs})
      | Call _ ->
          failwith "This statement is not supported in C/C++!"
      | Nullify (pid, loc) -> 
          {env; heap; logs}
      | Abstract loc -> 
          {env; heap; logs}
      | ExitScope (id_list, loc) -> 
          {env; heap; logs}

  let pp_session_name _node fmt = F.pp_print_string fmt "C/C++ semantic summary analysis" 
end

module Analyzer = AbstractInterpreter.MakeWTO (TransferFunctions)

module Initializer = struct
  open SemanticSummaryDomain
    
  module Helper = HelperFunction


  let init_env : (Var.t * Typ.t) list -> Env.t -> Env.t =
    fun vars env ->
      (fun env (var, typ) -> Env.add var (Loc.mk_const var) env)
      |> (fun f -> Caml.List.fold_left f env vars)

  let get_struct typ tenv =
    match Typ.name typ with
    | Some s -> 
      (match Tenv.lookup tenv s with
      | Some s -> 
          s
      | None -> 
          failwith ("The structure cannot be found in a type environment: " ^ (Typ.to_string typ)))
    | _ -> failwith ("this typ is not a struct type: " ^ (Typ.to_string typ))

  let mk_tmap: (Loc.t * Typ.t) list -> Tenv.t -> TypMap.t -> TypMap.t =
    fun loc_typs tenv tmap ->
      let rec f: (Loc.t * Typ.t) -> TypMap.t -> TypMap.t = 
        fun (loc, typ) tmap ->
          if JniModel.is_jni_struct typ then
            tmap
          else 
            let tmap' = TypMap.add typ loc tmap in
            match typ.Typ.desc with
            | Tptr (typ', kind) ->
                (f @@ (Loc.mk_pointer loc, typ')) @@ tmap'
            | Tstruct name ->
                let fld_typs = Helper.get_fld_and_typs name tenv in
                (fun tmap (field, typ) ->
                  f (Loc.mk_offset loc (Loc.mk_index_of_string field), typ) tmap)
                |> (fun f -> Caml.List.fold_left f tmap' fld_typs)
            | Tarray {elt; length = Some i} -> (* fixed size arrays *)
                let loc' = Loc.unwrap_ptr loc in (* C allocates array location directly to variable address *)
                let index = (IntLit.to_int_exn i) - 1 in
                let rec mk_array = fun i tmap -> (
                  match i with
                  | -1 -> 
                      tmap 
                  | _ ->
                      f (Loc.mk_offset loc' (Loc.mk_index_of_int i), (Typ.mk (Tptr (elt, Pk_pointer)))) tmap
                      |> mk_array (i - 1))
                in
                mk_array index tmap
            | Tint _ -> (* ignore non-pointer types *)
                tmap
            | Tfun _ -> (* ignore non-pointer types *)
                tmap 
            | Tvoid -> (* ignore non-pointer types *)
                tmap
            | _ ->
                failwith (F.asprintf "not support type: %a: %a." Loc.pp loc (Typ.pp_full Pp.text) typ)
      in
      Caml.List.fold_right f loc_typs tmap

  let init_heap: (Loc.t * Typ.t) list -> Tenv.t -> Heap.t -> TypMap.t -> Heap.t =
    fun loc_typs tenv heap tmap ->
      let is_gt = fun l1 l2 -> (Loc.compare l1 l2) = 1 in
      let pos_aliases = fun loc typ tmap -> (
        match TypMap.find_opt typ tmap with
        | None -> 
            LocSet.empty 
        | Some (_, s) -> 
            LocSet.filter (is_gt loc) s |> LocSet.filter (fun x -> Loc.is_pointer x || Loc.is_offset x))
      in
      let rec iter_loc : Heap.t -> (Loc.t * Typ.t) -> Heap.t =
        fun heap (loc, typ) ->
          if JniModel.is_jni_struct typ then
            heap
          else 
            let desc = typ.Typ.desc in
            let handle_alias = fun loc typ ->
              let avs, cst = 
                ((pos_aliases loc typ tmap)
                |> ((fun alias (avs, cst) ->
                    let cst' = 
                      Cst.cst_eq alias loc
                      |> Cst.cst_and cst
                    in
                    AVS.singleton (Val.of_loc alias, cst')
                    |> (fun avs' -> Helper.(avs + avs'), Cst.cst_and cst (Cst.cst_not cst')))
                  |> LocSet.fold))
                @@ (AVS.empty, Cst.cst_true)
              in
              AVS.add (Val.of_loc loc, cst) avs
            in
            match desc with
            | Tptr (ptr_typ, kind) ->
                let ptr_loc = Loc.mk_pointer loc in
                let heap' = 
                  handle_alias ptr_loc ptr_typ
                  |> (fun x -> Heap.add loc x heap)
                in
                iter_loc heap' (ptr_loc, ptr_typ)
            | Tstruct name ->
                let fld_typs = Helper.get_fld_and_typs name tenv in
                (fun heap (field, typ) ->
                  iter_loc heap (Loc.mk_offset loc (Loc.mk_index_of_string field), (Typ.mk (Tptr (typ, Pk_pointer)))))
                |> (fun f -> Caml.List.fold_left f heap fld_typs)
            | Tarray {elt; length = Some i} -> (* fixed size arrays *)
                let loc' = Loc.unwrap_ptr loc in (* C allocates array location directly to variable address *)
                let index = (IntLit.to_int_exn i) - 1 in
                let rec mk_array = fun i heap -> (
                  match i with
                  | -1 -> 
                      heap
                  | _ ->
                    let heap'' = iter_loc heap (Loc.mk_offset loc' (Loc.mk_index_of_int i), (Typ.mk (Tptr (elt, Pk_pointer)))) in
                    mk_array (i - 1) heap'')
                in
                mk_array index heap
            | _ -> 
                heap
      in
      Caml.List.fold_left iter_loc heap loc_typs

  let exec_initializers env heap tmap global_vars =
    let handle: Pvar.t * Typ.t -> Env.t * Heap.t -> Env.t * Heap.t =
      fun (pvar, typ) (env, heap) ->
        match Pvar.get_initializer_pname pvar with
        | Some callee_pname -> (
            match TransferFunctions.get_proc_summary callee_pname with
            | Some {env; heap; logs} ->
                let var = Var.of_pvar pvar in
                let update_tmap env heap tmap loc (typ: Typ.t) =
                  match typ.desc with
                  | Tptr (typ', kind) ->
                      tmap
                  | Tstruct name ->
                      tmap
                  | Tarray { elt; length = Some i } ->
                      tmap
                  | _ -> 
                      failwith "Unhandled type."
                in
                (env, heap)
            | None ->
                let () = L.progress "Cannot execute it!" in
                (env, heap))
        | None ->
            (env, heap)
    in
    Caml.List.fold_right handle global_vars (env, heap)

  let init_global : Tenv.t -> Env.t * Heap.t * TypMap.t =
    fun tenv ->
      let open GlobalEnv in
      if not !is_global_init then
        let globals = PreForGlobal.Storage.load () in
        let global_pvars = 
          (fun pvar typ res ->
            (pvar, typ) :: res)
          |> (fun x -> PreForGlobal.NameType.fold x globals [])
        in
        let global_vars = 
          (fun (pvar, typ) res ->
            (Var.of_pvar pvar, typ) :: res)
          |> (fun x -> Caml.List.fold_right x global_pvars [])
        in
        let env = init_env global_vars Env.empty in
        let loc_typs = 
          (fun (var, typ) locs ->
            (Loc.mk_pointer (Env.find var env), typ) :: locs)
          |> (fun f -> Caml.List.fold_right f global_vars [])
        in
        let tmap = mk_tmap loc_typs tenv TypMap.empty in
        let heap = init_heap loc_typs tenv Heap.empty tmap in
        let env', heap' = exec_initializers env heap tmap global_pvars in
        let () = is_global_init := true in
        let () = glob_env := env' in
        let () = glob_heap := heap' in
        let () = glob_tmap := tmap in
        !glob_env, !glob_heap, !glob_tmap
      else
        !glob_env, !glob_heap, !glob_tmap

  let init : Tenv.t -> Procdesc.t -> Env.t * Heap.t =
    fun tenv pdesc ->
      let attrs = Procdesc.get_attributes pdesc in
      let scope = Var.mk_scope (Typ.Procname.to_string (Procdesc.get_proc_name pdesc)) in
      let arg_vars = 
        (fun (arg, typ) -> (Var.of_string (Mangled.to_string arg) ~proc:scope, typ))
        |> (fun f -> Caml.List.map f attrs.formals) 
      in
      let local_vars = 
        (fun (var: ProcAttributes.var_data) -> 
          let name = var.name in
          let typ = var.typ in
          (Var.of_string (Mangled.to_string name) ~proc:scope, typ))
        |> (fun f -> Caml.List.map f attrs.locals)
      in
      let env, heap, tmap = init_global tenv in
      let env' = init_env (arg_vars @ local_vars) env in
      let locs_arg, locs_loc = 
        (fun (var, typ) -> Env.find var env', Typ.mk (Tptr (typ, Pk_pointer)))
        |> (fun f -> (Caml.List.map f arg_vars, Caml.List.map f local_vars))
      in
      let tmap' = mk_tmap locs_arg tenv tmap in
      let heap' = init_heap (locs_arg @ locs_loc) tenv heap tmap' in
      env', heap'
end

let checker {Callbacks.proc_desc; tenv; summary} : Summary.t =
    let proc_name = Procdesc.get_proc_name proc_desc in
    if not (JniModel.is_jni proc_name) then (
        let () = L.progress "Analyzing a function %s\n@." (Typ.Procname.to_string proc_name) in
        let (env, heap) = Initializer.init tenv proc_desc in
        let before_astate = SemanticSummaryDomain.make env heap SemanticSummaryDomain.CallLogs.empty in
        let proc_data = ProcData.make_default proc_desc tenv in 
        match Analyzer.compute_post proc_data ~initial:before_astate with
        | Some p -> 
                let opt_astate = Optimizer.optimize p (Typ.Procname.to_string proc_name) in
        let session = incr summary.Summary.sessions ; !(summary.Summary.sessions) in
        let summ' = {summary with Summary.payloads = { summary.Summary.payloads with Payloads.semantic_summary = Some opt_astate}; Summary.proc_desc = proc_desc; Summary.sessions = ref session} in
        Summary.store summ'; 
        (if JniModel.is_java_native proc_name then
          let ldg = LogDepGraph.mk_ldg opt_astate.logs in
          let dot_graph = LogDepGraph.DotPrinter.DotGraph.to_dot_graph ldg in
          let graph_str = F.asprintf "%a" LogDepGraph.DotPrinter.DotGraph.pp dot_graph in
          let oc = open_out ((Typ.Procname.to_string proc_name) ^ ".out") in
          let () = Printf.fprintf oc "%s" graph_str in
          close_out oc
        );
        L.progress "Final in %s: %a\n@." (Typ.Procname.to_string proc_name) SemanticSummaryDomain.pp opt_astate;
        summ'
        | None -> summary
    )
    else 
        (L.progress "Skiping analysis for a JNI function %s\n@." (Typ.Procname.to_string proc_name); summary)
