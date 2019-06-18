open! IStd
open Core
module F = Format
module L = Logging
module Sem = SemanticFunctions
open SemanticSummaryDomain
open SUtils

module AnalysisTargets = struct
  module Targets = PrettyPrintable.MakePPSet(Typ.Procname) 

  let targets_dat = "targets.dat"

  let load_targets () = 
    try
      let ic = Pervasives.open_in targets_dat in
      let res = Marshal.from_channel ic in
      Pervasives.close_in ic; res
    with _ ->
      Targets.empty
    
  let store_target proc_name = 
    let targets = load_targets () in
    let targets' = Targets.add proc_name targets in
    let oc = Pervasives.open_out targets_dat in
    Marshal.to_channel oc targets' [];
    Pervasives.close_out oc

  let is_targeted proc_name = true
  (*
    if JniModel.is_jni proc_name then
      false
    else if JniModel.is_callable_from_java proc_name then
      true
    else 
      let res = load_targets () in
      Targets.mem proc_name res
      *)

  let add_target proc_name = 
    store_target proc_name 
end

module PpSumm = struct
  let get_inst_type (i: Sil.instr) = 
    match i with
    | Load _ -> 
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

(* module TypeMap = struct ... end was moved to 'initializer.ml' *)

module CFG = ProcCfg.NormalOneInstrPerNode

module TransferFunctions = struct
  module CFG = CFG
  module Helper = HelperFunction
  module Domain = SemanticSummaryDomain.Domain
  module Model = SemanticModels
  open SemanticSummaryDomain

  type extras = ProcData.no_extras

  let fun_params pdesc =
  (*  if GlobalEnv.is_global_var_init_fun pdesc then
      let (pvar, typ) = GlobalEnv.get_initialized_global_ext pdesc in
      let holder, htyp = GlobalEnv.get_holder_var pdesc typ in
      [ holder, htyp ]
    else *)
      let attrs = Procdesc.get_attributes pdesc in
      let scope = VVar.mk_scope (Typ.Procname.to_string (Procdesc.get_proc_name pdesc)) in
      let args = attrs.formals in
      Caml.List.map (fun (m, typ) -> ((VVar.of_string (Mangled.to_string m) ~proc:scope), typ)) args

  let opt_heap_every_stmt = false

  let mk_domain heap logs = 
    (*let () = L.progress "ResHeap: %a\n@." Heap.pp heap in*)
    if opt_heap_every_stmt then
      Domain.make (Heap.opt_cst_in_heap heap) logs
    else
      Domain.make heap logs

  let rec get_proc_summary ?caller do_clear callee_name = 
    Ondemand.clear_cache ();
    let () = L.progress "Request summary of %s\n@." (Typ.Procname.to_string callee_name) in
    let () = AnalysisTargets.add_target callee_name in
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
        | None -> (
            if do_clear then (
              match Ondemand.get_proc_desc callee_name with
              | Some callee_pdesc -> (
                  let _ = Summary.reset callee_pdesc in
                  Ondemand.clear_cache ();
                  get_proc_summary ?caller false callee_name)
              | None ->
                  None)
            else
              None))
    | None -> 
        None

  let rec exec_expr scope location heap (expr: Exp.t) = 
    match expr with
    | Var i -> 
        let loc = Loc.of_id i ~proc:scope in
        Heap.find loc heap
    | UnOp (op, e, typ) ->
        let loc = Loc.mk_implicit (Location.to_string location) in
        Val.singleton (loc, Cst.cst_true)
    | BinOp (op, e1, e2) -> 
        let loc = Loc.mk_implicit (Location.to_string location) in
        Val.singleton (loc, Cst.cst_true)
    | Exn typ ->
        (* do not handle exceptions *)
        Val.empty
    | Closure f -> 
        failwith "C does not support anonymous functions"
    | Const c -> 
        (* only handle string and integer constants *)
        (match c with
        | Cint s -> 
            let loc = IntLit.to_big_int s |> Loc.mk_const_of_z in
            Val.singleton (loc, Cst.cst_true)
        | Cfun fn ->
            Val.singleton (Loc.mk_fun_pointer fn, Cst.cst_true)
        | Cstr s -> 
            let loc = Loc.mk_const_of_string s in
            Val.singleton (loc, Cst.cst_true)
        | Cfloat _ -> 
            Val.empty
        | Cclass _ -> 
            Val.empty)
    | Cast (typ, e) ->
        (* TODO: need type casting? *)
        exec_expr scope location heap e
    | Lvar pvar -> (* Location of a variable *)
        let loc = Loc.of_pvar ~proc:scope pvar in
        Val.singleton (loc, Cst.cst_true)
    | Lfield (e, fn_tn, typ) -> (* Location of a field *)
        let field = Typ.Fieldname.to_string fn_tn |> Loc.mk_const_of_string in
        let obj_addr_v = exec_expr scope location heap e in
        let obj_v = Val.fold
          (fun (obj_addr, obj_addr_cst) v -> Helper.(((Heap.find obj_addr heap) ^ obj_addr_cst) + v))
          obj_addr_v Val.empty
        in
        Val.fold (fun (obj_loc, obj_cst) vals -> 
          if not (Loc.is_const obj_loc) then
            Val.add (Loc.mk_offset obj_loc field, obj_cst) vals
          else vals)
        obj_v Val.empty
        (*Val.map (fun (obj_loc, obj_cst) -> Loc.mk_offset obj_loc field, obj_cst) obj_v*)
    | Lindex (e1, e2) -> (* &(e1[e2]) *)
        let index_v = exec_expr scope location heap e2 in (* value of e2 *)
        let arr_v = exec_expr scope location heap e1 in (* address of e1 *)
        let arr_index_pair = Helper.(arr_v * index_v) in (* all the pairs of e1 val and e2 val *)
        Caml.List.fold_right
          (fun ((arr_loc, arr_cst), (index_loc, index_cst)) v -> (* loc of e1[e2] *)
            Helper.(v + Val.singleton ((Loc.mk_offset arr_loc index_loc), Cst.cst_and arr_cst index_cst)))
          arr_index_pair Val.empty
    | Sizeof data -> 
        (* TODO: Calculate the size of data *)
        Val.singleton (Loc.mk_const_of_z Z.one, Cst.cst_true)
        (* AVS.singleton (Val.of_int (Int.top), Cst.cst_true) *)

    let calc_args tenv scope location heap args =
      let heap', args_rev_v = Caml.List.fold_left
          (fun (h, vlist) ((arg: Exp.t), _) -> (
            match arg with
            | Lvar pvar when Pvar.is_global pvar (*when Pvar.is_compile_constant pvar || Pvar.is_ice pvar*) -> (
                match Pvar.get_initializer_pname pvar with
                | Some callee_pname -> (
                    match Ondemand.get_proc_desc callee_pname with 
                    | Some callee_desc -> ((* no exisiting function: because of functions Infer made *)
                        match get_proc_summary true callee_pname with
                        | Some ({heap=end_heap}) ->
                            let rhs_v = Val.singleton (Loc.of_pvar pvar, Cst.cst_true) in
                            let h' = Heap.union (fun l v1 v2 -> Some (Val.union v1 v2)) heap end_heap in
                            h', rhs_v :: vlist
                        | None ->
                            let () = L.progress "Does not exist summary: %s\n@." (Typ.Procname.to_string callee_pname) in
                            h, Val.empty :: vlist)
                  | None ->
                      let () = L.progress "Does not exist proc descriptor: %s\n@." (Typ.Procname.to_string callee_pname) in
                      h, Val.empty :: vlist)
              | None -> 
                  let () = L.progress "Does not exist proc descriptor: %a\n@." (Pvar.pp Pp.text) pvar in
                  h, Val.empty :: vlist)
            | _ ->
                h, exec_expr scope location heap arg :: vlist))
          (heap, []) args
      in
      let args_v = Caml.List.rev args_rev_v in
      let globs_v = GlobalEnv.get_glob_pvars () 
          |> Caml.List.fold_left (fun vlist (glob, _) -> (Heap.find (Loc.of_pvar glob) heap) :: vlist) []
          |> Caml.List.rev
      in
      heap', globs_v @ args_v

    let print_to_file caller callee merged ienv =
        let to_file pp h n =
            let oc = open_out n in
            let () = Printf.fprintf oc "%s" (Format.asprintf "%a" pp h) in
            close_out oc
        in
        to_file Heap.pp caller "heap_caller"; to_file Heap.pp callee "heap_callee"; to_file Heap.pp merged "heap_merged"; to_file InstEnv.pp ienv "inst_env"

  let exec_instr : Domain.t -> extras ProcData.t -> CFG.Node.t -> Sil.instr -> Domain.t = 
    fun {heap; logs} {pdesc; tenv; extras} node instr ->
      let () = L.progress "%a\n@." PpSumm.pp_inst (node, instr) in
      let proc_name = Typ.Procname.to_string @@ Procdesc.get_proc_name pdesc in
      let scope = VVar.mk_scope proc_name in
      match instr with
      | Load (id, Exp.Lvar pvar, typ, loc) when Pvar.is_global pvar -> (
          let lhs_addr = Loc.of_id ~proc:scope id in
          match Pvar.get_initializer_pname pvar with
          | Some callee_pname -> (
              match get_proc_summary true callee_pname with
              | Some ({heap=end_heap}) ->
                  let heap' = Heap.union (fun l v1 v2 -> Some (Val.union v1 v2)) heap end_heap in
                  let rhs_addr = Loc.of_pvar pvar in
                  let rhs_v = Heap.find rhs_addr heap' in (* TODO: merge global values *)
                  let heap'' = Heap.add lhs_addr (Helper.load rhs_v heap') heap' in
                  mk_domain heap'' logs
              | None ->
                  let () = L.progress "Does not exist summary: %s\n@." (Typ.Procname.to_string callee_pname) in
                  mk_domain heap logs) 
          | None ->
              let () = L.progress "Does not exist initializer: %a\n@."  Loc.pp lhs_addr in
              mk_domain heap logs)

      | Load (id, e1, typ, loc) -> 
          let lhs_addr = Loc.of_id id ~proc:scope in
          let rhs_v = exec_expr scope loc heap e1 |> (fun x -> Helper.load x heap) in
          let heap' = Heap.add lhs_addr rhs_v heap in
          mk_domain heap' logs

      | Store (Lvar pvar, typ, e2, loc) when Pvar.is_return pvar -> (* for return statements *)
          let rhs_v = exec_expr scope loc heap e2 in
          let mname = Typ.Procname.to_string (Procdesc.get_proc_name pdesc) in
          let ret_addr = Loc.mk_ret mname in
          let heap' = Heap.weak_update ret_addr rhs_v heap in
          mk_domain heap' logs

      | Store (e1 , typ, e2, loc) -> 
          let lhs_v = exec_expr scope loc heap e1 in
          let rhs_v = exec_expr scope loc heap e2 in
          let heap' = Helper.store lhs_v rhs_v heap in
          let res = mk_domain heap' logs in
          res
            
      | Prune (e, loc, b, i) -> (* do not support heap pruning *)
          mk_domain heap logs

      | Call ((id, ret_typ), (Const (Cfun callee_pname)), [(e, typ)], loc, flag) when (Typ.Procname.to_string callee_pname) = "__new" -> ((* for dynamic allocations *)
          try
            let data_typ = Exp.texp_to_typ None e in
            let lhs_addr = Loc.of_id id ~proc:scope in
            let ptr = Loc.mk_concrete_pointer ~dyn:true lhs_addr in
            let ptr' = Loc.mk_concrete_pointer ~dyn:true ptr in
            let heap' = Heap.add lhs_addr (Val.singleton (ptr, Cst.cst_true)) heap in
            let heap'' = Heap.add ptr (Val.singleton (ptr', Cst.cst_true)) heap' in
            mk_domain heap'' logs
          with _ ->
            mk_domain heap logs)

      | Call ((id, ret_typ), (Const (Cfun callee_pname)), args, loc, flag) when JniModel.is_jni callee_pname -> (* for jni function calls *)
          let lhs_addr = Loc.of_id id ~proc:scope in
          let ret_addr = Loc.mk_implicit ((Location.to_string loc) ^ ":ret") in
          let ret_addr_ptr = Loc.mk_concrete_pointer ret_addr in
          let heap' = Heap.add lhs_addr (Val.singleton (ret_addr, Cst.cst_true)) heap
            |> Heap.add ret_addr (Val.singleton (ret_addr_ptr, Cst.cst_true)) 
          in
          let jnifun = JNIFun.of_procname callee_pname in
          let arg_index = ref (Caml.List.length args) in
          let dumped_heap, arg_addrs = Caml.List.fold_right 
            (fun (arg_expr, _) (dumped_heap, arg_addrs) ->
              let () = (arg_index := !arg_index - 1) in
              let arg_name = (Location.to_string loc) ^ ":arg" ^ (string_of_int !arg_index) in
              let arg_addr = Loc.mk_implicit arg_name in
              let arg_v = exec_expr scope loc dumped_heap arg_expr in
              (Heap.add arg_addr arg_v dumped_heap, arg_addr :: arg_addrs))
            args (heap', [])
          in
          let cs = CallSite.mk proc_name loc.Location.line loc.Location.col in
          let log = LogUnit.mk [cs] ret_addr jnifun arg_addrs dumped_heap in
          let logs' = CallLogs.add log logs in  
          mk_domain heap' logs'

      | Call ((id, ret_typ), (Const (Cfun callee_pname)), args, loc, flag) -> 
          let lhs_addr = Loc.of_id ~proc:scope id in
          (match Ondemand.get_proc_desc callee_pname with 
          | Some callee_desc -> (* no exisiting function: because of functions Infer made *)
              (match get_proc_summary true ~caller:pdesc callee_pname with
              | Some ({ heap = end_heap; logs = end_logs }) ->
                let heap' = Heap.optimize ~scope heap in
                let heap'', args_v = calc_args tenv scope loc heap' args in
                let args_v' = (* Ignore variadic arguments *)
                  let callee_attr = Procdesc.get_attributes callee_desc in
                  if callee_attr.ProcAttributes.is_variadic then
                    let arg_len = (Caml.List.length callee_attr.ProcAttributes.formals) in
                    let rec sublist l i =
                      if i = arg_len then
                        []
                      else (
                        match l with
                        | [] ->
                            failwith "The number of arguments is less then formals."
                        | h :: t ->
                            h :: (sublist t (i + 1))
                      )
                    in
                    sublist args_v 0
                  else
                    args_v
                in
                let ienv = Instantiation.mk_ienv tenv scope (fun_params callee_desc) args_v' end_heap heap'' in
                let () = L.progress "# Start composition.\n@." in
                let start_gettimeofday = Unix.gettimeofday () in
                let heap''' = Instantiation.comp_heap heap'' heap'' end_heap ienv in
                (*let () = print_to_file heap'' end_heap heap''' ienv in
                let () = Caml.List.iter (fun arg_v -> L.progress "ARG: %a\n@." Val.pp arg_v) args_v' in 
                let () = L.progress "#Instantiation: HeapSize Changed ((%d + %d) -> %d)\n@." (Heap.size heap'') (Heap.size end_heap) (Heap.size heap''') in
                let _ = read_line () in*)
                let cs = CallSite.mk proc_name loc.Location.line loc.Location.col in
                let logs' = Instantiation.comp_log cs logs end_logs heap''' ienv in
                let end_gettimeofday = Unix.gettimeofday () in
                let () = L.progress "\tDone: %f\n@." (end_gettimeofday -. start_gettimeofday) in
                let ret_addr = Loc.mk_ret_of_pname callee_pname in
                let heap'''' = 
                  (match Heap.find_opt ret_addr heap''' with
                  | Some v -> 
                      Heap.add lhs_addr v (Heap.remove ret_addr heap''')
                  | None -> (* kind of passing parameter as a return value *)
                      let ret_param_addr = VVar.of_string "__return_param" ~proc:scope |> Loc.mk_explicit in
                      (match Heap.find_opt ret_param_addr heap''' with
                      | Some v ->
                          Heap.add lhs_addr v (Heap.remove ret_param_addr heap''')
                      | None ->
                          heap''' ))
                in
                mk_domain heap'''' logs'
              | None -> 
                  let () = L.progress "Not existing callee. Just ignore this call.\n@." in
                  mk_domain heap logs
                  )
          | None -> 
              let () = L.progress "Not existing callee (empty declaration). Just ignore this call.\n@." in
              mk_domain heap logs)

      | Call ((id, ret_typ), e, args, loc, flag) -> 
          let () = L.progress "FUN_EXPR: %a\n@." Val.pp (exec_expr scope loc heap e) in
          mk_domain heap logs

      | Call _ ->
          let () = L.progress "Not support function pointers\n@." in
          mk_domain heap logs

      | Nullify (pid, loc) -> 
          mk_domain heap logs

      | Abstract loc -> 
          mk_domain heap logs

      | ExitScope (id_list, loc) -> 
          let heap' =Caml.List.fold_left (fun heap id ->
            (match Var.get_ident id with
            | Some ident -> 
                Heap.remove (Loc.of_id ident) heap
            | None ->
                (match Var.get_pvar id with
                | Some pvar ->
                    Heap.remove (Loc.of_pvar pvar) heap
                | None ->
                    heap))) heap id_list
          in
          mk_domain heap' logs
    

  let pp_session_name _node fmt = F.pp_print_string fmt "C/C++ semantic summary analysis" 
end

module Analyzer = AbstractInterpreter.MakeWTO (TransferFunctions)

(* module Initializer = struct ... end was moved to 'initializer.ml' *)
let checker {Callbacks.proc_desc; tenv; summary} : Summary.t =
    let proc_name = Procdesc.get_proc_name proc_desc in
    if AnalysisTargets.is_targeted proc_name then (
        let () = L.progress "Analyzing a function %s\n@." (Typ.Procname.to_string proc_name) in
        let () = L.progress "ATTRIBUTE:\n%a\n@." ProcAttributes.pp (Procdesc.get_attributes proc_desc) in
        let heap = Initializer.init tenv proc_desc in
        let before_astate = SemanticSummaryDomain.make heap SemanticSummaryDomain.CallLogs.empty in
        let proc_data = ProcData.make_default proc_desc tenv in (
        match Analyzer.compute_post proc_data ~initial:before_astate with
        | Some p -> 
          let opt_astate = Domain.optimize p ~scope:(VVar.mk_scope (Typ.Procname.to_string proc_name)) ~rm_tmp: true in
          (*L.progress "Final in %s: %a\n@." (Typ.Procname.to_string proc_name) SemanticSummaryDomain.pp opt_astate;
          L.progress "Logs: %a\n@." CallLogs.pp opt_astate.logs;*)
          let session = incr summary.Summary.sessions ; !(summary.Summary.sessions) in
          {summary with Summary.payloads = { summary.Summary.payloads with Payloads.semantic_summary = Some opt_astate}; Summary.proc_desc = proc_desc; Summary.sessions = ref session}
        | None -> 
            summary)
    )
    else 
      summary
