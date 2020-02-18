open! IStd
open Core
module F = Format
module L = Logging
module Sem = SemanticFunctions
module TH = TypeHandler
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
    let rec pp_lst fmt = function
      | [] -> ()
      | x :: xs -> F.fprintf fmt " %a; %a" Procdesc.Node.pp_id (Procdesc.Node.get_id x) pp_lst xs in
    F.fprintf fmt "%a: [%s] %a\n" Procdesc.Node.pp_id node_id inst_type (Sil.pp_instr ~print_types: true Pp.text) inst;
    F.fprintf fmt "  pred [%a]\n" pp_lst (Procdesc.Node.get_preds node);
    F.fprintf fmt "  succ [%a]" pp_lst (Procdesc.Node.get_succs node)
end

(* module TypeMap = struct ... end was moved to 'initializer.ml' *)

let debug = ref false 

module CFG = ProcCfg.NormalOneInstrPerNode

module TransferFunctions = struct
  module CFG = CFG
  module Helper = HelperFunction
  module Domain = SemanticSummaryDomain.Domain
  module Model = SemanticModels
  module GH = GlobalHandler

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

  let get_global_pvar_and_typ e = 
    let glob_pvar = GH.get_glob_pvar e in
    let all_globals = GlobalEnv.internal_get_glob_pvars () in
    try Caml.List.find (fun (pvar', typ) -> pvar' = glob_pvar) all_globals with _ ->
      Caml.List.find (fun (pvar', typ) -> Mangled.equal (Pvar.get_name pvar') (Pvar.get_name glob_pvar)) all_globals

  let get_global_pvar_and_typ_opt e =
    try Some (get_global_pvar_and_typ e) with _ -> None
    (*match GH.get_glob_pvar_opt e with
    | None -> None
    | Some glob_pvar ->
      let all_globals = GlobalEnv.internal_get_glob_pvars () in
      Caml.List.find_opt (fun (pvar', typ) -> pvar' = glob_pvar) all_globals*)

  let mk_domain heap logs = 
    (*let () = L.progress "ResHeap: %a\n@." Heap.pp heap in*)
    if opt_heap_every_stmt then
      Domain.make (Heap.opt_cst_in_heap heap) logs
    else
      Domain.make heap logs

  let get_proc_summary ?caller do_clear callee_name = 
    let sum = 
      match caller with
      | Some s -> (
          Ondemand.analyze_proc_name ~caller_pdesc:s callee_name)
      | None -> (
          Ondemand.analyze_proc_name callee_name)
    in
    match sum with
    | Some s -> 
        s.Summary.payloads.Payloads.semantic_summary
    | None -> 
        None

  let rec exec_expr scope location heap (expr: Exp.t) = 
      match expr with
      | Var i -> 
          let loc = Loc.of_id i ~proc:scope in
          Heap.find loc heap
      | UnOp (op, e, typ) ->
          Val.empty
          (*let loc = Loc.mk_implicit (Location.to_string location) in
          Val.singleton (loc, Cst.cst_true)*)
      | BinOp (op, e1, e2) -> 
          Val.empty
          (*let loc = Loc.mk_implicit (Location.to_string location) in
          Val.singleton (loc, Cst.cst_true)*)
      | Exn typ ->
          (* do not handle exceptions *)
          Val.empty
      | Closure f -> 
          failwith "C does not support anonymous functions"
      | Const c -> 
          (* only handle string and integer constants *)
          (match c with
          | Cint s -> 
              Val.empty
              (*let loc = IntLit.to_big_int s |> Loc.mk_const_of_z in
              Val.singleton (loc, Cst.cst_true)*)
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
          let field_str = Typ.Fieldname.to_string fn_tn in
          let field = Loc.to_const_typ_of_string field_str in
          let obj_addr_v = exec_expr scope location heap e in
          let obj_v = Helper.load obj_addr_v heap in
          Val.map (fun (l, cst) -> if Loc.is_jni_env l then (Typ.Procname.from_string_c_fun ("_JNIEnv_" ^ field_str) |> Loc.mk_fun_pointer, cst) else Loc.mk_offset l field, cst) obj_v
      | Lindex (e1, Const (Cint s)) -> (* &(e1[e2]) *)
          let index = IntLit.to_big_int s |> Loc.mk_const_of_z in
          let arr_addr_v = exec_expr scope location heap e1 in (* address of e1 *)
          let arr_v = Helper.load arr_addr_v heap in
          Val.map (fun (l, cst) -> Loc.mk_offset l (Loc.strip_const index), cst) arr_v
      | Lindex (e1, _) -> (* &(e1[e2]) *)
          let arr_addr_v = exec_expr scope location heap e1 in (* address of e1 *)
          let arr_v = Helper.load arr_addr_v heap in
          Val.map (fun (l, cst) -> Loc.mk_offset l Loc.const_typ_top, cst) arr_v
      | Sizeof data -> 
          Val.empty
          (* TODO: Calculate the size of data *)
          (*Val.singleton (Loc.mk_const_of_z Z.one, Cst.cst_true)*)
          (* AVS.singleton (Val.of_int (Int.top), Cst.cst_true) *)

    let calc_args tenv scope location heap args =
      let heap', args_rev_v = Caml.List.fold_left
          (fun (h, vlist) ((arg: Exp.t), _) -> (
            match arg with
            | Lvar pvar when Pvar.is_global pvar (*when Pvar.is_compile_constant pvar || Pvar.is_ice pvar*) -> (
                match get_global_pvar_and_typ_opt arg with
                | Some (glob_pvar, glob_typ) -> 
                    let heap' = GH.inject_dummy_mappings tenv glob_pvar glob_typ heap in
                    let glob_addr = exec_expr scope location heap' arg in
                    heap', (glob_addr :: vlist)
                | None -> 
                    heap, (exec_expr scope location heap arg) :: vlist)

                    (*
                    let (glob_pvar, glob_typ) = get_global_pvar_and_typ arg in
                    let heap' = GH.inject_dummy_mappings tenv glob_pvar glob_typ heap in
                    let glob_addr = exec_expr scope location heap' arg in
                    heap', (glob_addr :: vlist))*)
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
    fun {heap; logs; graph} {pdesc; tenv; extras} node instr ->
      (*let () = L.progress "%a\n@." PpSumm.pp_inst (node, instr) in*)
      let proc_name = Typ.Procname.to_string @@ Procdesc.get_proc_name pdesc in
      (*let () = match node with 
        | (n, i) -> L.progress "%s:%a\n@." proc_name Procdesc.Node.pp_id (Procdesc.Node.get_id n) in*)
      let scope = VVar.mk_scope proc_name in
      (* vvv  Append node into graph  vvv *)
      let cfg_node =
        let zero = IntLit.of_int 0 in
        let rec try_parse_exp = function
          | Exp.Var ident ->
              let ex_loc = Loc.of_id ident ~proc:scope in
              (*let () = L.progress "  -- Exp.Var %a\n@." Ident.pp ident in*)
              (*let () = L.progress "  --     Loc %a\n@." Loc.pp ex_loc in*)
              (match Heap.find_opt ex_loc heap with
                | None -> ControlFlowGraph.Exp.Unknown
                | Some x -> match Val.elements x with
                  | (Loc.Implicit l, c) :: xs ->
                      ControlFlowGraph.Exp.IsTrue l
                  | _ -> ControlFlowGraph.Exp.Unknown)
          | Exp.UnOp (LNot, e, _) ->
              try_parse_exp e |> ControlFlowGraph.Exp.neg
          | Exp.BinOp (Eq, e, Exp.Const zero) ->
              try_parse_exp e |> ControlFlowGraph.Exp.neg
          | Exp.BinOp (Eq, Exp.Const zero, e) ->
              try_parse_exp e |> ControlFlowGraph.Exp.neg
          | Exp.BinOp (Ne, e, Exp.Const zero) ->
              try_parse_exp e
          | Exp.BinOp (Ne, Exp.Const zero, e) ->
              try_parse_exp e
          | Exp.Lvar pvar ->
              (*let () = L.progress "  -- Exp.PVar %a\n@." Pvar.pp_value pvar in*)
              ControlFlowGraph.Exp.Unknown
          | _ -> ControlFlowGraph.Exp.Unknown in
        let node = match node with (n, i) -> n in
        let kind = match instr with
          | Prune (exp, _, true, _) ->
              ControlFlowGraph.Node.KPruneT (try_parse_exp exp)
          | Prune (exp, _, false, _) ->
              ControlFlowGraph.Node.KPruneF (try_parse_exp exp)
          | ExitScope _ -> ControlFlowGraph.Node.KEnd
          | Call _ -> ControlFlowGraph.Node.KCall
          | _ -> ControlFlowGraph.Node.KCommon in
        let id = Procdesc.Node.get_id node in
        let idx = ControlFlowGraph.Graph.alloc_idx graph in
        let loc = ControlFlowGraph.NodeLoc.mk [proc_name, id, idx] in
        let mk_loc_list = List.fold ~init: [] ~f: (fun l n ->
            let loc = ControlFlowGraph.NodeLoc.mk
                [proc_name, Procdesc.Node.get_id n, -1] in
            loc :: l ) in
        let succ_list = Procdesc.Node.get_succs node |> mk_loc_list in
        let pred_list = Procdesc.Node.get_preds node |> mk_loc_list in
        ControlFlowGraph.Graph.add_node kind loc succ_list pred_list graph
      in
      (* ^^^  ----------------------  ^^^ *)
      match instr with
      | Load (id, e1, typ, loc) when TH.is_not_allowed typ -> ( (* Handling load for global variables *)
          mk_domain heap logs graph
      )
      | Load (id, e1, typ, loc) when GH.is_global e1 -> ( (* Handling load for global variables *)
          let lhs_addr = Loc.of_id id ~proc:scope in
          (match get_global_pvar_and_typ_opt e1 with
          | Some (glob_pvar, glob_typ) -> 
              let heap' = GH.inject_dummy_mappings tenv glob_pvar glob_typ heap in
              let glob_v = exec_expr scope loc heap' e1 |> (fun x -> Helper.load x heap') in
              let heap'' = Heap.add lhs_addr glob_v heap' in
              mk_domain heap'' logs graph
          | None -> 
              let rhs_v = exec_expr scope loc heap e1 |> (fun x -> Helper.load x heap) in
              let heap' = Heap.add lhs_addr rhs_v heap in
              mk_domain heap' logs graph)
              
      (*
          let (glob_pvar, glob_typ) = get_global_pvar_and_typ e1 in
          let heap' = GH.inject_dummy_mappings tenv glob_pvar glob_typ heap in
          let glob_v, heap'' = exec_expr scope loc heap' e1 |> (fun x -> Helper.load x heap') in
          let heap''' = Heap.add lhs_addr glob_v heap'' in
          mk_domain heap''' logs graph *)
      )

      | Load (id, e1, typ, loc) when JniModel.is_jni_env_ptr_for_c typ -> 
          let lhs_addr = Loc.of_id id ~proc:scope in
          let lhs_ptr_addr = Loc.mk_concrete_pointer lhs_addr in
          let lhs_ptr_ptr_addr = Loc.mk_concrete_pointer lhs_ptr_addr in
          let heap' = JniModel.put_jni_env_modeling lhs_ptr_ptr_addr heap in
          let heap'' = Heap.add lhs_addr (Val.singleton (lhs_ptr_addr, Cst.cst_true)) heap' in
          let heap''' = Heap.add lhs_ptr_addr (Val.singleton (lhs_ptr_ptr_addr, Cst.cst_true)) heap'' in
          mk_domain heap''' logs graph

      | Load (id, e1, typ, loc) -> 
          let lhs_addr = Loc.of_id id ~proc:scope in
          let rhs_v = exec_expr scope loc heap e1 |> (fun x -> Helper.load x heap) in
          let heap' = Heap.add lhs_addr rhs_v heap in
          mk_domain heap' logs graph

      | Store (Lvar pvar, typ, e2, loc) when TH.is_not_allowed typ -> (* for return statements *)
          mk_domain heap logs graph

      | Store (Lvar pvar, typ, e2, loc) when Pvar.is_return pvar -> (* for return statements *)
          let rhs_v = exec_expr scope loc heap e2 in
          let mname = Typ.Procname.to_string (Procdesc.get_proc_name pdesc) in
          let ret_addr = Loc.mk_ret mname in
          let heap' = Heap.weak_update ret_addr rhs_v heap in
          mk_domain heap' logs graph

      | Store (e1, typ, e2, loc) when GH.is_global e2 -> ( (* Handling load for global variables *)
          (match get_global_pvar_and_typ_opt e2 with
          | Some (glob_pvar, glob_typ) -> 
              let heap' = GH.inject_dummy_mappings tenv glob_pvar glob_typ heap in
              let lhs_v = exec_expr scope loc heap' e1 in
              let rhs_v = exec_expr scope loc heap' e2 in
              let heap'' = Helper.store lhs_v rhs_v heap' in
              let res = mk_domain heap'' logs graph in
              res
          | None -> 
              let lhs_v = exec_expr scope loc heap e1 in
              let rhs_v = exec_expr scope loc heap e2 in
              let heap' = Helper.store lhs_v rhs_v heap in
              let res = mk_domain heap' logs graph in
              res)
          (*
          let (glob_pvar, glob_typ) = get_global_pvar_and_typ e2 in
          let heap' = GH.inject_dummy_mappings tenv glob_pvar glob_typ heap in
          let lhs_v = exec_expr scope loc heap' e1 in
          let rhs_v = exec_expr scope loc heap' e2 in
          let heap'' = Helper.store lhs_v rhs_v heap' in
          let res = mk_domain heap'' logs graph in
          res
          *)
      )

      | Store (e1, typ, e2, loc) when GH.is_global e1 -> ( (* Handling load for global variables *)
          (match get_global_pvar_and_typ_opt e1 with
          | Some (glob_pvar, glob_typ) -> 
              let heap' = GH.inject_dummy_mappings tenv glob_pvar glob_typ heap in
              let lhs_v = exec_expr scope loc heap' e1 in
              let rhs_v = exec_expr scope loc heap' e2 in
              let heap'' = Helper.store lhs_v rhs_v heap' in
              let res = mk_domain heap'' logs graph in
              res
          | None -> 
              let lhs_v = exec_expr scope loc heap e1 in
              let rhs_v = exec_expr scope loc heap e2 in
              let heap' = Helper.store lhs_v rhs_v heap in
              let res = mk_domain heap' logs graph in
              res)
          (*
          let (glob_pvar, glob_typ) = get_global_pvar_and_typ e1 in
          let heap' = GH.inject_dummy_mappings tenv glob_pvar glob_typ heap in
          let lhs_v = exec_expr scope loc heap' e1 in
          let rhs_v = exec_expr scope loc heap' e2 in
          let heap'' = Helper.store lhs_v rhs_v heap' in
          let res = mk_domain heap'' logs graph in
          res*)
      )

      | Store (e1 , typ, e2, loc) -> 
          let lhs_v = exec_expr scope loc heap e1 in
          let rhs_v = exec_expr scope loc heap e2 in
          let heap' = Helper.store lhs_v rhs_v heap in
          let res = mk_domain heap' logs graph in
          res
            
      | Prune (e, loc, b, i) -> (* do not support heap pruning *)
          mk_domain heap logs graph

      | Call ((id, ret_typ), (Const (Cfun callee_pname)), [(e, typ)], loc, flag) when (Typ.Procname.to_string callee_pname) = "__new" -> ((* for dynamic allocations *)
          try
            let data_typ = Exp.texp_to_typ None e in
            let lhs_addr = Loc.of_id id ~proc:scope in
            let ptr = Loc.mk_concrete_pointer ~dyn:true lhs_addr in
            let ptr' = Loc.mk_concrete_pointer ~dyn:true ptr in
            let heap' = Heap.add lhs_addr (Val.singleton (ptr, Cst.cst_true)) heap in
            let heap'' = Heap.add ptr (Val.singleton (ptr', Cst.cst_true)) heap' in
            mk_domain heap'' logs graph
          with _ ->
            mk_domain heap logs graph) 

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
          let log = LogUnit.mk
              [cs]
              cfg_node.ControlFlowGraph.Node.loc
              ret_addr jnifun arg_addrs dumped_heap in
          let logs' = CallLogs.add log logs in  
          mk_domain heap' logs' graph

      | Call ((id, ret_typ), (Const (Cfun callee_pname)), args, loc, flag) -> 
          let lhs_addr = Loc.of_id ~proc:scope id in
          (match Ondemand.get_proc_desc callee_pname with 
          | Some callee_desc -> (* no exisiting function: because of functions Infer made *)
              if (Caml.List.length (fun_params callee_desc)) = (Caml.List.length args) then
                (match get_proc_summary true ~caller:pdesc callee_pname with
                | Some ({ heap = end_heap; logs = end_logs; graph = end_graph }, gs) ->
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
                  (*let () = L.progress "# Start composition.\n@." in*)
                  let start_gettimeofday = Unix.gettimeofday () in
                  let heap''' = Instantiation.comp_heap heap'' heap'' end_heap ienv in
                  (*let () = print_to_file heap'' end_heap heap''' ienv in
                  let () = Caml.List.iter (fun arg_v -> L.progress "ARG: %a\n@." Val.pp arg_v) args_v' in 
                  let () = L.progress "#Instantiation: HeapSize Changed ((%d + %d) -> %d)\n@." (Heap.size heap'') (Heap.size end_heap) (Heap.size heap''') in
                  let _ = read_line () in*)
                  let cs = CallSite.mk proc_name loc.Location.line loc.Location.col in
                  let logs' = Instantiation.comp_log
                      cs cfg_node.ControlFlowGraph.Node.loc
                      logs end_logs heap''' ienv in
                  let end_gettimeofday = Unix.gettimeofday () in
                  (*let () = L.progress "\tDone: %f\n@." (end_gettimeofday -. start_gettimeofday) in*)
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
                  ControlFlowGraph.Graph.merge graph cfg_node end_graph;
          (*L.progress "%a\n@."
            (ControlFlowGraph.Graph.export_dot "MergedGraph")
            graph;*)
                  mk_domain heap'''' logs' graph
                | None -> 
                    (*let () = L.progress "Not existing callee. Just ignore this call.\n@." in*)
                    mk_domain heap logs graph
                    ) 
              else
                mk_domain heap logs graph
          | None -> 
              (*let () = L.progress "Not existing callee (empty declaration). Just ignore this call.\n@." in*)
              mk_domain heap logs graph)

      | Call ((id, ret_typ), e, args, loc, flag) -> 
          let fval = exec_expr scope loc heap e in
          if Val.exists (fun (l, _) -> Loc.is_jni_fun_pointer l) fval then
            let lhs_addr = Loc.of_id id ~proc:scope in
            let ret_addr = Loc.mk_implicit ((Location.to_string loc) ^ ":ret") in
            let ret_addr_ptr = Loc.mk_concrete_pointer ret_addr in
            let heap' = Heap.add lhs_addr (Val.singleton (ret_addr, Cst.cst_true)) heap
              |> Heap.add ret_addr (Val.singleton (ret_addr_ptr, Cst.cst_true)) 
            in
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
            let logs' = Val.fold (fun (l, _) logs -> 
              if Loc.is_jni_fun_pointer l then
                let jnifun = Loc.get_jni_fun_name_exn l |> JNIFun.of_string in
                let log = LogUnit.mk
                    [cs]
                    cfg_node.ControlFlowGraph.Node.loc
                    ret_addr jnifun arg_addrs dumped_heap in
                CallLogs.add log logs
              else logs) fval logs
            in
            mk_domain heap' logs' graph
          else
            mk_domain heap logs graph
      | Call _ ->
          (*let () = L.progress "Not support function pointers\n@." in*)
          mk_domain heap logs graph

      | Nullify (pid, loc) -> 
          mk_domain heap logs graph

      | Abstract loc -> 
          mk_domain heap logs graph

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
          mk_domain heap' logs graph
    

  let pp_session_name _node fmt = F.pp_print_string fmt "C/C++ semantic summary analysis" 
end

module Analyzer = AbstractInterpreter.MakeWTO (TransferFunctions)


(* module Initializer = struct ... end was moved to 'initializer.ml' *)
let checker {Callbacks.proc_desc; tenv; summary} : Summary.t =
    let proc_name = Procdesc.get_proc_name proc_desc in
    if AnalysisTargets.is_targeted proc_name then (
      (*(if (Typ.Procname.to_string proc_name) = "JavaCPP_getClass" then
        debug := true
      );*)
      (*  let () = L.progress "Analyzing a function %s\n@." (Typ.Procname.to_string proc_name) in*)
        (*let () = L.progress "ATTRIBUTE:\n%a\n@." ProcAttributes.pp (Procdesc.get_attributes proc_desc) in*)
        let heap = Initializer.init tenv proc_desc in
        let graph = ControlFlowGraph.Graph.mk_empty () in
        let before_astate = SemanticSummaryDomain.make heap SemanticSummaryDomain.CallLogs.empty graph in
        let proc_data = ProcData.make_default proc_desc tenv in (
        match Analyzer.compute_post proc_data ~initial:before_astate with
        | Some p -> 
          let opt_astate = Domain.optimize p ~scope:(VVar.mk_scope (Typ.Procname.to_string proc_name)) ~rm_tmp: true in
          let f_heap = opt_astate.Domain.heap in
          let gstore = GlobalHandler.collect_global_store f_heap in
          (*L.progress "GLOBAL_UPDATE: %a\n@." GlobalHandler.GlobalStore.pp gstore; *)
          (*L.progress "Final in %s: %a\n@." (Typ.Procname.to_string proc_name) SemanticSummaryDomain.pp opt_astate;*)
          (*L.progress "Logs: %a\n@." CallLogs.pp opt_astate.logs;*)
          let session = incr summary.Summary.sessions ; !(summary.Summary.sessions) in

          (*L.progress "%a\n@."
            (ControlFlowGraph.Graph.export_dot "BeforeUpdate")
            graph;*)
          ControlFlowGraph.Graph.update_link_locs graph;
          (*L.progress "%a\n@." ControlFlowGraph.Graph.pp graph;*)
          (*L.progress "%a\n@."
            (ControlFlowGraph.Graph.export_dot "FinalGraph")
            graph;*)

          {summary with Summary.payloads = { summary.Summary.payloads with Payloads.semantic_summary = Some (opt_astate, gstore)}; Summary.proc_desc = proc_desc; Summary.sessions = ref session}
        | None -> 
            summary)
    )
    else 
      summary
