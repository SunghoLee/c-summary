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
    match bindings map |> Caml.List.find_opt (fun (typ', v) -> Typ.equal_ignore_quals typ typ') with
    | Some (_, v) ->
        Some v
    | None ->
        None

  let find ?default typ map =
    match default with
    | Some df -> (
        match find_opt typ map with
        | Some v ->
            v
        | None ->
            df)
    | None ->
        find typ map

  let pp = pp ~pp_value: LocSet.pp
end

module CFG = ProcCfg.NormalOneInstrPerNode

module TransferFunctions = struct
  module CFG = CFG
  module Helper = HelperFunction
  module Domain = SemanticSummaryDomain.Domain
  module Model = SemanticModels
  open SemanticSummaryDomain

  type extras = ProcData.no_extras

  let opt_heap_every_stmt = false

  let mk_domain heap logs = 
    if opt_heap_every_stmt then
      Domain.make (Heap.opt_cst_in_heap heap) logs
    else
      Domain.make heap logs

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
            let loc = IntLit.to_int_exn s |> Loc.mk_const_of_int in
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
        Val.map (fun (obj_loc, obj_cst) -> Loc.mk_offset obj_loc field, obj_cst) obj_v
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
        Val.singleton (Loc.mk_const_of_int 1, Cst.cst_true)
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
                        match get_proc_summary callee_pname with
                        | Some ({heap=end_heap}) ->
                            let rhs_v = Val.singleton (Loc.of_pvar pvar, Cst.cst_true) in
                            let ienv = Instantiation.mk_ienv tenv scope callee_desc [rhs_v] end_heap h in
                            let h' = Instantiation.comp_heap h h end_heap ienv in
                            let () = L.progress "==> %a\n@." Heap.pp h' in
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

  let exec_instr : Domain.t -> extras ProcData.t -> CFG.Node.t -> Sil.instr -> Domain.t = 
    fun {heap; logs} {pdesc; tenv; extras} node instr ->
      let () = L.progress "%a\n@." PpSumm.pp_inst (node, instr) in
      let proc_name = Typ.Procname.to_string @@ Procdesc.get_proc_name pdesc in
      let scope = Var.mk_scope proc_name in
      match instr with
      | Load (id, Exp.Lvar pvar, typ, loc) when Pvar.is_global pvar -> (
          let lhs_addr = Loc.of_id ~proc:scope id in
          match Pvar.get_initializer_pname pvar with
          | Some callee_pname -> (
              match get_proc_summary callee_pname with
              | Some ({heap=end_heap}) ->
                  let rhs_addr = Loc.of_pvar pvar in
                  let rhs_v = Heap.find rhs_addr end_heap in (* TODO: merge global values *)
                  let heap' = Heap.add lhs_addr (Helper.load rhs_v end_heap) heap in
                  mk_domain heap' logs
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
          mk_domain heap' logs
      | Prune (e, loc, b, i) -> (* do not support heap pruning *)
          mk_domain heap logs
      | Call ((id, ret_typ), (Const (Cfun callee_pname)), args, loc, flag) when JniModel.is_jni callee_pname -> (* for jni function calls *)
          let lhs_addr = Loc.of_id id ~proc:scope in
          let ret_addr = Loc.mk_implicit ((Location.to_string loc) ^ ":ret") in
          let ret_addr_ptr = Loc.mk_pointer ret_addr in
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
          let log = LogUnit.mk ret_addr jnifun arg_addrs dumped_heap in
          let logs' = CallLogs.add log logs in  
          mk_domain heap' logs'
      | Call ((id, ret_typ), (Const (Cfun callee_pname)), args, loc, flag) -> 
          let lhs_addr = Loc.of_id ~proc:scope id in
          (match Ondemand.get_proc_desc callee_pname with 
          | Some callee_desc -> (* no exisiting function: because of functions Infer made *)
              (match get_proc_summary ~caller:pdesc callee_pname with
              | Some ({ heap = end_heap; logs = end_logs }) ->
                let heap' = Heap.optimize ~scope heap in
                let heap'', args_v = calc_args tenv scope loc heap' args in
                let ienv = Instantiation.mk_ienv tenv scope callee_desc args_v end_heap heap'' in
                let heap''' = Instantiation.comp_heap heap'' heap'' end_heap ienv in
                let logs' = Instantiation.comp_log logs end_logs heap''' ienv in
                let ret_addr = Loc.mk_ret_of_pname callee_pname in
                let heap'''' = 
                  (match Heap.find_opt ret_addr heap''' with
                  | Some v -> 
                      Heap.add lhs_addr v heap'''
                  | None -> (* kind of passing parameter as a return value *)
                      let ret_param_addr = Var.of_string "__return_param" ~proc:scope |> Loc.mk_explicit in
                      (match Heap.find_opt ret_param_addr heap''' with
                      | Some v ->
                          Heap.add lhs_addr v heap'''
                      | None ->
                          Heap.add lhs_addr (Val.singleton (Loc.mk_implicit (Location.to_string loc), Cst.cst_true)) heap'''))
                in
                mk_domain heap'''' logs'
              | None -> 
                  let () = L.progress "Not existing callee. Just ignore this call.\n@." in
                  mk_domain heap logs
                  )
          | None -> 
              let () = L.progress "Not existing callee. Just ignore this call.\n@." in
              mk_domain heap logs)
      | Call _ ->
          let () = L.progress "Not support function pointers\n@." in
          mk_domain heap logs
      | Nullify (pid, loc) -> 
          mk_domain heap logs
      | Abstract loc -> 
          mk_domain heap logs
      | ExitScope (id_list, loc) -> 
          mk_domain heap logs
    

  let pp_session_name _node fmt = F.pp_print_string fmt "C/C++ semantic summary analysis" 
end

module Analyzer = AbstractInterpreter.MakeWTO (TransferFunctions)

module Initializer = struct
  open SemanticSummaryDomain
    
  module Helper = HelperFunction

  let get_struct typ tenv =
    match Typ.name typ with
    | Some s -> 
      (match Tenv.lookup tenv s with
      | Some s -> 
          s
      | None -> 
          failwith ("The structure cannot be found in a type environment: " ^ (Typ.to_string typ)))
    | _ -> failwith ("this typ is not a struct type: " ^ (Typ.to_string typ))

  let mk_tmap loc_typs tenv tmap =
    let rec f (loc, typ) tmap =
      if JniModel.is_jni_struct typ then tmap
      else 
        let tmap' = TypMap.add typ loc tmap in
        match typ.Typ.desc with
        | Tptr (typ', kind) ->
            f ((Loc.mk_pointer loc), typ') tmap'
        | Tstruct name ->
            Helper.get_fld_and_typs name tenv
            |> Caml.List.fold_left 
                (fun tmap (field, typ) -> f (Loc.mk_offset loc (Loc.mk_const_of_string field), typ) tmap)
                tmap'
        | Tarray {elt; length = Some i} -> (* fixed size arrays *)
            let loc' = Loc.unwrap_ptr loc in (* C allocates array location directly to variable address *)
            let index = (IntLit.to_int_exn i) - 1 in
            let rec mk_array = fun i tmap -> (
              if i = -1 then tmap
              else
                f (Loc.mk_offset loc' (Loc.mk_const_of_int i), (Typ.mk (Tptr (elt, Pk_pointer)))) tmap
                |> mk_array (i - 1))
            in
            mk_array index tmap
        | Tint _ -> (* ignore non-pointer types *)
            tmap
        | Tfloat _ -> (* ignore non-pointer types *)
            tmap
        | Tfun _ -> (* ignore non-pointer types *)
            tmap 
        | Tvoid -> (* ignore non-pointer types *)
            tmap
        | _ ->
            failwith (F.asprintf "not support type: %a: %a." Loc.pp loc (Typ.pp_full Pp.text) typ)
    in
    Caml.List.fold_right f loc_typs tmap

  let init_heap loc_typs tenv heap tmap =
    let is_gt l1 l2 = (Loc.compare l1 l2) = 1 in
    let pos_aliases addr typ tmap = TypMap.find typ tmap ~default:LocSet.empty
      |> LocSet.filter (fun x -> is_gt addr x && (Loc.is_pointer x || Loc.is_offset x))
    in
    let handle_alias addr typ = 
      let v, cst = LocSet.fold 
        ((fun alias (v, cst) ->
          let cst' = Cst.cst_eq alias addr |> Cst.cst_and cst in
          Helper.(v + Val.singleton (alias, cst')), Cst.cst_and cst (Cst.cst_not cst')))
        (pos_aliases addr typ tmap) (Val.empty, Cst.cst_true)
      in
      Val.add (addr, cst) v
    in
    let rec iter_loc heap (addr, typ) = 
      if JniModel.is_jni_struct typ then heap
      else 
        let desc = typ.Typ.desc in
        match desc with
        | Tptr (ptr_typ, kind) ->
            let ptr_loc = Loc.mk_pointer addr in
            let heap' = handle_alias ptr_loc ptr_typ
              |> (fun x -> Heap.add addr x heap)
            in
            iter_loc heap' (ptr_loc, ptr_typ)
        | Tstruct name ->
            Helper.get_fld_and_typs name tenv
            |> Caml.List.fold_left
                (fun heap (field, typ) ->
                  iter_loc heap (Loc.mk_offset addr (Loc.mk_const_of_string field), (Typ.mk (Tptr (typ, Pk_pointer)))))
                heap 
        | Tarray {elt; length = Some i} -> (* fixed size arrays *)
            let loc' = Loc.unwrap_ptr addr in (* C allocates array location directly to variable address *)
            let index = (IntLit.to_int_exn i) - 1 in
            let rec mk_array i heap = 
              if i = -1 then heap
              else
                iter_loc heap (Loc.mk_offset loc' (Loc.mk_const_of_int i), (Typ.mk (Tptr (elt, Pk_pointer)))) 
                |> mk_array (i - 1)
            in
            mk_array index heap
        | _ -> 
            heap
    in
    Caml.List.fold_left iter_loc heap loc_typs

  let init tenv pdesc =
    let scope = Var.mk_scope (Typ.Procname.to_string (Procdesc.get_proc_name pdesc)) in
    let arg_vars = (Caml.List.map
      (fun (arg, typ) -> (Var.of_string (Mangled.to_string arg) ~proc:scope, typ))
      (Procdesc.get_formals pdesc)) 
      @ (GlobalEnv.get_glob_vars ())
    in
    let local_vars = Caml.List.map
      (fun (var: ProcAttributes.var_data) -> Var.of_string (Mangled.to_string var.name) ~proc:scope, var.typ)
      (Procdesc.get_locals pdesc)
    in
    let locs_arg, locs_loc = 
      (fun (var, typ) -> Loc.mk_explicit var, Typ.mk (Tptr (typ, Pk_pointer)))
      |> (fun f -> (Caml.List.map f arg_vars, Caml.List.map f local_vars))
    in
    let tmap = mk_tmap locs_arg tenv TypMap.empty in
    init_heap (locs_arg @ locs_loc) tenv Heap.empty tmap
end

let checker {Callbacks.proc_desc; tenv; summary} : Summary.t =
    let proc_name = Procdesc.get_proc_name proc_desc in
    if not (JniModel.is_jni proc_name) then (
        let () = L.progress "Analyzing a function %s\n@." (Typ.Procname.to_string proc_name) in
        let () = L.progress "ATTRIBUTE:\n%a\n@." ProcAttributes.pp (Procdesc.get_attributes proc_desc) in
        let heap = Initializer.init tenv proc_desc in
        let before_astate = SemanticSummaryDomain.make heap SemanticSummaryDomain.CallLogs.empty in
        let proc_data = ProcData.make_default proc_desc tenv in 
        match Analyzer.compute_post proc_data ~initial:before_astate with
        | Some p -> 
          let opt_astate = Domain.optimize p ~scope:(Var.mk_scope (Typ.Procname.to_string proc_name)) in
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
        | None -> 
            summary
    )
    else 
        (L.progress "Skiping analysis for a JNI function %s\n@." (Typ.Procname.to_string proc_name); summary)
