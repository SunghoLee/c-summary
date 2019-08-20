open! IStd
open Core
module F = Format
module L = Logging

module Domain = PointerPreanalysisDomain

module PpSumm = struct
  let get_inst_type (i: Sil.instr) = 
    match i with
    | Load (id, e, typ, loc) -> 
      Format.asprintf "Load (%a)" (Typ.pp_full Pp.text) typ
    | Store (e1, typ, e2, loc) -> 
        Format.asprintf "Store (%a)" (Typ.pp_full Pp.text) typ
    | Prune _ -> 
        "Prune"
    | Call ((id, typ_id), e, args, loc, _) -> 
        Caml.List.fold_left (fun s (_, typ) -> Format.asprintf "%s (%a)" s (Typ.pp_full Pp.text) typ) 
        (Format.asprintf "Call %a = " (Typ.pp_full Pp.text) typ_id)
        args
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

module AliasReporter = struct
  module ProcResMap = PrettyPrintable.MakePPMap(Typ.Procname) 

  let alias_dat = "alias.dat"

  let load_aliases () = 
    try
      let ic = Pervasives.open_in alias_dat in
      let res = Marshal.from_channel ic in
      Pervasives.close_in ic; res
    with _ ->
      ProcResMap.empty
    
  let store_aliases proc_name res = 
    let aliases = load_aliases () in
    let aliases' = ProcResMap.add proc_name res aliases in
    let oc = Pervasives.open_out alias_dat in
    let () = Marshal.to_channel oc aliases' [] in
    Pervasives.close_out oc 
    (*try
    Pervasives.close_out oc
    with _ ->
      let () = L.progress "====\n%a\n====" (ProcResMap.pp ~pp_value: Domain.pp) aliases' in
      let () = Pervasives.close_out oc in
      failwith "funtional value?"*)
end

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

  let is_targeted proc_name = 
    if JniModel.is_jni proc_name then
      false
    else if JniModel.is_callable_from_java proc_name then
      true
    else 
      let res = load_targets () in
      Targets.mem proc_name res

  let add_target proc_name = 
    store_target proc_name 
end

let get_struct typ tenv =
  match Typ.name typ with
  | Some s -> 
    (match Tenv.lookup tenv s with
    | Some s -> 
        s
    | None -> 
        failwith ("The structure cannot be found in a type environment: " ^ (Typ.to_string typ)))
  | _ -> failwith ("this typ is not a struct type: " ^ (Typ.to_string typ))


module CFG = ProcCfg.NormalOneInstrPerNode

module TransferFunctions = struct
  module CFG = CFG
  module Domain = Domain
  module PKFactory = Domain.PointerKey

  type extras = ProcData.no_extras

  let fun_params pdesc =
      let attrs = Procdesc.get_attributes pdesc in
      let scope = Domain.VVar.mk_scope (Typ.Procname.to_string (Procdesc.get_proc_name pdesc)) in
      let args = attrs.formals in
      Caml.List.map (fun (m, typ) -> ((Domain.VVar.of_string (Mangled.to_string m) ~proc:scope), typ)) args

  let rec get_proc_summary ?caller do_clear callee_name = 
    Ondemand.clear_cache ();
    (*let () = L.progress "Request summary of %s\n@." (Typ.Procname.to_string callee_name) in*)
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
        match s.Summary.payloads.Payloads.pointer_preanalysis with
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

  let is_primitive (typ: Typ.t) = 
    match typ.desc with
    | Tint _ | Tfloat _ | Tvoid _ | Tfun _ | TVar _ -> true
    | _ -> false

  let match_param_args tenv ctxt params args m =
    let module TypSet = PrettyPrintable.MakePPSet(struct include Typ let pp = pp_full Pp.text end) in
    let rec update visited param arg (typ: Typ.t) m =
      if TypSet.mem typ visited then
        m
      else 
        let visited' = TypSet.add typ visited in
        match typ.desc with
        | Tptr (base_typ, _) ->
            let param_pk = PKFactory.mk_pk_w_ctxt [ ctxt ] param in
            let arg_lpk = PKFactory.mk_lpk_w_ctxt [ ctxt ] arg in
            let m' = Domain.eq param_pk arg_lpk m in
            update visited' (Domain.Loc.mk_var_pointer param) (Domain.Loc.mk_var_pointer arg) base_typ m'
        | Tstruct name ->
            let param_pk = PKFactory.mk_pk_w_ctxt [ ctxt ] param in
            let arg_lpk = PKFactory.mk_lpk_w_ctxt [ ctxt ] arg in
            let m' = Domain.eq param_pk arg_lpk m in
            (try 
              HelperFunction.get_fld_and_typs name tenv
              |> Caml.List.fold_left 
                  (fun m (field, typ) -> 
                    if is_primitive typ then
                      m
                    else
                      update visited' (Domain.Loc.mk_offset param (Domain.Loc.mk_const_of_string field)) (Domain.Loc.mk_var_pointer (Domain.Loc.mk_offset arg (Domain.Loc.mk_const_of_string field))) typ  m)
                  m'
            with _ ->
              m)
        | Tint _ | Tfloat _ | Tfun _ | TVar _ | Tarray _ | Tvoid -> 
            m
        | _ -> failwith (F.asprintf "Match failure: %a" (Typ.pp_full Pp.text) typ)
    in
    Caml.List.fold_left2 (fun m (p, typ) a -> update TypSet.empty p a typ m) m params args

  let partial_list lst n =
    let rec impl lst i =
      if i > n then []
      else (Caml.List.hd lst) :: (impl (Caml.List.tl lst) (i + 1))
    in
    impl lst 1

  let rec exec_expr scope (expr: Exp.t) = 
    match expr with
    | Var i -> 
        Domain.Loc.mk_var_pointer (Domain.Loc.of_id i ~proc:scope)
    | UnOp (op, e, typ) ->
        Domain.Loc.primitive
    | BinOp (op, e1, e2) -> 
        Domain.Loc.primitive
    | Exn typ ->
        Domain.Loc.primitive
    | Closure f -> 
        failwith "C does not support anonymous functions"
    | Const c -> 
        Domain.Loc.primitive
    | Cast (typ, e) ->
        exec_expr scope e
    | Lvar pvar -> 
        Domain.Loc.of_pvar ~proc:scope pvar
    | Lfield (e, fn_tn, typ) -> (* Location of a field *)
        let field = Typ.Fieldname.to_string fn_tn |> Domain.Loc.mk_const_of_string in
        let obj_addr = exec_expr scope e in
        let obj = Domain.Loc.mk_var_pointer obj_addr in
        Domain.Loc.mk_offset obj field
    | Lindex (e1, e2) -> (* &(e1[e2]) *)
        (* not support *)
        Domain.Loc.primitive
    | Sizeof data -> 
        (* not support *)
        Domain.Loc.primitive

  let exec_instr : Domain.t -> extras ProcData.t -> CFG.Node.t -> Sil.instr -> Domain.t = 
    fun m {pdesc; tenv; extras} node instr ->
      (*let () = L.progress "%a\n@." PpSumm.pp_inst (node, instr) in*)
      let proc_name = Typ.Procname.to_string @@ Procdesc.get_proc_name pdesc in
      let scope = Domain.VVar.mk_scope proc_name in
      match instr with
      | Load (id, e, typ, loc) -> 
          if is_primitive typ then
            m
          else
            let lhs_addr = Domain.Loc.of_id ~proc:scope id |> PKFactory.mk_pk in
            let rhs_addr = exec_expr scope e |> PKFactory.mk_pk in
            Domain.eq lhs_addr rhs_addr m

      | Store (Lvar pvar, typ, e2, loc) when Pvar.is_return pvar -> (* for return statements *)
          if is_primitive typ then
            m
          else
            let ret_addr = Typ.Procname.to_string (Procdesc.get_proc_name pdesc) |> Domain.Loc.mk_ret |> PKFactory.mk_pk in
            let rhs_addr = exec_expr scope e2 |> PKFactory.mk_lpk in
            Domain.eq ret_addr rhs_addr m

      | Store (e1 , typ, e2, loc) -> 
          if is_primitive typ then
            m
          else
            let lhs_addr = exec_expr scope e1 |> PKFactory.mk_pk in
            let rhs_addr = exec_expr scope e2 |> PKFactory.mk_lpk in
            (*let () = L.progress "#RHS: %a\n@." PKFactory.pp rhs_addr in*)
            Domain.eq lhs_addr rhs_addr m
            
      | Prune (e, loc, b, i) -> (* do not support heap pruning *)
          m

      | Call ((id, ret_typ), (Const (Cfun callee_pname)), [(e, typ)], loc, flag) when (Typ.Procname.to_string callee_pname) = "__new" -> ((* for dynamic allocations *)
          let lhs_addr = Domain.Loc.of_id id ~proc:scope |> PKFactory.mk_pk in
          let rhs_addr = Domain.Loc.mk_implicit (Location.to_string loc) |> PKFactory.mk_pk in
          Domain.eq lhs_addr rhs_addr m
      )

      | Call ((id, ret_typ), (Const (Cfun callee_pname)), args, loc, flag) when JniModel.is_jni callee_pname -> (* for jni function calls *)
          let lhs_addr = Domain.Loc.of_id id ~proc:scope |> PKFactory.mk_pk in
          let rhs_addr = Domain.Loc.mk_implicit (Location.to_string loc) |> PKFactory.mk_pk in
          Domain.eq lhs_addr rhs_addr m

      | Call ((id, ret_typ), (Const (Cfun callee_pname)), args, loc, flag) -> 
          let lhs_addr = Domain.Loc.of_id ~proc:scope id |> PKFactory.mk_pk in
          (match Ondemand.get_proc_desc callee_pname with 
          | Some callee_desc -> (* no exisiting function: because of functions Infer made *)
              (*let _ = L.progress "ARG: %d, PARAM: %d\n@." (Caml.List.length args_v') (Caml.List.length params) in*)
              if (Caml.List.length (fun_params callee_desc)) = (Caml.List.length args) then
                (match get_proc_summary true ~caller:pdesc callee_pname with
                | Some callee_m ->
                    let args_v = 
                      Caml.List.map (fun (arg, typ) -> arg) args |> Caml.List.map (exec_expr scope) in
                    let args_v' = (* Ignore variadic arguments *)
                      let callee_attr = Procdesc.get_attributes callee_desc in
                      if callee_attr.ProcAttributes.is_variadic then
                        let arg_len = (Caml.List.length callee_attr.ProcAttributes.formals) in
                        partial_list args_v arg_len
                      else
                        args_v
                    in
                    let cs = CallSite.make (Procdesc.get_proc_name pdesc) loc in
                    let ctxt = Domain.mk_ctxt cs args_v' in
                    let callee_m' = Domain.assign_context ctxt callee_m in
                    let m' = Domain.join m callee_m' in
                    let params = Caml.List.map (fun (param, typ) -> (Domain.Loc.mk_explicit param, typ)) (fun_params callee_desc) in
                    let m'' = match_param_args tenv ctxt params args_v' m' in
                    let ret_addr = Domain.Loc.mk_ret_of_pname callee_pname |> PKFactory.mk_pk in
                    let ret_param_addr = Domain.VVar.of_string "__return_param" ~proc:scope |> Domain.Loc.mk_explicit |> PKFactory.mk_pk in
                    Domain.eq lhs_addr ret_addr m'' |> Domain.eq lhs_addr ret_param_addr
                | None -> 
                    (*let () = L.progress "Not existing callee. Just ignore this call.\n@." in*)
                    m
                    )
              else
                m
          | None -> 
              (*let () = L.progress "Not existing callee (empty declaration). Just ignore this call.\n@." in*)
              m)

      | Call _ | Nullify _ | Abstract _ | ExitScope _ ->
          m    
      | _ -> 
          failwith (F.asprintf "check on this instruction semantics: %a" PpSumm.pp_inst (node, instr))

  let pp_session_name _node fmt = F.pp_print_string fmt "Steensgaard style pointer analysis for C/C++" 
end

module Analyzer = AbstractInterpreter.MakeWTO (TransferFunctions)

let checker {Callbacks.proc_desc; tenv; summary} : Summary.t =
    let proc_name = Procdesc.get_proc_name proc_desc in
    let proc_data = ProcData.make_default proc_desc tenv in 
    if AnalysisTargets.is_targeted proc_name then (
    (*let () = L.progress "Analyzing a function %s\n@." (Typ.Procname.to_string proc_name) in*)
    (*let () = L.progress "ATTRIBUTE:\n%a\n@." ProcAttributes.pp (Procdesc.get_attributes proc_desc) in*)
    match Analyzer.compute_post proc_data ~initial:PointerPreanalysisDomain.empty with
    | Some p -> 
        (*if not (JniModel.is_jni proc_name) && JniModel.is_callable_from_java proc_name then
        (* TODO: record the result *)*)
        (*let () = L.progress "Final in %s: %a\n@." (Typ.Procname.to_string proc_name) Domain.pp (Domain.root_lift p) in*)
        let session = incr summary.Summary.sessions ; !(summary.Summary.sessions) in
        (if JniModel.is_callable_from_java proc_name then
          AliasReporter.store_aliases proc_name p);
        {summary with Summary.payloads = { summary.Summary.payloads with Payloads.pointer_preanalysis = Some p}; Summary.proc_desc = proc_desc; Summary.sessions = ref session}
    | None -> 
        summary)
    else
      summary
