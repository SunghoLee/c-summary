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

module NameType = GlobalNameType

module TransferFunctions (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = NameType

  type extras = ProcData.no_extras

  let get_glob_name pvar = 
    if Pvar.is_global pvar then
      let tu = Pvar.get_translation_unit pvar in
      match tu with
      | Some s -> 
          "#GB_" ^ (SourceFile.to_string s) ^ "_" ^ (Pvar.to_string pvar)
      | None -> 
          Pvar.to_string pvar
    else if Pvar.is_local pvar then
      Mangled.to_string (Pvar.get_name pvar)
    else failwith "no other variable types in C"

  let get_inst_type (i: Sil.instr) = 
    match i with
    | Load _ -> "Load"
    | Store _ -> "Store"
    | Prune _ -> "Prune"
    | Call _ -> "Call"
    | Nullify _ -> "Nullify"
    | Abstract _ -> "Abstract"
    | ExitScope _ -> "ExitScope"

  let pp_inst proc_data (i: Sil.instr) = 
    let proc_name_str = Typ.Procname.to_string (Procdesc.get_proc_name proc_data.ProcData.pdesc) in
    match i with
    | Call (_, _, args, _, _) -> 
        let arg_str = Caml.List.fold_left (fun s (e, t) -> s ^ " " ^ (F.asprintf "%a" Exp.pp e) ^ ":" ^ (F.asprintf "%a" (Typ.pp_full Pp.text) t)) "" args in
        L.progress "%s: [%s] %a - %s\n@." proc_name_str (get_inst_type i) (Sil.pp_instr ~print_types:true Pp.text) i arg_str
    | _ ->
        L.progress "%s: [%s] %a\n@." proc_name_str (get_inst_type i) (Sil.pp_instr ~print_types:true Pp.text) i


  let rec handle_arr (arr: Exp.t) (i: Exp.t) typ = 
    match arr,i with
    | Lvar pvar, index -> (
      match index with
      | Const (Cint s) ->
          if Pvar.is_global pvar then
            let typ' = Typ.mk_array ~length:(IntLit.add s IntLit.one) typ in
            Some (pvar, typ')
          else
            None
      | _ ->
          if Pvar.is_global pvar then
            let typ' = Typ.mk_array ~length:(IntLit.zero) typ in
            Some (pvar, typ')
          else
            None
    )

    | Lindex (arr', i'), index  -> (
        match handle_arr arr' i' typ with
        | Some (p, t) -> (
          match index with
          | Const (Cint s) -> 
              Some(p, Typ.mk_array ~length:(IntLit.add s IntLit.one) t)
          | _ ->
              Some(p, Typ.mk_array ~length:(IntLit.zero) t)
        )
        | _ ->
            None
    )

    | _, Const (Cint s) ->
        None

    | _ -> 
        None

  let exec_expr m (expr : Exp.t) t : Domain.t =
      match expr with
      | Var i -> m
      | UnOp (op, e, typ) -> m
      | BinOp (op, e1, e2) -> m
      | Exn typ -> m
      | Closure f -> m
      | Const c -> m
      | Cast (typ, e) -> m
      | Lfield (e, fn, typ) -> m
      | Lindex (e1, e2) -> (
        match handle_arr e1 e2 t with
        | Some (p, t') -> 
            Domain.add p t' m
        | None ->
            m)
      | Sizeof data -> m
      | Lvar pvar -> 
              if Pvar.is_global pvar then
                  Domain.add pvar t m
              else 
                  m

  let handle_global (e: Exp.t) typ astate =
    match e with
    | Lvar pvar -> 
        if Pvar.is_global pvar then
          Domain.add pvar typ astate
        else
          astate
    | Lindex (arr, i) -> (
        match handle_arr arr i typ with
        | Some (p, t) ->
            Domain.add p t astate
        | _ ->
            astate
        )
    | _ -> astate

  let exec_instr astate proc_data node (instr: Sil.instr) =
      (*L.progress "PRE: %s\n@." (Domain.pp_m astate);*)
      (*pp_inst proc_data instr; *)
      let post = (
      match instr with
      | Load (id, e1, typ, loc) -> 
          handle_global e1 typ astate

      | Store (e1, typ, e2, loc) -> 
          let astate' = handle_global e1 typ astate in
          if Typ.is_pointer typ then
            handle_global e2 (Typ.strip_ptr typ) astate'
          else 
            astate'

      (* Calling an initializer function behaves quite differently from normal functions. The argument type is not deterministic. *)
      | Call ((id, typ_e1), (Const (Cfun callee_pname)), args, loc, flag) when String.is_prefix (Typ.Procname.to_string callee_pname) ~prefix:"__variable_initialization" ->
          (*Caml.List.fold_left (fun i (e, t) -> exec_expr i e (Typ.mk (Tptr (t, Pk_pointer)))) astate args*)
          Caml.List.fold_left (fun i (e, t) -> handle_global e t i) astate args

      | Call ((id, typ_e1), (Const (Cfun callee_pname)), args, loc, flag) -> 
          Caml.List.fold_left (fun i (e, t) -> 
            if Typ.is_pointer t then
              handle_global e (Typ.strip_ptr t) i
            else 
              i) astate args

      | _ -> astate
              ) in
    let node_kind = CFG.Node.kind node in
    let pname = Procdesc.get_proc_name proc_data.ProcData.pdesc in
    (*L.progress "POST: %s\n@." (Domain.pp_m post); *)
    post

    let pp_session_name _node fmt = F.pp_print_string fmt "C/C++ semantic summary analysis"
end

module Analyzer = AbstractInterpreter.MakeRPO (TransferFunctions (ProcCfg.Exceptional))

module Procs : sig
    type typ
    val add : Typ.Procname.t -> Procdesc.t -> typ -> typ
    val find : Typ.Procname.t -> typ -> Procdesc.t
    val find_opt : Typ.Procname.t -> typ -> Procdesc.t option
    val empty : typ
    val find_first: (Typ.Procname.t -> bool) -> typ -> Typ.Procname.t * Procdesc.t
end = struct
    include Caml.Map.Make(Typ.Procname)
    type typ = Procdesc.t t
end

let procs = ref Procs.empty

module Storage : sig
    val store : NameType.t -> unit 
    val load : unit -> NameType.t
    val store_fun : Procs.typ -> unit 
    val load_fun : unit -> Procs.typ
end = struct
    let glob_env = "glob_env.dat"
    let func_env = "funcs.dat"

    let load () = 
      let ic = Pervasives.open_in glob_env in
      let res = Marshal.from_channel ic in
      Pervasives.close_in ic; res

    let store a = 
      let oc = Pervasives.open_out glob_env in
      Marshal.to_channel oc a [];
      Pervasives.close_out oc

    let store_fun a = 
        let oc = Pervasives.open_out func_env in
        Marshal.to_channel oc a [];
        Pervasives.close_out oc

    let load_fun () = 
        let ic = Pervasives.open_in func_env in
        let res = Marshal.from_channel ic in
        Pervasives.close_in ic; res
end

let checker {Callbacks.proc_desc; tenv; summary} : Summary.t =
    let proc_data = ProcData.make_default proc_desc tenv in 
    let proc_name_str = Typ.Procname.to_string (Procdesc.get_proc_name proc_desc) in
    match Analyzer.compute_post proc_data ~initial:NameType.empty with
      | Some p -> 
    (*      let _ = L.progress "Proc: %s\nFINAL: %a\n" proc_name_str NameType.pp p in*)
          (*Storage.store p;*)
          let session = incr summary.Summary.sessions ; !(summary.Summary.sessions) in
          {summary with Summary.payloads = { summary.Summary.payloads with Payloads.global_preanalysis = Some p}; Summary.proc_desc = proc_desc; Summary.sessions = ref session}
      | None ->
          summary
            (*
    let inv_map = Analyzer.exec_pdesc (ProcData.make_default proc_desc tenv) ~initial:NameType.empty in
    let post = inv_map.AbstractInterpreter.State.post in 
    Storage.store post;
    summary*)
    (*Analyzer.InvariantMap.iter merge inv_map;
    Storage.store !summ;
    summary*)
