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

module NameType = struct
  include PrettyPrintable.MakePPMonoMap(struct include Pvar let pp = pp Pp.text end)(struct include Typ let pp = pp_full Pp.text end)

  let debug = ref false

  let choose_most_specific_one typ1 typ2 =
    (* 1 means that the second one is more specific than the first, and -1 means the opposite way. *)
    let rec impl typ1 typ2 =
      match typ1.Typ.desc, typ2.Typ.desc with
      | Tarray _,  Tptr _ -> -1
      | Tptr _,  Tarray _ -> 1
      | Tptr (base1, _), Tptr (base2, _) -> impl base1 base2
      | Tarray {length=length1}, Tarray {length=length2} -> if length1 > length2 then -1 else 1
      | Tarray _, _ -> -1
      | _, Tarray _ -> 1
      | Tvoid, _ -> 1
      | _, Tvoid -> -1
      | _ -> 
          if typ1 = typ2 then
            1
          else
            failwith (F.asprintf "Not compatible types: %a <> %a" (Typ.pp_full Pp.text) typ1 (Typ.pp_full Pp.text) typ2)
      (*
      | Tint of ikind  (** integer type *)
      | Tfloat of fkind  (** float type *)
      | Tvoid  (** void type *)
      | Tfun of {no_return: bool}  (** function type with noreturn attribute *)
      | Tptr of t * ptr_kind  (** pointer type *)
      | Tstruct of name  (** structured value type name *)
      | TVar of string  (** type variable (ie. C++ template variables) *)
      | Tarray of {elt: t; length: IntLit.t option; stride: IntLit.t option}*)
    in
    if (impl typ1 typ2) > 0 then typ2
    else typ1

  let ( <= ) ~lhs ~rhs = 
    (fun var typ ->
      mem var rhs && (find var lhs) = (find var rhs))
    |> (fun x -> for_all x lhs)

  let rec equal_template_arg (arg1: Typ.template_arg) (arg2: Typ.template_arg) =
    match arg1, arg2 with
    | TType t1, TType t2 -> equal_typs t1 t2
    | TInt i1, TInt i2 -> i1 = i2
    | TNull, TNull | TNullPtr, TNullPtr | TOpaque, TOpaque -> true
    | _ -> false
        
  and equal_template_args arg1 arg2 =
    match arg1, arg2 with
    | [], [] -> true
    | h1 :: t1, h2 :: t2 -> (equal_template_arg h1 h2) && (equal_template_args t1 t2)
    | _ -> false

  and equal_template_spec (sp1: Typ.template_spec_info) (sp2: Typ.template_spec_info) =
    match sp1, sp2 with
    | NoTemplate, NoTemplate -> true
    | Template {mangled = m1; args = a1}, Template {mangled = m2; args = a2} -> (
      match m1, m2 with
      | None, None -> true
      | Some s1, Some s2 when s1 = s2 -> 
          equal_template_args a1 a2
      | _ ->
          false)
    | _ -> false

  and equal_quals q1 q2 =
    (QualifiedCppName.to_qual_string q1) = (QualifiedCppName.to_qual_string q2)

  and equal_str_name (n1: Typ.name) (n2: Typ.name) =
    match n1, n2 with
    | CStruct qcn1, CStruct qcn2 -> equal_quals qcn1 qcn2
    | CUnion qcn1, CUnion qcn2 -> equal_quals qcn1 qcn2
    | CppClass (qcn1, spec1), CppClass (qcn2, spec2) -> 
        (equal_quals qcn1 qcn2) && (equal_template_spec spec1 spec2)
    | _ -> false

  and equal_typs (typ1: Typ.t) (typ2: Typ.t) = 
    match typ1.desc, typ2.desc with
    | Tstruct n1, Tstruct n2 -> equal_str_name n1 n2
    | Tint i1, Tint i2 -> i1 = i2
    | Tfloat f1, Tfloat f2 -> f1 = f2
    | Tvoid, Tvoid -> true
    | Tfun {no_return = r1}, Tfun {no_return = r2} -> r1 = r2
    | Tptr (b1, _), Tptr (b2, _) -> equal_typs b1 b2
    | TVar s1, TVar s2 -> s1 = s2
    | Tarray {elt = e1; length = l1; stride = s1}, Tarray {elt = e2; length = l2; stride = s2} -> 
        (e1 = e2) && (l1 = l2) && (s1 = s2)
    | _ -> false

  let join lhs rhs = 
    (fun var typ1 typ2 ->
      if equal_typs typ1 typ2 then
        Some typ1
      else
        ((*L.progress "\t\tProcessing: %a\n@." (Pvar.pp Pp.text) var *)
        Some (choose_most_specific_one typ1 typ2))
        )
    |> (fun x -> union x lhs rhs)

  let widen ~prev ~next ~num_iters = join prev next

  let add pvar typ m =
    if mem pvar m then
      let pre_typ = find pvar m in
      add pvar (choose_most_specific_one typ pre_typ) m
    else
      add pvar typ m
end

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

  let pp_inst (i: Sil.instr) = 
    L.progress "[%s] %a\n@." (get_inst_type i) (Sil.pp_instr ~print_types:true Pp.text) i 

  let exec_expr m (expr : Exp.t) t instr : Domain.t =
      match expr with
      | Var i -> m
      | UnOp (op, e, typ) -> m
      | BinOp (op, e1, e2) -> m
      | Exn typ -> m
      | Closure f -> m
      | Const c -> m
      | Cast (typ, e) -> m
      | Lfield (e, fn, typ) -> m
      | Lindex (e1, e2) -> m
      | Sizeof data -> m
      | Lvar pvar -> 
              if Pvar.is_global pvar then
                  Domain.add pvar (Typ.strip_ptr t) m
              else 
                  m

  let exec_instr astate proc_data node (instr: Sil.instr) =
      (*L.progress "PRE: %s\n@." (Domain.pp_m astate);*)
      pp_inst instr; 
      let post = (
      match instr with
      | Load (id, Lvar pvar, typ, loc) -> 
          if Pvar.is_global pvar then
            let () = L.progress "\tGlobal: %a: %a\n@." (Pvar.pp Pp.text) pvar (Typ.pp_full Pp.text) typ in
            Domain.add pvar typ astate
          else
            astate

      | Load (id, Lindex (Lvar pvar, Const (Cint s)), typ, loc) -> 
          if Pvar.is_global pvar then
            let () = L.progress "\tGlobal: %a: %a\n@." (Pvar.pp Pp.text) pvar (Typ.pp_full Pp.text) typ in
            let typ' = Typ.mk_array ~length:(IntLit.add s IntLit.one) typ in
            Domain.add pvar typ' astate
          else
            astate

      | Store (Lvar pvar, typ, e2, loc) -> 
          if Pvar.is_global pvar then
            let () = L.progress "\tGlobal: %a: %a\n@." (Pvar.pp Pp.text) pvar (Typ.pp_full Pp.text) typ in
            Domain.add pvar typ astate
          else
            astate

      | Store (Lindex (Lvar pvar, Const (Cint s)), typ, e2, loc) -> 
          if Pvar.is_global pvar then
            let () = L.progress "\tGlobal: %a: %a\n@." (Pvar.pp Pp.text) pvar (Typ.pp_full Pp.text) typ in
            let typ' = Typ.mk_array ~length:(IntLit.add s IntLit.one) typ in
            Domain.add pvar typ' astate
          else
            astate

      | Store (e1, typ, Lvar pvar, loc) -> 
          if Pvar.is_global pvar then
            let () = L.progress "\tGlobal: %a: %a\n@." (Pvar.pp Pp.text) pvar (Typ.pp_full Pp.text) typ in
            Domain.add pvar (Typ.strip_ptr typ) astate
          else
            astate

      | Call ((id, typ_e1), (Const (Cfun callee_pname)), args, loc, flag) when (Typ.Procname.to_string callee_pname) = "__variable_initialization" -> 
          Caml.List.fold_left (fun i (e, t) -> exec_expr i e (Typ.mk (Tptr (t, Pk_pointer))) instr) astate args
      | Call ((id, typ_e1), (Const (Cfun callee_pname)), args, loc, flag) -> 
          Caml.List.fold_left (fun i (e, t) -> exec_expr i e t instr) astate args
      | _ -> astate
              ) in
    let node_kind = CFG.Node.kind node in
    let pname = Procdesc.get_proc_name proc_data.ProcData.pdesc in
    (*L.progress "POST: %s\n@." (Domain.pp_m post); *)
    post

    let pp_session_name _node fmt = F.pp_print_string fmt "C/C++ semantic summary analysis"
end

module Analyzer = AbstractInterpreter.MakeRPO (TransferFunctions (ProcCfg.Exceptional))

let summ = ref NameType.empty

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
      let tn = try load () with _ -> NameType.empty in
      let tn' = NameType.join a tn in
      let oc = Pervasives.open_out glob_env in
      Marshal.to_channel oc tn' [];
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
    let proc_name = Procdesc.get_proc_name proc_desc in
    (
        procs := Procs.add proc_name proc_desc !procs;
        Storage.store_fun !procs
    );
    let merge node_id state = 
        let post = state.AbstractInterpreter.State.post in
        summ := NameType.union (fun x y z -> Some y) post !summ
    in 
    let proc_name_str = Typ.Procname.to_string (Procdesc.get_proc_name proc_desc) in
    let inv_map = Analyzer.exec_pdesc (ProcData.make_default proc_desc tenv) ~initial:(!summ) in
    Analyzer.InvariantMap.iter merge inv_map;
    Storage.store !summ;
    summary
