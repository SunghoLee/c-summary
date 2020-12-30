(*
 * Copyright (c) 2020, SW@ Laboratory at CNU.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open Core
module F = Format
module L = Logging

module M = PrettyPrintable.MakePPMap(struct include Pvar let pp = pp Pp.text end) 
include (M: module type of M with type 'a t := 'a M.t)

type t = Typ.t M.t

let pp_internal = pp ~pp_value:(Typ.pp_full Pp.text)

let pp fmt gnt =
  F.fprintf fmt "%a" pp_internal gnt

let debug = ref false

let choose_most_specific_one typ1 typ2 =
  (* 1 means that the second one is more specific than the first, and -1 means the opposite way. *)
  let rec impl typ1 typ2 =
    match typ1.Typ.desc, typ2.Typ.desc with
    | Tarray _,  Tptr _ -> typ1
    | Tptr _,  Tarray _ -> typ2
    | Tptr (base1, _), Tptr (base2, _) -> Typ.mk (Tptr (impl base1 base2, Pk_pointer))
    | Tarray {elt=et1; length=length1}, Tarray {elt=et2; length=length2} -> 
        let base = impl et1 et2 in 
        let i = (match length1, length2 with
          | Some l1, Some l2 -> if IntLit.leq l1 l2 then l2 else l1
          | Some l, _ | _, Some l -> l
          | _, _ -> IntLit.zero)
        in
        Typ.mk_array ~length:i base
    | Tarray _, _ -> typ1
    | _, Tarray _ -> typ2
    | Tvoid, _ -> typ2
    | _, Tvoid -> typ1
    | _ -> typ1
        (*if Typ.equal_desc typ1.Typ.desc typ2.Typ.desc then
          typ1
        else
          let _ = L.progress "Not compatible types:  %a <> %a\n@." (Typ.pp_full Pp.text) typ1 (Typ.pp_full Pp.text) typ2 in
          typ1*)
          (*failwith (F.asprintf "Not compatible types: %a <> %a" (Typ.pp_full Pp.text) typ1 (Typ.pp_full Pp.text) typ2)*)
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
  impl typ1 typ2

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
    if Typ.equal_desc typ1.Typ.desc typ2.Typ.desc then
      Some typ1
    else
      ((*L.progress "\t\tProcessing: %a\n@." (Pvar.pp Pp.text) var *)
      Some (choose_most_specific_one typ1 typ2))
      )
  |> (fun x -> union x lhs rhs)

let widen ~prev ~next ~num_iters = 
  (*(
  if num_iters > 500 then
    let _ = L.progress "PREV: %a\n@." pp prev in
    let _ = L.progress "NEXT: %a\n@." pp next in
    let _ = L.progress "EQ: %b\n@." (prev <= next) in
    ());*)
  join prev next

let add pvar typ m =
  if mem pvar m then
    let pre_typ = find pvar m in
    add pvar (choose_most_specific_one typ pre_typ) m
  else
    add pvar typ m
