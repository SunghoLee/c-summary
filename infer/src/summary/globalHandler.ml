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
module Sem = SemanticFunctions
open SemanticSummaryDomain
open SUtils

module TypSet = PrettyPrintable.MakePPSet(struct include Typ let pp = pp_full Pp.text end)

module GlobalStore = struct
  include Heap
  let add = strong_update
end


      (*
let rec mk_dummy_loc (expr: Exp.t) = 
  match expr with
  | Lvar pvar -> 
      Loc.of_pvar pvar
  | Lfield (e, fn_tn, typ) -> 
      let field = Typ.Fieldname.to_string fn_tn |> Loc.mk_const_of_string in
      let glob_addr = mk_dummy_loc e in
      let glob_ptr_addr = Loc.mk_concrete_pointer glob_addr in
      Loc.mk_offset glob_ptr_addr field
  | Lindex (e1, e2) -> 
      Loc.mk_implicit "GB"
  | _ -> 
      failwith (F.asprintf "It does not a global variable: %a" Exp.pp expr)
      *)

let rec get_glob_pvar (expr: Exp.t) =
  match expr with
  | Lvar pvar -> 
      pvar
  | Cast (typ, e) ->
      get_glob_pvar e
  | Lfield (e, fn_tn, typ) -> 
      get_glob_pvar e
  | Lindex (e1, e2) -> 
      get_glob_pvar e1
  | _ -> 
      failwith (F.asprintf "It does not a global variable: %a" Exp.pp expr)

      (*
let rec get_glob_pvar_opt (expr: Exp.t) =
  match expr with
  | Lvar pvar -> 
      Some pvar
  | Cast (typ, e) ->
      get_glob_pvar_opt e
  | Lfield (e, fn_tn, typ) -> 
      get_glob_pvar_opt e
  | Lindex (e1, e2) -> 
      get_glob_pvar_opt e1
  | _ -> None
  *)

let rec mk_dummy_heap tenv visited heap (addr, typ) = 
  if TypSet.mem typ visited then
    heap
  else if Heap.mem addr heap then
    heap
  else
    let desc = typ.Typ.desc in
    let visited' = TypSet.add typ visited in
    match desc with
    | Tptr (ptr_typ, kind) ->
        let ptr_loc = Loc.mk_concrete_pointer addr in
        let heap' = Heap.add addr (Val.singleton (ptr_loc, Cst.cst_true)) heap in
        mk_dummy_heap tenv visited' heap' (ptr_loc, ptr_typ)
    | Tstruct name -> (
        match HelperFunction.get_fld_and_typs_opt name tenv with
        | Some s -> Caml.List.fold_left
            (fun heap (field, typ) ->
              mk_dummy_heap tenv visited' heap (Loc.mk_offset addr (Loc.to_const_typ_of_string field), (Typ.mk (Tptr (typ, Pk_pointer))))) heap s
        | None -> heap
        )
    | Tarray {elt; length = Some i} -> (* fixed size arrays *)
        heap
    | _ -> 
        heap


let inject_dummy_mappings tenv pvar typ heap =
  let lpvar = Loc.of_pvar pvar in
  mk_dummy_heap tenv TypSet.empty heap (lpvar, (Typ.mk (Tptr (typ, Pk_pointer))))

let rec is_global (expr: Exp.t) = 
  match expr with
  | Lvar pvar -> (* Location of a variable *)
      Pvar.is_global pvar
  | Cast (typ, e) ->
      is_global e
  | Lfield (e, fn_tn, typ) -> (* Location of a field *)
      is_global e
  | Lindex (e1, e2) -> (* &(e1[e2]) *)
      is_global e1
  | _ -> 
      false

let remove_loc l v =
  Val.filter (fun (l', cst) -> not (l = l')) v

let remove_simple_extension gs = 
  let f l v gs' = 
    let v' = (remove_loc (Loc.mk_concrete_pointer l) v) in
    if Val.is_empty v' then
      gs'
    else
      GlobalStore.add l v' gs'
  in
  GlobalStore.fold f gs GlobalStore.empty

let collect_global_store (h: Heap.t) =
  Heap.fold (fun l v acc ->
    if Loc.is_global_loc l then
      GlobalStore.add l v acc
    else 
      acc) h GlobalStore.empty
  |> remove_simple_extension

let whole_pp fmt (a,b) =
  L.progress "%a \n %a \n@." SemanticSummaryDomain.pp a GlobalStore.pp b
