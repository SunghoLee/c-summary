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
module Domain = SemanticSummaryDomain
open Domain
open Pervasives
open SUtils

let get_struct name tenv =
  match Tenv.lookup tenv name with
  | Some s -> 
      s
  | None -> 
      failwith ("The structure cannot be found in a type environment: " ^ (Typ.Name.to_string name))

let get_fld_and_typs name tenv = 
  get_struct name tenv
  |> (fun x -> x.Typ.Struct.fields @ x.Typ.Struct.statics)
  |> ((fun pairs (n, ftyp, _) -> (Typ.Fieldname.to_string n, ftyp) :: pairs) 
    |> (fun f -> Caml.List.fold_left f []))

let get_fld_and_typs_opt name tenv = try Some (get_fld_and_typs name tenv) with _ -> None
  
let find_field_typ: String.t -> Typ.Struct.fields -> Typ.t = 
  fun name fields ->
    let rec iter_field : (Typ.Fieldname.t * Typ.t * Annot.Item.t) list -> Typ.t = 
      function
        h :: t ->
          (match h with
          | (fn, typ, _) when (Typ.Fieldname.to_string fn) = name -> typ
          | _ -> iter_field t)
        | [] -> 
            failwith "The field does not exist."
    in
    iter_field fields

(* {(v1, cst1) ... (vn, cstn)} ^ (cst) = {(v1, cst1 & cst) ... (vn, cstn & cst)} *)
let ( ^ ) v cst = Val.map (fun (loc, cst') -> (loc, Cst.cst_and cst' cst)) v

(* {(v11, cst11) ... (v1n, cst1n)} + {(v21, cst21) ... (v2m, cst2m)}  = 
  {(v11, cst11) ... (v1n, cst1n), (v21, cst21) ... (v2m, cst2m)}*)
let ( + ) lhs rhs = Val.union lhs rhs

(* Cartesian product *)
let ( * ) x y = Caml.List.concat (Caml.List.map (fun x -> Caml.List.map (fun y -> x, y) (Val.elements y)) (Val.elements x))

(* x = *y  *)
let load v heap =
  (fun ((loc: Loc.t), cst) v ->
    let v' = Heap.find loc heap in
    let v'' = v' ^ cst in
    v'' + v )
  |> (fun f -> Val.fold f v Val.empty)

(* *x = y *)
let store lhs rhs heap =
  let f = fun (loc, cst) heap ->
    let pre_v = 
      match Heap.find_opt loc heap with
      | Some v -> 
          v
      | None -> (* first assignment cases: no exising pre-assigned value *)
          Val.empty 
    in
    let pre_v' = pre_v ^ (Cst.cst_not cst) in
    let new_v = rhs ^ cst in
    let merged_v = pre_v' + new_v in
    Heap.add loc merged_v heap 
    |> Heap.add (Loc.mk_offset loc (Loc.to_const_typ_of_z Z.zero)) merged_v (* special handling for array: *array = array[0] *)
  in
  Val.fold f lhs heap

