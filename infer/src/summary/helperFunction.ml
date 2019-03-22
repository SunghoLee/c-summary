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
let ( ^ ) avs cst =
  AVS.map (fun (value, cst') -> (value, Cst.cst_and cst' cst)) avs

(* {(v11, cst11) ... (v1n, cst1n)} + {(v21, cst21) ... (v2m, cst2m)}  = 
  {(v11, cst11) ... (v1n, cst1n), (v21, cst21) ... (v2m, cst2m)}*)
let ( + ) lhs rhs = AVS.union lhs rhs

(* Cartesian product *)
let ( * ) x y =
  Caml.List.concat (Caml.List.map (fun x -> Caml.List.map (fun y -> x, y) (AVS.elements y)) (AVS.elements x))

(* x = *y  *)
let load avs heap =
  let f = fun (value, cst) avs ->
    let loc = Val.to_loc value in
    let avs' = Heap.find loc heap in
    let avs'' =  (* special handling for array: *array = array[0] *)
      (match loc with
      | Offset (base, (I 0)) | Offset (base, IndexTop) ->
          (match Heap.find_opt base heap with
          | Some s ->
              avs' + s
          | None ->
              avs')
      | _ -> 
          (match Heap.find_opt (Loc.mk_offset loc (Loc.mk_index_of_int 0)) heap with
          | Some s ->
              avs' + s
          | None ->
              avs'))
    in
    let avs''' = avs'' ^ cst in
    avs''' + avs
  in
  AVS.fold f avs AVS.empty

(* *x = y *)
let store lhs rhs heap =
  let f = fun (value, cst) heap ->
    let loc = Val.to_loc value in
    let pre_avs = 
      match Heap.find_opt loc heap with
      | Some v -> 
          v
      | None -> (* first assignment cases: no exising pre-assigned value *)
          AVS.empty 
    in
    let pre_avs' = pre_avs ^ (Cst.cst_not cst) in
    let new_avs = rhs ^ cst in
    let merged_avs = pre_avs' + new_avs in
    Heap.add loc merged_avs heap 
  in
  AVS.fold f lhs heap

