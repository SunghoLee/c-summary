open! IStd
open Core
module F = Format
module L = Logging
module Helper = HelperFunction
open SUtils
open SemanticSummaryDomain

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

(* module Initializer *)

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
  let local_vars = 
    if GlobalEnv.is_global_var_init_fun pdesc then
      [GlobalEnv.get_initialized_global_ext pdesc |> (fun (pvar, typ) -> Var.of_pvar pvar, typ)]
    else
      Caml.List.map
      (fun (var: ProcAttributes.var_data) -> Var.of_string (Mangled.to_string var.name) ~proc:scope, var.typ)
      (Procdesc.get_locals pdesc)
  in
  let locs_arg, locs_loc = 
    (fun (var, typ) -> Loc.mk_explicit var, Typ.mk (Tptr (typ, Pk_pointer)))
    |> (fun f -> (Caml.List.map f arg_vars, Caml.List.map f local_vars))
  in
  let tmap = mk_tmap locs_arg tenv TypMap.empty in
  init_heap (locs_arg @ locs_loc) tenv Heap.empty tmap
