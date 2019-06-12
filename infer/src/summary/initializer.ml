open! IStd
open Core
module F = Format
module L = Logging
module Helper = HelperFunction
open SUtils
open SemanticSummaryDomain

module AliasEnv = struct
  module PkSet = PrettyPrintable.MakePPSet(PointerPreanalysisDomain.PointerKey)

  let find_all_root loc amap = 
    let open PointerPreanalysis.AliasReporter in
    let open PointerPreanalysisDomain in
    let f_pkmap pk n set = 
      let pk_loc = PointerKey.get_holder pk |> LocHolder.get_loc in
      if pk_loc = loc then PkSet.add (UnionFind.Tree.Node.get_pk (UnionFind.find n)) set
      else set
    in
    let f_pres proc pkmap set = (L.progress "S: %s\n@." (Typ.Procname.to_string proc);set) (*Pk2NodeMap.fold f_pkmap pkmap set*) in
    ProcResMap.fold f_pres amap PkSet.empty

  let is_alias alias_map l1 l2 = 
    let open PointerPreanalysis.AliasReporter in
    let open PointerPreanalysisDomain in
    let alias_results = alias_map in 
    let () = L.progress "Start? \n@." in
    (*let min = ProcResMap.min_binding alias_results in*)
    let () = L.progress "Alias calculation...\n@." in
    let () = L.progress "Size: %d\n@." (ProcResMap.cardinal alias_results) in
    let l1_root = find_all_root l1 alias_results in
    let l2_root = find_all_root l2 alias_results in
    let inter = PkSet.inter l1_root l2_root in
    if PkSet.is_empty inter then false
    else true
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

  let remove_elem typ loc map = 
    match find_opt typ map with
    | Some s ->
        let loc_set = LocSet.remove loc s in
        add typ loc_set map
    | None ->
        map

  let add typ loc map =  
    match find_opt typ map with
    | Some s ->
        let loc_set = LocSet.add loc s in
        add typ loc_set map
    | None ->
        add typ (LocSet.singleton loc) map 

  (* compare two Typ s using Typ.equal_ignore_quals
   * (except the case that given types are structs) *)
  let typ_equal_ignore_quals typ typ' =
    match typ.Typ.desc, typ'.Typ.desc with
    | Tstruct name, Tstruct name' ->
        name = name'
    | _ ->
        Typ.equal_ignore_quals typ typ'

  let find_opt typ map =
    match bindings map |> Caml.List.find_opt (fun (typ', v) -> typ_equal_ignore_quals typ typ') with
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
  let module TypSet = PrettyPrintable.MakePPSet(struct include Typ let pp = pp_full Pp.text end) in
  let rec f visited (loc, typ) tmap =
    if JniModel.is_jni_struct typ || TypSet.mem typ visited then tmap
    else 
      let tmap' = TypMap.add typ loc tmap in
      let visited' = TypSet.add typ visited in
      match typ.Typ.desc with
      | Tptr (typ', kind) ->
          f visited' ((Loc.mk_var_pointer loc), typ') tmap'
      | Tstruct name ->
          Helper.get_fld_and_typs name tenv
          |> Caml.List.fold_left 
              (fun tmap (field, typ) -> f visited' (Loc.mk_offset loc (Loc.mk_const_of_string field), (Typ.mk (Tptr (typ, Pk_pointer)))) tmap)
                tmap'
      | Tarray {elt; length = Some i} -> (* fixed size arrays *)
          (*let loc' = Loc.unwrap_ptr loc in (* C allocates array location directly to variable address *)
          let index = (IntLit.to_int_exn i) - 1 in
          let rec mk_array = fun i tmap -> (
            if i = -1 then tmap
            else
              f struct_typs (Loc.mk_offset loc' (Loc.mk_const_of_z (Z.of_int i)), (Typ.mk (Tptr (elt, Pk_pointer)))) tmap
              |> mk_array (i - 1))
          in
          mk_array index tmap*)
          tmap
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
  Caml.List.fold_right (f TypSet.empty) loc_typs tmap

let init_heap ~this ~do_array loc_typs tenv heap tmap = (* loc_types: local list *)
  let alias_map = PointerPreanalysis.AliasReporter.load_aliases () in
  let module TypSet = PrettyPrintable.MakePPSet(struct include Typ let pp = pp_full Pp.text end) in
  let is_gt l1 l2 = (Loc.compare l1 l2) = 1 in
  let pos_aliases addr typ tmap =
    TypMap.find typ tmap ~default:LocSet.empty
    |> LocSet.filter (fun x -> is_gt addr x && (Loc.is_pointer x || Loc.is_offset x))
    |> LocSet.filter (AliasEnv.is_alias alias_map addr)
  in
  let handle_alias base_cst addr pos_a = (
    if not this then
      let v, cst = LocSet.fold 
        ((fun alias (v, cst) ->
          let cst_eq = Cst.cst_eq alias addr in
          let cst_neq = Cst.cst_not cst_eq in
          let cst' = Cst.cst_and cst cst_eq in
          Helper.(v + Val.singleton (alias, cst')), Cst.cst_and cst cst_neq))
        pos_a (Val.empty, base_cst)
      in
      Val.add (addr, cst) v
    else 
      Val.singleton (addr, base_cst))
  in
  let mk_new_base_cst base_cst addr pos_a =
    (* make (a1 != addr) ^ (a2 != addr) ^ ... ^ (an != addr) *)
    LocSet.fold 
      (fun alias cst -> Cst.cst_not (Cst.cst_eq alias addr) |> Cst.cst_and cst)
      pos_a base_cst
  in
  let rec iter_loc visited base_cst heap (addr, typ) = 
    (* base_cst: inherited constraint *)
    if JniModel.is_jni_struct typ || TypSet.mem typ visited then heap
    else 
      let desc = typ.Typ.desc in
      let visited' = TypSet.add typ visited in
      match desc with
      | Tptr (ptr_typ, kind) ->
          let ptr_loc = 
            if this && not (Loc.is_explicit addr) then (* 'this' value in constructors *)
              Loc.mk_concrete_pointer addr 
            else
              Loc.mk_var_pointer addr
          in
          let pos_a = pos_aliases ptr_loc ptr_typ tmap in
          let heap' = handle_alias base_cst ptr_loc pos_a
            |> (fun x -> Heap.add addr x heap)
          in
          let new_cst = mk_new_base_cst base_cst ptr_loc pos_a in
          iter_loc visited' new_cst heap' (ptr_loc, ptr_typ)
      | Tstruct name -> (
        if not this then
          Helper.get_fld_and_typs name tenv
          |> Caml.List.fold_left
              (fun heap (field, typ) ->
                iter_loc visited' base_cst heap (Loc.mk_offset addr (Loc.mk_const_of_string field), (Typ.mk (Tptr (typ, Pk_pointer))))) heap 
        else
          heap)
      | Tarray {elt; length = Some i} -> (* fixed size arrays *)
          if do_array then
            let loc' = Loc.unwrap_ptr addr in (* C allocates array location directly to variable address *)
            let index = (IntLit.to_int_exn i) - 1 in
            let rec mk_array i heap = 
              if i = -1 then heap
              else
                iter_loc visited base_cst heap (Loc.mk_offset loc' (Loc.mk_const_of_z (Z.of_int i)), (Typ.mk (Tptr (elt, Pk_pointer)))) 
                |> mk_array (i - 1)
            in
            mk_array index heap
          else
            heap
      | _ -> 
          heap
  in
  let res = Caml.List.fold_left (iter_loc TypSet.empty Cst.cst_true) heap loc_typs in
  let () = L.progress "INIT: %a\n@." Heap.pp res in
  res

let init tenv pdesc =
  let scope = VVar.mk_scope (Typ.Procname.to_string (Procdesc.get_proc_name pdesc)) in
  let globs, do_array = 
    if GlobalEnv.is_global_var_init_fun pdesc then
      (match GlobalEnv.get_initialized_global pdesc with
      | Some (pvar, typ) ->
          [VVar.of_pvar pvar, typ], true
      | None ->
          [], false)
    else 
      [], false
  in
  let arg_vars = (Caml.List.map
    (fun (arg, typ) -> (VVar.of_string (Mangled.to_string arg) ~proc:scope, typ))
    (Procdesc.get_formals pdesc)) 
    @ (GlobalEnv.get_glob_vars ())
  in
  let local_vars = globs
      (* @ Caml.List.map
      (fun (var: ProcAttributes.var_data) -> VVar.of_string (Mangled.to_string var.name) ~proc:scope, var.typ)
      (Procdesc.get_locals pdesc)*)
  in
  let locs_arg, locs_loc = 
    (fun (var, typ) -> Loc.mk_explicit var, Typ.mk (Tptr (typ, Pk_pointer)))
    |> (fun f -> (Caml.List.map f arg_vars, Caml.List.map f local_vars))
  in
  let res = 
    if (Typ.Procname.is_constructor (Procdesc.get_proc_name pdesc)) then
      let this_arg = Caml.List.hd (locs_arg @ locs_loc) in
      let rest_args = Caml.List.tl (locs_arg @ locs_loc) in
      let tmap = mk_tmap (Caml.List.tl locs_arg) tenv TypMap.empty in
      init_heap ~this:false ~do_array rest_args tenv (init_heap ~this:true ~do_array [this_arg] tenv Heap.empty tmap) tmap
    else
      let tmap = mk_tmap locs_arg tenv TypMap.empty in
      init_heap ~this:false ~do_array (locs_arg @ locs_loc) tenv Heap.empty tmap
  in
  let () = L.progress "#INIT: %a\n@." Heap.pp res in
  res
