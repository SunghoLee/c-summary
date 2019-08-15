open Core
module F = Format
module L = Logging
module Sem = SemanticFunctions
module GH = GlobalHandler
open SemanticSummaryDomain
open SUtils

module LocSet = SemanticSummaryDomain.LocSet

module IOModule = struct
  let file = "global_locations.dat"

  let load_glocs () = 
    try
      let ic = Pervasives.open_in file in
      let res = Marshal.from_channel ic in
      Pervasives.close_in ic; res
    with _ ->
      LocSet.empty
    
  let store_glocs locs = 
    let oc = Pervasives.open_out file in
    Marshal.to_channel oc locs [];
    Pervasives.close_out oc
end

let get_all_procs () =
  InferBase.ResultsDir.assert_results_dir "";
  Procedures.get_all (fun x y -> true) () 

let get_summary p = Summary.get p

let get_semantic_summary p =
  match get_summary p with
  | Some s ->(
    match s.Summary.payloads.Payloads.semantic_summary with
    | Some _ as o -> 
        o
    | None -> 
        None)
  | None ->
      None

let get_summaries procs = 
  let f l p =
    match get_summary p with
    | Some s ->
        s::l
    | None ->
        l
  in
  Caml.List.fold_left f [] procs

let update_summaries summs = 
  Caml.List.iter (fun summ -> Summary.store summ) summs
  
let collect_all_store summs = 
  let f gs s = (
    match s.Summary.payloads.Payloads.semantic_summary with
    | Some (_, gs') ->
        GH.GlobalStore.fold (fun l v gs'' -> GH.GlobalStore.weak_update l v gs'') gs' gs
    | None ->
        gs)
  in
  Caml.List.fold_left f GH.GlobalStore.empty summs

let lift gs =
  let f l v gs' =
    GH.GlobalStore.add (Loc.mk_concrete_pointer l) v gs'
  in
  GH.GlobalStore.fold f gs GH.GlobalStore.empty

let replace_avs gs avs =
  let f (l, cst) avs' =
    match GH.GlobalStore.find_opt l gs with
    | Some v ->
        Val.fold (fun (l', _) v' -> Val.add (l', cst) v') v avs'
    | None ->
        Val.add (l, cst) avs'
  in
  Val.fold f avs Val.empty

let replace_heap gs heap =
  let f l v h =
    Heap.add l (replace_avs gs v) h
  in
  Heap.fold f heap Heap.empty

let replace_log gs log =
  let f u l =
    CallLogs.add {u with LogUnit.heap = replace_heap gs u.LogUnit.heap} l
  in
  CallLogs.fold f log CallLogs.empty

let replace gs summs =
  let f l s = (
    match s.Summary.payloads.Payloads.semantic_summary with
    | Some ({heap; logs}, gs') -> (
        let heap' = replace_heap gs heap in
        let log' = replace_log gs logs in
        let s' = {s with Summary.payloads = { s.Summary.payloads with Payloads.semantic_summary = Some ({heap=heap';logs=log'}, gs')}} in
        s'::l
        )
    | None ->
       l)
  in
  Caml.List.fold_left f [] summs

let remove_loc l v =
  Val.filter (fun (l', cst) -> not (l = l')) v

let remove_simple_extension gs = 
  let f l v gs' = 
    GH.GlobalStore.add l (remove_loc (Loc.mk_concrete_pointer l) v) gs'
  in
  GH.GlobalStore.fold f gs GH.GlobalStore.empty


let print_all () =
  let all_procs = get_all_procs () in
  let summs = get_summaries all_procs in
  let f summ =
    match summ.Summary.payloads.Payloads.semantic_summary with
    | Some ({heap; logs}, gs') ->
        L.progress "FUN: %s\nHeap: %a\nLog: %a\n@." (InferIR.Typ.Procname.to_string (Summary.get_proc_name summ)) Heap.pp heap CallLogs.pp logs
    | None ->
        ()
  in
  Caml.List.iter f summs

let print_gs gs = 
  L.progress "GS: %a\n@." GH.GlobalStore.pp gs

let collect_implicit_locs gs =
  let f l v set =
    Val.fold (fun (l', _) set' -> if Loc.is_implicit l' then LocSet.add l' set' else set') v set
  in
  GH.GlobalStore.fold f gs LocSet.empty

let _  = 
  let all_procs = get_all_procs () in
  let summs = get_summaries all_procs in
  let gs = collect_all_store summs in
  let gs' = lift gs in
  let ilocs = collect_implicit_locs gs' in
  let summs' = replace gs' summs in
  update_summaries summs';
  IOModule.store_glocs ilocs

