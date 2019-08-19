open Core
module F = Format
module L = Logging
module Sem = SemanticFunctions
module GT = GlobalNameType

open SemanticSummaryDomain
open SUtils

let get_all_procs () =
  InferBase.ResultsDir.assert_results_dir "";
  Procedures.get_all (fun x y -> true) () 

let get_summary p = Summary.get p

let get_global_types p =
  match get_summary p with
  | Some s ->(
    match s.Summary.payloads.Payloads.global_preanalysis with
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

let collect_all_globals summs = 
  let f gt s = (
    match s.Summary.payloads.Payloads.global_preanalysis with
    | Some gt' ->
        GT.join gt gt'
    | None ->
        gt)
  in
  Caml.List.fold_left f GT.empty summs

let print_all nt =
  L.progress "GLOBAL: %a\n@." GT.pp nt

let _  = 
  let all_procs = get_all_procs () in
  let summs = get_summaries all_procs in
  let gt = collect_all_globals summs in
  print_all gt;
  PreForGlobal.Storage.store gt
