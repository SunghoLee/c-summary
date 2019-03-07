open Pervasives
module L = Logging
module Domain = SemanticSummaryDomain
open Domain 

type env_t = Loc.t Env.t
type heap_t = AVS.t Heap.t

let sem_new_array : Var.t -> AVS.t list -> env_t -> heap_t -> CallLogs.t -> Domain.t =
  fun lhs_var args_avs env heap logs ->
    let avs, heap' = 
      let top_avs = AVS.singleton (Val.of_loc Loc.top, Cst.cst_true) in
      match args_avs with
      | h  :: [] ->
          if (AVS.cardinal h) = 1 then
            let v, cst = AVS.min_elt h in
            if Val.is_int v then
              let vi = Val.to_int v in
              if not (Int.is_top vi) then 
                let pvi = Int.to_int vi in
                let mk_struct = fun i str heap ->
                  if i = pvi then
                    str, heap
                  else
                    let f = string_of_int i in
                    let loc = Loc.new_const_loc () in
                    let ptr_loc = Loc.mk_pointer loc in
                    let str' = Struct.add f loc str in
                    let heap' = Heap.add loc (AVS.singleton (Val.of_loc ptr_loc, Cst.cst_true)) heap in
                    str', heap'
                in
                let str, heap' = mk_struct 0 Struct.empty heap in
                AVS.singleton (Val.of_struct str, Cst.cst_true), heap'
              else
                top_avs, heap
            else
              top_avs, heap
          else
            top_avs, heap
      | _ ->
          top_avs, heap
    in
    let heap'' = Heap.add (Env.find lhs_var env) avs heap' in
    Domain.make env heap'' logs

let models = [
  "__new_array", sem_new_array
]

let is_modeled f = 
  let fname = Typ.Procname.to_string f in
  Caml.List.exists (fun (name, _) -> name = fname) models

let apply_semantics lhs_var fname args_avs env heap logs =
  let _, handler = Caml.List.find (fun (name, _) -> name = fname) models in
  handler lhs_var args_avs env heap logs

