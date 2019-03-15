open SemanticSummaryDomain
open Pervasives
module Helper = HelperFunction

let handle_array_lookup: AVS.t -> AVS.t -> AVS.t =
  fun arr_avs index_avs ->
    let handle: AVS.t -> (ValCst.t * ValCst.t) -> AVS.t =
      fun avs ((arr_val, arr_cst), (index_val, index_cst)) ->
        match arr_val, Val.to_int index_val with
        | Struct str, Int i ->
            string_of_int i
            |> (fun x -> Struct.find x str)
            |> Val.of_loc
            |> (fun x -> x, Cst.cst_and arr_cst index_cst)
            |> (fun x -> AVS.add x avs)
        | Struct str, Top ->
            (fun _ loc avs ->
              Val.of_loc loc
              |> (fun x -> x, Cst.cst_and arr_cst index_cst) 
              |> (fun x -> AVS.add x avs))
            |> (fun x -> Struct.fold x str AVS.empty)
        | Loc loc, Int i ->         
            Loc.mk_offset loc i
            |> Val.of_loc
            |> (fun x -> x, Cst.cst_and arr_cst index_cst)
            |> (fun x -> AVS.add x avs)
        | _ -> 
            failwith "Not implemented yet!"

    in
    Helper.(arr_avs * index_avs)
    |> (fun x -> Caml.List.fold_left handle AVS.empty x)
