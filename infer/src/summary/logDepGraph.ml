open! IStd
open Core
module F = F
module L = Logging
module Domain = SemanticSummaryDomain
open Domain

module Node = struct 
  type node_t = HEAD | END | Node of LogUnit.t
  [@@deriving compare]

  type t = 
    { g_head: node_t
    ; g_end: node_t
    ; n: node_t
    ; mutable succ: t list 
    ; mutable pred: t list }
  [@@deriving compare]

  let head_node () = 
    { g_head = HEAD
    ; g_end = END
    ; n = HEAD
    ; succ = []
    ; pred = []
    }

  let end_node () = 
    { g_head = HEAD
    ; g_end = END
    ; n = END
    ; succ = []
    ; pred = []
    }

  let mk_init log = 
    { g_head = HEAD
    ; g_end = END
    ; n = Node log 
    ; succ = []
    ; pred = []
    }

  let is_head {n} = match n with HEAD -> true | _ -> false

  let is_end {n} = match n with END -> true | _ -> false

  let ( <= ) n_to n_from = 
    n_to.pred <- n_from :: n_to.pred;
    n_from.succ <- n_to :: n_from.succ;
    n_to 

  let ( => ) n_from n_to =
    let _ = n_to <= n_from in
    n_from
    
  let ( *<= ) ns_to n_from = 
    let iter_to = fun n_to ->
      n_to.pred <- n_from :: n_to.pred;
      n_from.succ <- n_to :: n_from.succ;
      n_to
    in
    Caml.List.map iter_to ns_to

  let ( <=* ) n_to ns_from =
    let iter_from = fun n_from ->
      n_to.pred <- n_from :: n_to.pred;
      n_from.succ <- n_to :: n_from.succ;
      n_from
    in
    let _ = Caml.List.map iter_from ns_from in
    n_to

  let ( *=> ) ns_from n_to = 
    let _ = n_to <=* ns_from in
    ns_from

  let ( =>* ) n_from ns_to =
    let _ = ns_to *<= n_from in
    n_from

  let to_string { n } =
    match n with
    | HEAD ->
        F.asprintf "HEAD"
    | END ->
        F.asprintf "END"
    | Node log ->
        F.asprintf "%a" LogUnit.pp log 
end

let update_succ node succ =
  node.Node.succ <- succ; node

let update_pred node pred =
  node.Node.pred <- pred; node

let add_succ node succ =
  update_succ node (succ :: node.Node.succ)

let add_pred node pred =
  update_pred node (pred :: node.Node.pred)

let add_from_to from_n to_n =
  let _ = add_succ from_n to_n in
  let _ = add_pred to_n from_n in
  ()

let iter_succ f node =
  Caml.List.iter f node.Node.succ

let iter_pred f node =
  Caml.List.iter f node.Node.pred

let get_pred node = node.Node.pred

let get_succ node = node.Node.succ

let get_head node = node.Node.g_head

let get_end node = node.Node.g_end

let get_current node = node.Node.n

let mk_init = Node.mk_init

let map f head = 
  let module Visited = Caml.Set.Make(Node) in
  let rec map_f = fun (visited, f_list) node ->
    if Visited.mem node visited then
      visited, f_list
    else
      let visited' = Visited.add node visited in
      let succ = get_succ node in
      Caml.List.fold_left map_f (visited', (f node) :: f_list) succ
  in
  let _, f_list = map_f (Visited.empty, []) head in
  Caml.List.rev f_list

let get_logunit node = 
  match node.Node.n with
  | Node l ->
      l
  | _ ->
      failwith "This is a special node; cannot unwrapp out this."

let mk_ldg logs =
  let nodes = 
    CallLogs.fold (fun log log_list -> (mk_init log) :: log_list) logs []
  in
  let is_in_heap = fun loc node ->
    let log = get_logunit node in
    let heap = log.LogUnit.heap in
    let iter_heap = fun loc' v ->
      if loc = loc' then 
        true
      else 
        Val.exists (fun (l, _) -> l = loc) v
    in
    Heap.exists iter_heap heap
  in
  let connect_edges = fun node ->
    let log = get_logunit node in
    let ret = log.LogUnit.rloc in
    let iter_node = fun node' ->
      if is_in_heap ret node' then
        let _ = Node.(node => node') in
        ()
    in
    let () = Caml.List.iter iter_node nodes in
    node
  in
  let nodes' = Caml.List.map connect_edges nodes in
  let rm_dup = fun node ->
    let pred = get_pred node in
    let succ = get_succ node in
    let pred' = Caml.List.sort_uniq (fun a b -> Node.compare a b) pred in
    let succ' = Caml.List.sort_uniq (fun a b -> Node.compare a b) pred in
    let node' = update_pred node pred' in
    let node'' = update_pred node' succ' in
    node''
  in
  let nodes'' = Caml.List.map rm_dup nodes' in
  let no_pred_nodes = 
    Caml.List.filter (fun x -> (Caml.List.length (get_pred x)) = 0) nodes''
  in
  let no_succ_nodes =
    Caml.List.filter (fun x -> (Caml.List.length (get_succ x)) = 0) nodes''
  in
  let head_n = Node.head_node () in
  let end_n = Node.end_node () in
  let end_n' = Node.(no_succ_nodes *=> end_n) in
  Node.(head_n =>* no_pred_nodes)

module DotPrinter = struct

  module DotGraph = struct

    module Shape = struct
      type t = RECORD | CIRCLE

      let record = RECORD

      let circle = CIRCLE

      let pp fmt = function
        | RECORD -> 
          F.fprintf fmt "record"
        | CIRCLE -> 
            F.fprintf fmt "circle"
    end

    module Label = struct
      type t = int * string

      let mk_label index desc = 
        Str.global_replace (Str.regexp_string "{") "\{" desc
        |> Str.global_replace (Str.regexp_string "}") "\}"
        |> Str.global_replace (Str.regexp_string ">") "\>"
        |> Str.global_replace (Str.regexp_string "<") "\<"
        |> (fun x -> index, x)

      let pp fmt (index, desc) = 
        F.fprintf fmt "%d [label=\"%s\"]" index desc
    end

    module Edge = struct
      type t = int * int

      let ( => ) n_from n_to = n_from n_to

      let pp fmt (n_from, n_to) =
        F.fprintf fmt "%d -> %d" n_from n_to
    end

    type t = 
      { shape: Shape.t
      ; labels: Label.t list
      ; edges: Edge.t list }

    module IndexMap = Caml.Map.Make(Node)

    let index = ref 1

    let new_index () = (index := !index + 1; !index - 1)

    let to_dot_graph head = 
      let new_index_to_node n imap =
        match IndexMap.find_opt n imap with
        | Some _ -> 
            false, imap
        | None ->
            let nid = new_index () in
            let imap' = IndexMap.add n nid imap in
            true, imap'
      in
      let get_index n imap = IndexMap.find n imap in
      let rec assign_index = fun imap n ->
        let cont, imap' = new_index_to_node n imap in
        if cont then
          let succ = get_succ n in
          Caml.List.fold_left assign_index imap' succ 
        else
          imap'
      in
      let imap = assign_index IndexMap.empty head in
      let node_to_label = fun node ->
        let index = get_index node imap in
        Label.mk_label index (Node.to_string node)
      in
      let node_to_edge = fun node ->
        let succ = get_succ node in
        let index = get_index node imap in
        Caml.List.fold_left (fun edges node' -> (index, get_index node' imap) :: edges) [] succ
      in
      let shape = Shape.record in
      let labels = map node_to_label head in
      let edges = Caml.List.flatten (map node_to_edge head) in
      { shape = shape; labels = labels; edges = edges }

      let pp fmt {shape; labels; edges} = 
        let rec pp_list pp_f fmt = function
          | [] -> 
              ()
          | h :: [] ->
              F.fprintf fmt "%a" pp_f h
          | h :: t ->
              F.fprintf fmt "%a\n%a" pp_f h (pp_list pp_f) t
        in
        F.fprintf fmt "digraph{\n node [shape=%a];\n\n %a\n\n %a\n }" 
          Shape.pp shape (pp_list Label.pp) labels (pp_list Edge.pp) edges
  end
end


