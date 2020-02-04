open! IStd
open Core
module F = Format
module L = Logging

(* Node Location Informations *)
module NodeLoc = struct
  (* id_t: id of node given by Infer *)
  type id_t = Procdesc.Node.id

  type elt =
    { fn: string; (* function name *)
      id: id_t; (* id from Infer *)
      sub_id: int; (* sub_id to distinguish splitted nodes *)
      idx: int (* index (order of abstract interpretation) *) }

  type t = elt list

  (* Constructor *)
  let rec mk: (string * id_t * int) list -> t =
    function [] -> []
           | (fn, id, idx) :: xs -> {fn; id; sub_id = 0; idx} :: mk xs

  (* Modifiers *)
  (* increment given location's index & sub_id *)
  let inc_idx: t -> t =
    function [] -> []
           | {fn; id; sub_id; idx} :: xs ->
               {fn; id; sub_id = sub_id + 1; idx = idx + 1} :: xs

  (* Comparison *)
  let rec mk_list_cmp cmp = function
    | [] -> (function [] -> 0
                    | _ :: _ -> -1)
    | x :: xs -> function
        | [] -> 1
        | y :: ys -> match cmp x y with 0 -> mk_list_cmp cmp xs ys
                                      | r -> r

  let compare_elt_by_idx { idx = idx1 } { idx = idx2 } = compare idx1 idx2

  let compare_by_idx  = mk_list_cmp compare_elt_by_idx

  let compare_elt_by_id
      { fn = fn1; id = id1; sub_id = sub1 }
      { fn = fn2; id = id2; sub_id = sub2 } =
    let c1 = compare fn1 fn2 in
    let c2 = compare id1 id2 in
    if c1 <> 0 then c1
    else if c2 <> 0 then c2
    else compare sub1 sub2

  let compare_by_id = mk_list_cmp compare_elt_by_id
      
  let compare_elt
      { fn = fn1; idx = idx1; id = id1; sub_id = sub1 }
      { fn = fn2; idx = idx2; id = id2; sub_id = sub2 } =
    let c1 = compare fn1 fn2 in
    let c2 = compare idx1 idx2 in
    let c3 = compare id1 id2 in
    if c1 <> 0 then c1
    else if c2 <> 0 then c2
    else if c3 <> 0 then c3
    else compare idx1 idx2

  let compare = mk_list_cmp compare_elt

  (* pp *)
  let rec pp_qt fmt = function
    | n when n > 0 -> F.fprintf fmt "'"; pp_qt fmt (n - 1)
    | _ -> ()
  let pp_elt fmt {fn; id; sub_id; idx} =
    F.fprintf fmt "%s:%a%a(%d)" fn Procdesc.Node.pp_id id pp_qt sub_id idx

  let pp fmt =
    let rec f s = function
      | [] -> ()
      | x :: xs -> (if s
          then F.fprintf fmt ":%a" pp_elt x
          else F.fprintf fmt  "%a" pp_elt x);
          f true xs
    in f false
end

(* Set of NodeLoc *)
module NodeLocSet = struct
  include PrettyPrintable.MakePPSet(NodeLoc)

  let to_list set = fold (fun e l -> e :: l) set []
end

(* Node for control flow graph *)
module Node = struct
  type 'a exp = EUnknown
              | EIsTrue of 'a
              | EIsFalse of 'a

  let exp_neg = function
    | EIsTrue loc -> EIsFalse loc
    | EIsFalse loc -> EIsTrue loc
    | x -> x

  type 'a node_kind =
  | KCommon
  | KCall (* Function call *)
  | KCallBegin (* Function call, succ is callee's initial node *)
  | KCallEnd (* Function call, pred is callee's terminal node *)
  | KPruneT of 'a exp (* True branch *)
  | KPruneF of 'a exp (* False branch *)
  | KEnd (* End of scope *)

  type 'a t =
    { kind: 'a node_kind;
      loc: NodeLoc.t;
      mutable succ: NodeLocSet.t;
      mutable pred: NodeLocSet.t }

  (* Constructors *)
  let mk kind loc succ_list pred_list =
    { kind;
      loc;
      succ = NodeLocSet.of_list succ_list;
      pred = NodeLocSet.of_list pred_list }

  let mk_on kind loc succ pred base_loc =
    { kind; loc = base_loc @ loc;
      succ = NodeLocSet.map (fun x -> base_loc @ x) succ;
      pred = NodeLocSet.map (fun x -> base_loc @ x) pred }

  (* Getters *)
  let get_kind { kind } = kind
  let get_loc { loc } = loc
  let get_succ_list { succ } = NodeLocSet.to_list succ
  let get_pred_list { pred } = NodeLocSet.to_list pred

  (* Modifiers *)
  let add_succ loc node =
    node.succ <- NodeLocSet.add loc node.succ
  let add_pred loc node =
    node.pred <- NodeLocSet.add loc node.pred

  (* Predicates *)
  let is_prune { kind } = match kind with
    | KPruneT _ | KPruneF _ -> true
    | _ -> false

  let is_pred_branch node =
    NodeLocSet.cardinal node.pred >= 2

  let is_succ_branch node =
    NodeLocSet.cardinal node.succ >= 2

  (* pp *)
  let pp_kind fmt t = match t with
    | KCommon -> F.fprintf fmt "Common"
    | KCall -> F.fprintf fmt "Call"
    | KCallBegin -> F.fprintf fmt "Call Begin"
    | KCallEnd -> F.fprintf fmt "Call End"
    | KPruneT _ -> F.fprintf fmt "True Prune"
    | KPruneF _ -> F.fprintf fmt "False Prune"
    | KEnd -> F.fprintf fmt "End"

  let pp fmt node =
    F.fprintf fmt "Node %a (type: %a)\n" NodeLoc.pp node.loc pp_kind node.kind;
    F.fprintf fmt "  <- %a\n" NodeLocSet.pp node.pred;
    F.fprintf fmt "  -> %a\n" NodeLocSet.pp node.succ
end

module Graph = struct
  type 'a t =
    { mutable nodes: 'a Node.t list; (* nodes *)
      mutable idx: int (* index counter *) }

  let mk_empty () = { nodes = []; idx = 0 }

  (* Index Allocator *)
  let set_idx i g = g.idx <- i; i
  let get_idx g = g.idx
  let alloc_idx g = let i = g.idx in g.idx <- g.idx + 2; i

  (* Find functions *)
  (* find_node_by_id: find node by id.
   *   If there are >=1 nodes having same id, choose one with the least idx *)
  let find_node_by_id loc g =
    let rec f res nodes =
      match nodes with
      | [] -> res
      | n :: ns -> if NodeLoc.compare_by_id n.Node.loc loc <> 0
          then f res ns
          else match res with
            | Some x when NodeLoc.compare_by_idx n.Node.loc x.Node.loc > 0 ->
                f res ns
            | _ -> f (Some n) ns
    in f None g.nodes

  (* find_node_by_id_gt_idx: find node by id.
   *   If there are >=1 nodes having same id, choose one with the greatest idx *)
  let find_node_by_id_gt_idx loc g =
    let rec f res nodes =
      match nodes with
      | [] -> res
      | n :: ns -> if NodeLoc.compare_by_id n.Node.loc loc <> 0
          then f res ns
          else match res with
            | Some x when NodeLoc.compare_by_idx n.Node.loc x.Node.loc < 0 ->
                f res ns
            | _ -> f (Some n) ns
    in f None g.nodes

  (* find_node: find node using the given location *)
  let find_node loc g =
    let rec f nodes =
      match nodes with
      | [] -> None
      | n :: ns -> if NodeLoc.compare n.Node.loc loc = 0
          then Some n
          else f ns
    in f g.nodes

  let get_a_succ node g =
    match NodeLocSet.to_list node.Node.succ with
    | [l] -> find_node l g
    | _ -> None

  (* Graph modifiers *)
  let add_node kind loc succ_list pred_list g =
    match find_node_by_id_gt_idx loc g with
    | None ->
        let node = Node.mk kind loc succ_list pred_list in
        g.nodes <- node :: g.nodes;
        node
    | Some node when 0 = NodeLoc.compare_by_idx node.Node.loc loc -> node
    | Some node ->
        let new_node = Node.mk kind loc succ_list [node.Node.loc] in
        node.succ <- NodeLocSet.of_list [loc];
        g.nodes <- new_node :: g.nodes;
        new_node

  let remove_node loc g =
    let rec f s nodes =
      match nodes with
      | [] -> List.rev s
      | n :: ns -> if NodeLoc.compare n.Node.loc loc = 0
          then List.rev_append s ns
          else f (n :: s) ns
    in g.nodes <- f [] g.nodes

  let sort g =
    g.nodes <- List.sort ~compare:(fun { Node.loc = loc1 } { Node.loc = loc2 } ->
        NodeLoc.compare_by_idx loc1 loc2) g.nodes

  let update_link_loc find g set =
    set |> NodeLocSet.map (fun l ->
      match find_node l g with
        | Some _ -> l
        | None ->
            match find l g with
            | None -> l
            | Some x -> x.Node.loc )

  let update_link_locs g =
    List.iter g.nodes ~f:(fun n ->
        n.Node.succ <- update_link_loc find_node_by_id g n.Node.succ;
        n.Node.pred <- update_link_loc find_node_by_id_gt_idx g n.Node.pred; ())

  (* Find Node *)
  let rec bfs (max_fn: 'a Node.t -> 'a Node.t -> bool) ext_lst (max, vis) g n = 
    if List.exists vis
        ~f:(fun loc -> 0 = NodeLoc.compare_by_idx n.Node.loc loc)
    then (max, vis)
    else
      let new_max = match max with
        | Some m when max_fn m n -> Some m
        | _ -> Some n in
      List.fold_right (ext_lst g n) 
        ~f: (fun loc v -> match find_node loc g with
            | None -> v
            | Some n' -> bfs max_fn ext_lst v g n')
        ~init: (new_max, n.Node.loc :: vis)

  let find_from n max_fn ext_lst g =
    bfs max_fn ext_lst (None, []) g n |> function
    | Some n, _ -> Some n
    | _ -> None
    
  let find_initial_from n =
    find_from n
      (fun a b -> NodeLoc.compare_by_idx a.Node.loc b.Node.loc < 0)
      (fun _ -> Node.get_pred_list)

  let find_terminal_from n =
    find_from n
      (fun a b -> NodeLoc.compare_by_idx a.Node.loc b.Node.loc > 0)
      (fun _ -> Node.get_succ_list)

  let find_initial g = match g.nodes with
    | [] -> None
    | x :: _ -> find_initial_from x g

  (* FCS: First Common Successor  *
  let get_strict_successor n g =
    let rec f = function
      | [] -> None
      | m :: ms -> match find_node m g with
        | None -> f ms
        | Some m -> if NodeLoc.compare_by_idx n.Node.loc m.Node.loc < 0
            then Some m
            else f ms in
    f (Node.get_succ_list n)
  
  let rec find_fcs na nb g =
    let c = NodeLoc.compare_by_idx na.Node.loc nb.Node.loc in
    if c = 0 then Some na
    else if c > 0 then find_fcs nb na g
    else match get_strict_successor na g with
      | None -> None
      | Some n -> find_fcs n nb g *)

  (* Merge (split `tgt`, connect `tgt_begin` -> src_g -> `tgt_end`) *)
  let rec add_nodes_as_subgraph nodes base_loc g =
    match nodes with
    | [] -> () (* DONE *)
    | Node.{ kind; loc; succ; pred } :: ns ->
        let new_n = Node.mk_on kind loc succ pred base_loc in
        g.nodes <- new_n :: g.nodes;
        add_nodes_as_subgraph ns base_loc g

  let merge g tgt src_g =
    match src_g.nodes with
    | [] -> (* Nothing to do *) ()
    | node :: _ ->
        let init = find_initial_from node src_g in
        let term = find_terminal_from node src_g in
        match init, term with
        | None, _ | _, None -> ()
        | Some i, Some t ->
            add_nodes_as_subgraph src_g.nodes tgt.Node.loc g;
            let i_loc = tgt.Node.loc @ i.Node.loc in
            let t_loc = tgt.Node.loc @ t.Node.loc in
            let i_node = find_node i_loc g in
            let t_node = find_node t_loc g in
            match i_node, t_node with
            | None, _ | _, None -> ()
            | Some i, Some t ->
                remove_node tgt.Node.loc g;
                let be_loc = tgt.Node.loc in
                let en_loc = NodeLoc.inc_idx be_loc in
                let be = Node.mk_on Node.KCallBegin
                                    be_loc
                                    (NodeLocSet.singleton i_loc)
                                    tgt.Node.pred
                                    [] in
                let en = Node.mk_on Node.KCallEnd
                                    en_loc
                                    tgt.Node.succ
                                    (NodeLocSet.singleton t_loc)
                                    [] in
                tgt.Node.succ |> NodeLocSet.iter (fun x ->
                    match find_node x g with
                    | None -> ()
                    | Some n ->
                        n.Node.pred <- n.Node.pred
                      |> NodeLocSet.remove tgt.Node.loc
                      |> NodeLocSet.add en_loc);
                Node.add_pred be_loc i;
                Node.add_succ en_loc t;
                g.nodes <- be :: en :: g.nodes

  (* pp *)
  let rec pp_nodes fmt nodes = match nodes with
    | [] -> F.fprintf fmt "End\n"
    | n :: ns -> F.fprintf fmt "%a\n" Node.pp n; pp_nodes fmt ns

  let pp fmt g = F.fprintf fmt "Graph: \n%a\n" pp_nodes g.nodes

  (* Dot *)
  let export_dot_loc fmt loc =
    F.fprintf fmt "\"%a\"" NodeLoc.pp loc
  let export_dot_node fmt node =
    export_dot_loc fmt node.Node.loc;
    let shape = match node.Node.kind with
      | Node.KCommon -> "ellipse"
      | Node.KCall -> "diamond" 
      | Node.KCallBegin -> "triangle" 
      | Node.KCallEnd -> "invtriangle" 
      | Node.KPruneT _ -> "larrow"
      | Node.KPruneF _ -> "rarrow"
      | Node.KEnd -> "box" in
    F.fprintf fmt "[label=%a shape=%s];"
      export_dot_loc node.Node.loc shape;
    node.Node.succ |> NodeLocSet.iter (fun succ ->
        F.fprintf fmt "%a -> %a;"
          export_dot_loc node.Node.loc
          export_dot_loc succ )
          
  let rec export_dot_nodes fmt nodes = match nodes with
    | [] -> ()
    | n :: ns ->
        export_dot_node fmt n;
        export_dot_nodes fmt ns
  let export_dot fmt g =
    F.fprintf fmt "digraph graphA { %a }\n@." export_dot_nodes g.nodes
end

module BlockReconsistution = struct
  let node_has_multiple_pred node =
    NodeLocSet.cardinal node.Node.pred >= 2

  let node_has_multiple_succ node =
    NodeLocSet.cardinal node.Node.succ >= 2

  type pred_kind = PDoWhile of NodeLoc.t
                 | PWhile of NodeLoc.t
  type pred_kinds = pred_kind list
  let get_node_pred_kinds node g =
    NodeLocSet.fold (fun loc (l: pred_kinds) ->
        if NodeLoc.compare node.Node.loc loc >= 0
        then l
        else match Graph.find_node loc g with
          | None -> l
          | Some x when Node.is_prune x -> PDoWhile loc :: l
          | _ -> PWhile loc :: l ) node.Node.pred []

  let rec find_next_node locs g =
    let sorted = List.sort NodeLoc.compare locs in
    match sorted with
    | [] -> None
    | [l] -> Graph.find_node l g
    | l1 :: l2 :: ls when 0 = NodeLoc.compare l1 l2 -> find_next_node (l2 :: ls) g
    | l :: ls -> match Graph.find_node l g with
      | None -> find_next_node ls g
      | Some x -> match next_node x g with
        | None -> None
        | Some x' -> if NodeLoc.compare x.Node.loc x'.Node.loc > 0
            then find_next_node ls g
            else find_next_node (x'.Node.loc :: ls) g
  and next_node node g =
    if node_has_multiple_succ node
    then find_next_node (NodeLocSet.to_list node.Node.succ) g
    else Graph.get_a_succ node g

  type range = NodeLoc.t * NodeLoc.t
  and block = BSimple of range
            | BIfElse of blocks * blocks
            | BWhile of blocks
            | BDoWhile of blocks
  and blocks = block list

end

