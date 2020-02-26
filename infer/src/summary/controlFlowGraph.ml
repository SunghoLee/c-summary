open! IStd
open Core
module F = Format
module L = Logging

(* Node Location Informations *)
module NodeLoc = struct
  type elt =
    { fn: string; (* function name *)
      id: int; (* id from Infer *)
      sub_id: int; (* sub_id to distinguish splitted nodes *)
      idx: int (* index (order of abstract interpretation) *) }
  (* sub_id exists for splitting a Call node into two nodes:
   * CallBegin and CallEnd. In the case, CallBegin.id = 0
   * and CallEnd.id = 1. Because of similar reason, idx increases
   * by 2 when idx allocation, only CallEnd.idx is odd
   * and all other idx are even, and CallEnd.idx = CallBegin.idx + 1. *)

  (* NodeLoc is a non-empty list of locations of nodes.
   * The 1st elem is a caller of the 2nd one,
   * the 2nd is a caller of the 3rd, ...
   * The last elem is ORIGINAL location of the node. *)
  type t = elt list

  (* Constructor
   * Take list of (fun name, node id, allocated idx) *)
  let rec mk: (string * int * int) list -> t =
    function [] -> []
           | (fn, id, idx) :: xs -> {fn; id; sub_id = 0; idx} :: mk xs

  (* Modifiers *)
  (* increment given location's index & sub_id.
   * Arg is CallBegin's loc, and the return is CallEnd's loc *)
  let inc_idx: t -> t =
    function [] -> []
           | {fn; id; sub_id; idx} :: xs ->
               {fn; id; sub_id = sub_id + 1; idx = idx + 1} :: xs
               
  (* decrease given location's index & sub_id.
   * If the given arg is CallEnd's loc, it'll return CallBegin's loc
   * Otherwise, it'll return the given arg *)
  let base_idx: t -> t =
    function [] -> []
           | {fn; id; sub_id; idx} :: xs ->
               {fn; id; sub_id = 0; idx = idx - sub_id} :: xs

  (* Comparisons
   * compare: full comparison; order: fn -> idx -> id -> sub_id
   * compare_by_idx: compare idx only; order: idx
   * compare_by_id: compare by fn and id; order: fn -> id *)
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
      { fn = fn1; id = id1 }
      { fn = fn2; id = id2 } =
    let c1 = compare fn1 fn2 in
    let c2 = compare id1 id2 in
    if c1 <> 0 then c1
    else compare id1 id2

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
    F.fprintf fmt "%s:%d%a(%d)" fn id pp_qt sub_id idx

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
module NodeLocSet = PrettyPrintable.MakePPSet(NodeLoc)

(* Expression to denote condition of prune *)
module Exp = struct
  type t = Unknown (* Unknown/inexpressible exp *)
         | IsTrue of string (* true if the var contains non-zero(true) *)
         | IsFalse of string (* true if the var contains zero(false) *)

  (* negate the given expression *)
  let neg = function
    | IsTrue loc -> IsFalse loc
    | IsFalse loc -> IsTrue loc
    | x -> x

  let to_string to_s top = function
    | Unknown -> top
    | IsTrue e -> "__isTrue(" ^ to_s e ^ ")"
    | IsFalse e -> "__isFalse(" ^ to_s e ^ ")"
end

(* Node for control flow graph *)
module Node = struct
  type node_kind =
  | KCommon
  | KCall (* Function call *)
  | KCallBegin (* Function call, succ is callee's initial node *)
  | KCallEnd (* Function call, pred is callee's terminal node *)
  | KPruneT of Exp.t (* True branch *)
  | KPruneF of Exp.t (* False branch *)
  | KEnd (* End of scope *)

  type t =
    { kind: node_kind;
      loc: NodeLoc.t;
      mutable succ: NodeLocSet.t;
      mutable pred: NodeLocSet.t;
      (* (lazily calculated) common successor
       * - None: Not calculated yet
       * - Some x: Already calculated and the result is x *)
      mutable cs: NodeLoc.t option option }

  (* Constructors *)
  let dummy loc =
    { kind = KCommon;
      loc;
      succ = NodeLocSet.empty;
      pred = NodeLocSet.empty;
      cs = None }

  let mk kind loc succ_list pred_list =
    { kind;
      loc;
      succ = NodeLocSet.of_list succ_list;
      pred = NodeLocSet.of_list pred_list;
      cs = None }

  (* Make a callee's node which is called at base_loc *)
  let mk_on kind loc succ pred base_loc =
    { kind; loc = base_loc @ loc;
      succ = NodeLocSet.map (fun x -> base_loc @ x) succ;
      pred = NodeLocSet.map (fun x -> base_loc @ x) pred;
      cs = None }

  (* Getters *)
  let get_kind { kind } = kind
  let get_loc { loc } = loc
  let get_succ_list { succ } = NodeLocSet.elements succ
  let get_pred_list { pred } = NodeLocSet.elements pred

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

  (* Comparisons *)
  let compare n1 n2 = NodeLoc.compare n1.loc n2.loc

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

module NodeSet = PrettyPrintable.MakePPSet(Node)

module Graph = struct
  type idx_pool_elem =
    { cardinal: int; (* # of instrs of the node *)
      base_idx: int; (* the smallest idx for the node *)
      mutable counter: int; (* index of given instr *)
      mutable is_done: bool (* whether allocation is done *) }
  type idx_pool = idx_pool_elem option Array.t

  type t =
    { mutable nodes: NodeSet.t; (* nodes *)
      mutable idx: int; (* index counter *)
      mutable pool: idx_pool;
      mutable containing_jni_call: bool }

  (* Construct *)
  let mk_empty () =
    { nodes = NodeSet.empty;
      idx = 0;
      pool = Array.create 0 None;
      containing_jni_call = false }

  (* Index Allocator *)
  let extend_pool len g =
    let m = len - Array.length g.pool in
    g.pool <- Array.append g.pool (Array.create m None)

  let alloc_idx' e =
    let idx = e.base_idx + 2 * e.counter in
    if (e.counter + 1) >= e.cardinal
    then ( e.counter <- 0;
           e.is_done <- true )
    else ( e.counter <- e.counter + 1 );
    idx

  let alloc_idx id n_instrs g =
    let cardinal = if n_instrs < 1 then 1 else n_instrs in
    if id >= Array.length g.pool
    then extend_pool (id + 1) g;
    match Array.get g.pool id with
    | None ->
        let e = { cardinal;
                  base_idx = g.idx;
                  counter = 0;
                  is_done = false } in
        Array.set g.pool id (Some e);
        g.idx <- g.idx + (2 * cardinal);
        (false, alloc_idx' e)
    | Some e ->
        let is_done = e.is_done in
        (is_done, alloc_idx' e)

  (* has_jni flag *)
  let has_jni g = g.containing_jni_call
  let mark_jni g = g.containing_jni_call <- true

  (* Find functions *)
  let find_nodes_by_id loc g =
    let f n = NodeLoc.compare_by_id n.Node.loc loc = 0 in
    NodeSet.filter f g.nodes

  (* find_node_by_id: find node by id.
   *   If there are >=1 nodes having same id, choose one with the least idx *)
  let find_node_by_id loc g =
    find_nodes_by_id loc g |> NodeSet.min_elt_opt

  (* find_node_by_id_gt_idx: find node by id.
   *   If there are >=1 nodes having same id, choose one with the greatest idx *)
  let find_node_by_id_last loc g =
    find_nodes_by_id loc g |> NodeSet.max_elt_opt

  (* find_node: find node using the given location *)
  let find_node loc g = NodeSet.find_opt (Node.dummy loc) g.nodes

  (* get_a_succ: choose arbitrary successor NODE of the given node *)
  let get_a_succ node g =
    match NodeLocSet.choose_opt node.Node.succ with
    | Some l -> find_node l g
    | None -> None

  (* Graph modifiers *)
  let add_node kind loc succ_list pred_list g =
    match find_node_by_id_last loc g with
    | Some node ->
        let new_node = Node.mk kind loc succ_list [node.Node.loc] in
        node.succ <- NodeLocSet.singleton loc;
        g.nodes <- NodeSet.add new_node g.nodes;
        new_node
    | None ->
        let node = Node.mk kind loc succ_list pred_list in
        g.nodes <- NodeSet.add node g.nodes;
        node

  let remove_node loc g =
    g.nodes <- NodeSet.remove (Node.dummy loc) g.nodes

  (* Update link
   * After call add_node, some nodes pointing dummy node location (idx = -1)
   * as succs/preds. update_link_locs will change this dummy location
   * into node location. (Loc in succs -> the first node with the given id,
   * Loc in preds -> the last node with the given id) *)
  let update_link_loc find g set =
    set |> NodeLocSet.map (fun l ->
      match find_node l g with
        | Some _ -> l
        | None -> match find l g with
            | None -> l
            | Some x -> x.Node.loc )

  let update_link_locs g =
    g.nodes |> NodeSet.iter (fun n ->
        n.Node.succ <- update_link_loc find_node_by_id g n.Node.succ;
        n.Node.pred <- update_link_loc find_node_by_id_last g n.Node.pred; ())

  (* Find Node *)
  let find_initial g = NodeSet.min_elt_opt g.nodes
  let find_terminal g = NodeSet.max_elt_opt g.nodes

  (* Merge (split `tgt`, connect `tgt_begin` -> src_g -> `tgt_end`) *)
  let add_nodes_as_subgraph nodes base_loc g =
    nodes |> NodeSet.iter
      (function Node.{ kind; loc; succ; pred } ->
          let new_n = Node.mk_on kind loc succ pred base_loc in
          g.nodes <- NodeSet.add new_n g.nodes)

  let merge g tgt src_g =
    if NodeSet.is_empty g.nodes || tgt.Node.kind <> Node.KCall
    then ()
    else
      let init = find_initial src_g in
      let term = find_terminal src_g in
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
                                  (NodeLocSet.singleton i.Node.loc)
                                  tgt.Node.pred
                                  [] in
              let en = Node.mk_on Node.KCallEnd
                                  en_loc
                                  tgt.Node.succ
                                  (NodeLocSet.singleton t.Node.loc)
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
              g.nodes <- g.nodes |> NodeSet.add en |> NodeSet.add be

  (* pp *)
  let pp_nodes fmt nodes =
    nodes |> NodeSet.iter (fun node -> F.fprintf fmt "%a\n" Node.pp node)

  let pp fmt g = F.fprintf fmt "Graph: \n%a\nEnd\n" pp_nodes g.nodes

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
        F.fprintf fmt "%a -> %a"
            export_dot_loc node.Node.loc
            export_dot_loc succ;
        if NodeLoc.compare_by_idx node.Node.loc succ < 0
        then F.fprintf fmt ";"
        else F.fprintf fmt " [style=dashed];");
    match node.Node.cs with
    | Some (Some cs) ->
        F.fprintf fmt "%a -> %a [color=red];"
            export_dot_loc node.Node.loc
            export_dot_loc cs
    | _ -> ()
          
  let rec export_dot_nodes fmt nodes =
    nodes |> NodeSet.iter (export_dot_node fmt)
  let export_dot name fmt g =
    F.fprintf fmt "digraph %s { %a }\n@." name export_dot_nodes g.nodes
end

module GraphHelper = struct
  let sort_locs_by_idx lst = List.sort ~compare:NodeLoc.compare_by_idx lst

  let compare_node_by_idx x y = NodeLoc.compare_by_idx x.Node.loc y.Node.loc

  let sort_nodes_by_idx lst = List.sort ~compare:compare_node_by_idx lst

  let get_inc_succ_list node =
    let f x = NodeLoc.compare_by_idx node.Node.loc x < 0 in
    Node.get_succ_list node
    |> List.filter ~f: f
    |> sort_locs_by_idx

  let get_dec_pred_list node =
    let f x = NodeLoc.compare_by_idx node.Node.loc x > 0 in
    Node.get_pred_list node
    |> List.filter ~f: f
    |> sort_locs_by_idx

  let loc_list_to_node_list lst g =
    let f nodes loc = match Graph.find_node loc g with
      | None -> nodes
      | Some x -> x :: nodes
    in List.fold_left ~f:f ~init:[] lst |> List.rev

  let get_inc_succ_node_list node g =
    loc_list_to_node_list (get_inc_succ_list node) g

  let get_dec_pred_node_list node g =
    loc_list_to_node_list (get_dec_pred_list node) g

  let rec add_node_to_slist n lst = match lst with
    | [] -> [n]
    | x :: xs ->
        let c = NodeLoc.compare_by_idx x.Node.loc n.Node.loc in
        if c > 0
        then x :: add_node_to_slist n xs
        else if c = 0 then lst else n :: lst

  let append_succs_of node g lst = 
    let succs = get_inc_succ_list node in
    let f lst loc =
      match Graph.find_node loc g with
      | None -> lst
      | Some x -> add_node_to_slist x lst
    in List.fold_left ~f:f ~init:lst succs

  let rec find_next_prune g loc =
    match Graph.find_node loc g with
    | None -> None
    | Some n ->
        match n.Node.kind with
        | Node.KPruneT v -> Some n
        | Node.KPruneF v -> Some n
        | _ -> match Node.get_succ_list n with
          | [x] -> find_next_prune g x
          | _ -> None

  let rec find_last_branch g loc =
    match Graph.find_node loc g with
    | None -> None
    | Some n ->
        match Node.get_succ_list n with
        | [x; y] -> Some loc
        | _ -> match Node.get_pred_list n with
          | [x] -> find_last_branch g x
          | _ -> None


  let rec find_cs g a b =
    (* Check cs was already calculated *)
    let cmp = NodeLoc.compare a b in
    if cmp = 0
    then Some a
    else let f, s = if cmp < 0 then a, b else b, a in
         match Graph.find_node f g with
         | None -> None
         | Some x -> match x.Node.cs with
           | None ->
               update_common_succ g f;
               find_cs g f s
           | Some (Some y) when NodeLoc.compare f y < 0 ->
               find_cs g y s
           | _ -> None

  and mark_locs g (loc, set) =
    let set' = NodeLocSet.add loc set in
    match Graph.find_node loc g with
    | None -> set'
    | Some x -> match x.Node.cs with
      | None -> update_common_succ g loc; mark_locs g (loc, set')
      | Some (Some y) when NodeLoc.compare loc y < 0 -> mark_locs g (y, set')
      | _ -> set'
            
  and update_common_succ g loc =
    (* Check cs was already calculated *)
    match Graph.find_node loc g with
    | None -> ()
    | Some x when x.Node.kind = KCallBegin ->
      ( match x.Node.cs with
      | Some _ -> ()
      | None ->
          NodeLocSet.iter (update_common_succ g) x.Node.succ;
          (match Graph.find_node (NodeLoc.inc_idx loc) g with
          | None ->
              x.Node.cs <- Some None; ()
          | Some y ->
              x.Node.cs <- Some (Some y.Node.loc);
              update_common_succ g y.Node.loc) )
    | Some x ->
      match x.Node.cs with
      | Some _ -> () (* DONE *)
      | None -> (* was not calculated because of lazyness *)
        match Node.get_succ_list x with
        | [] ->
            x.Node.cs <- Some None; ()
        | [s] -> x.Node.cs <- Some (Some s); (* Simple update *)
                 update_common_succ g s
        | y :: ys ->
            NodeLocSet.iter (update_common_succ g) (x.Node.succ);
            let rec f l rs = match rs with
              | [] -> Some l
              | z :: zs -> match find_cs g l z with
                | None -> None
                | Some t -> f t zs in
            x.Node.cs <- Some (f y ys); ()

  let update_common_successors g =
    match Graph.find_initial g with
    | None -> ()
    | Some x -> update_common_succ g x.Node.loc
end
