open! IStd
open Core
module F = Format
module L = Logging
module Sem = Semanticfunctions
module TH = TypeHandler
open SemanticSummaryDomain
open SUtils

module NodeLoc = struct
  type id_t = Procdesc.Node.id

  type elt =
    { fn: string;
      id: id_t;
      idx: int }

  type t = elt list

  (* Constructor *)
  let rec mk = function
    | [] -> []
    | (fn, id, idx) :: xs -> {fn; id; idx} :: mk xs

  let rec mk_list_cmp cmp = function
    | [] -> (function [] -> 0
                    | y :: ys -> -1)
    | x :: xs -> function
        | [] -> 1
        | y :: ys -> match cmp x y with 0 -> mk_list_cmp cmp xs ys
                                      | r -> r

  (* Compare *)
  let compare_elt_by_idx { idx = idx1 } { idx = idx2 } = compare idx1 idx2

  let compare_by_idx  = mk_list_cmp compare_elt_by_idx

  let compare_elt { fn = fn1; id = id1 }
                  { fn = fn2; id = id2 } =
    let c1 = compare fn1 fn2 in
    if c1 = 0 then compare id1 id2
              else c1

  let rec compare = mk_list_cmp compare_elt

  (* pp *)
  let pp_elt fmt {fn; id; idx} =
    F.fprintf fmt "%s:%a" fn Procdesc.Node.pp_id id

  let pp fmt =
    let rec f s = function
      | [] -> ()
      | x :: xs -> (if s
          then F.fprintf fmt ":%a" pp_elt x
          else F.fprintf fmt  "%a" pp_elt x);
          f true xs
    in f false
end

module NodeLocSet = PrettyPrintable.MakePPSet(NodeLoc)

module Node = struct
  type node_kind =
  | KCommon
  | KCall
  | KPruneT
  | KPruneF
  | KEnd

  type t = {
    kind: node_kind;
    loc: NodeLoc.t;
    mutable succ: NodeLocSet.t;
    mutable pred: NodeLocSet.t }

  let loc_set_to_list set = NodeLocSet.fold (fun e l -> e :: l) set []

  (* Constructors *)
  let mk kind loc =
    { kind;
      loc;
      succ = NodeLocSet.empty;
      pred = NodeLocSet.empty }

  (* Getters *)
  let get_kind { kind } = kind
  let get_loc { loc } = loc
  let get_succ_list { succ } = loc_set_to_list succ
  let get_pred_list { pred } = loc_set_to_list pred

  (* Modifiers *)
  let add_succ loc node =
    node.succ <- NodeLocSet.add loc node.succ

  let add_pred loc node =
    node.pred <- NodeLocSet.add loc node.pred

  (* pp *)
  let pp_kind fmt t = match t with
    | KCommon -> F.fprintf fmt "Common"
    | KCall -> F.fprintf fmt "Call"
    | KPruneT -> F.fprintf fmt "True Prune"
    | KPruneF -> F.fprintf fmt "False Prune"
    | KEnd -> F.fprintf fmt "End"

  let pp fmt node =
    F.fprintf fmt "Node %a (type: %a)\n" NodeLoc.pp node.loc pp_kind node.kind;
    F.fprintf fmt "  <- %a\n" NodeLocSet.pp node.pred;
    F.fprintf fmt "  -> %a\n" NodeLocSet.pp node.succ
end

module Graph = struct
  type t = {
    mutable nodes: Node.t list;
    mutable idx: int }

  let init () = { nodes = []; idx = 0 }

  (* Index Allocator *)
  let set_idx i g = g.idx <- i; i

  let get_idx g = g.idx

  let alloc_idx g =
    let i = g.idx in
    g.idx <- g.idx + 1;
    i

  (* Graph *)

  let find_node loc g =
    let rec f nodes =
      match nodes with
      | [] -> None
      | n :: ns -> if NodeLoc.compare n.Node.loc loc = 0
          then Some n
          else f ns
    in f g.nodes

  let add_node (kind, loc) g =
    match find_node loc g with
    | Some node -> node
    | None ->
        let node = Node.mk kind loc in
        g.nodes <- node :: g.nodes;
        node

  let add_succ loc node =
    node.Node.succ <- NodeLocSet.add loc node.Node.succ

  let add_pred loc node =
    node.Node.pred <- NodeLocSet.add loc node.Node.pred

  let sort g =
    g.nodes <- List.sort (fun { Node.loc = loc1 } { Node.loc = loc2 } ->
        NodeLoc.compare_by_idx loc1 loc2) g.nodes

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
      | Node.KPruneT -> "larrow"
      | Node.KPruneF -> "rarrow"
      | Node.KEnd -> "box" in
    F.fprintf fmt "[label=%a shape=%s];"
      export_dot_loc node.Node.loc shape;
    node.succ |> NodeLocSet.iter (fun succ ->
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
