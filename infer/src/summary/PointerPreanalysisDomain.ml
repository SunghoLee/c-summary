open! IStd
open Core
open Pervasives
module F = Format
module L = Logging

module LocHolder = struct
  type t = AddrHolder of SemanticSummaryDomain.Loc.t
  | Holder of SemanticSummaryDomain.Loc.t
  | None
  [@@deriving compare]

  let none = None

  let mk_addr_holder (l: SemanticSummaryDomain.Loc.t) =
    match l with
    | Explicit _ | Implicit _ | Const _ | FunPointer _ | Ret _ | Offset _ -> AddrHolder l
    | Pointer (base, _, _) -> Holder base
    | LocTop -> failwith "Pointer analysis does not handle Location Top!"

  let mk_holder (l: SemanticSummaryDomain.Loc.t) = Holder l

  let is_primitive = function
    | Holder loc -> (SemanticSummaryDomain.Loc.mk_implicit "primitive") = loc
    | _ -> false

  let pp fmt = function
    | AddrHolder l ->
        F.fprintf fmt "&(%a)" SemanticSummaryDomain.Loc.pp l
    | Holder l ->
        F.fprintf fmt "%a" SemanticSummaryDomain.Loc.pp l
    | None -> 
        F.fprintf fmt "None" 
end

module type Context = sig
  type t
  val empty : t
  val make : LocHolder.t -> CallSite.t -> t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
end

let ctxt_depth = 1

module EveryWhere : Context = struct
  type t = EveryWhere
  [@@deriving compare]

  let empty = EveryWhere

  let make receiver callsite = EveryWhere

  let equal lhs rhs = true

  let pp fmt ctxt = F.fprintf fmt "EveryWhere"
end

module OBJContext : Context = struct
  type t = None
  | OBJ of LocHolder.t
  [@@deriving compare]

  let empty = None

  let make receiver callsite = OBJ receiver

  let equal lhs rhs = (compare lhs rhs) = 0

  let pp fmt = function
    | None -> 
        F.fprintf fmt " "
    | OBJ pk -> 
        F.fprintf fmt "%a" LocHolder.pp pk
end

module CSContext : Context = struct
  type t = None
  | CS of CallSite.t
  [@@deriving compare]

  let empty = None 

  let make receiver callsite = CS callsite

  let equal lhs rhs = (compare lhs rhs) = 0

  let pp fmt = function
    | None ->
        F.fprintf fmt " "
    | CS cs ->
        F.fprintf fmt "%a" CallSite.pp cs
end

module VVar = struct
  include SemanticSummaryDomain.VVar
end

module Loc = struct
  include SemanticSummaryDomain.Loc

  let primitive = mk_implicit "primitive"
end

(* Choose Context for Pointer analysis *)
module Ctxt = CSContext 

module PointerKey = struct
  type t = { holder: LocHolder.t; mutable ctxt: Ctxt.t list }
  [@@deriving compare]

  let mk_lpk_w_ctxt ctxt l = { holder = LocHolder.mk_addr_holder l; ctxt }

  let mk_pk_w_ctxt ctxt l = { holder = LocHolder.mk_holder l; ctxt }

  let mk_lpk (l: Loc.t) = mk_lpk_w_ctxt [] l

  let mk_pk (l: Loc.t) = mk_pk_w_ctxt [] l

  let is_primitive { holder } = LocHolder.is_primitive holder

  let pp_ctxt fmt ctxt_list = 
    let rec impl fmt = function
      | [] -> F.fprintf fmt "]"
      | h :: [] -> F.fprintf fmt "%a ]" Ctxt.pp h
      | h :: t -> F.fprintf fmt "%a :: %a" Ctxt.pp h impl t
    in
    F.fprintf fmt "[ %a" impl ctxt_list

  let pp fmt { holder;  ctxt } = F.fprintf fmt "%a : %a" LocHolder.pp holder pp_ctxt ctxt

  let copy { holder; ctxt } = { holder; ctxt }

  let assign_ctxt ctxt pk = if ctxt <> Ctxt.empty && ( Caml.List.length pk.ctxt ) < ctxt_depth then pk.ctxt <- (pk.ctxt @ [ctxt]) ; pk
end

module UnionFind = struct
  module Tree = struct
    module Node = struct
      type t = Nil
      | N of { mutable parent: t; pk: PointerKey.t }
      [@@deriving compare]

      let nil = Nil

      let mk_node l = N { parent = Nil; pk= l }

      let is_nil = function Nil -> true | _ -> false

      let is_root = function N n -> is_nil n.parent | _ -> false

      let get_parent = function N n -> n.parent | _ -> failwith "Nil"

      let set_parent p = function N n -> n.parent <- p | _ -> failwith "Nil"

      let get_pk = function N n -> n.pk | _ -> failwith "Nil"

      let rec pp fmt = function
        | Nil ->  
            Format.fprintf fmt "Nil"
        | N { parent; pk } ->
            Format.fprintf fmt "%a <- %a" pp parent PointerKey.pp pk

      let chg_pk pk n = 
        match n with
        | Nil -> failwith "cannot assign pk to Nil"
        | N { parent } -> N { parent ; pk = pk }
    end

    type t = Node.t
  end

  let rec find n =
    let open Tree.Node in
    if is_root n then n
    else 
      let p' = find (get_parent n) in
      (set_parent p' n; p')

  let union n1 n2 =
    let open Tree.Node in
    let root1 = find n1 in
    let root2 = find n2 in
    if root1 <> root2 then (
      if not (is_root root1) || not (is_root root2) then
        failwith "root must be nil!"
      else
        Tree.Node.set_parent root1 root2)
end

module Pk2NodeMap = struct
  module M = PrettyPrintable.MakePPMap(PointerKey) 

  include (M: module type of M with type 'a t := 'a M.t)

  type t = UnionFind.Tree.Node.t M.t
    
end

module InstEnv = struct
  module M = PrettyPrintable.MakePPMap(UnionFind.Tree.Node) 

  include (M: module type of M with type 'a t := 'a M.t)

  type t = UnionFind.Tree.Node.t M.t
end

type t = Pk2NodeMap.t

let mk_ctxt cs args = 
  let receiver = 
    if Caml.List.length args = 0 then
      LocHolder.none
    else 
      LocHolder.mk_addr_holder (Caml.List.hd args)
  in
  Ctxt.make receiver cs

(* NOTE: this function handles mutable structures! *)
let assign_context ctxt m = 
  let open UnionFind.Tree in
  let rec f pk n (ienv, m) =
    if n = Node.nil || InstEnv.mem n ienv then
      (ienv, m)
    else
      let pk' = PointerKey.copy pk |> PointerKey.assign_ctxt ctxt in
      let n' = Node.chg_pk pk' n in 
      let m' = Pk2NodeMap.add pk' n' m in
      let ienv' = InstEnv.add n n' ienv in (
        match InstEnv.find_opt (Node.get_parent n')ienv with
        | Some p ->
            (Node.set_parent p n'; (ienv', m'))
        | None ->
            let p = Node.get_parent n' in
            let (ienv'', m'') = f (Node.get_pk p) p (ienv', m') in
            (Node.set_parent (InstEnv.find p ienv'') n'); (ienv'', m''))
  in
  let _, m' = Pk2NodeMap.fold f m (InstEnv.add UnionFind.Tree.Node.nil UnionFind.Tree.Node.nil InstEnv.empty, Pk2NodeMap.empty) in
  m'

let join = Pk2NodeMap.union (fun k (n1: UnionFind.Tree.Node.t) (n2: UnionFind.Tree.Node.t) -> 
  if n1 = n2 then Some n1
  else (UnionFind.union n1 n2; Some n2))

let widen ~prev ~next ~num_iters = join prev next

let compare = Pk2NodeMap.compare (fun n1 n2 -> UnionFind.Tree.Node.compare n1 n2) 

let (<=) ~lhs ~rhs = if compare lhs rhs <= 0 then true else false

let pp = Pk2NodeMap.pp ~pp_value:UnionFind.Tree.Node.pp 

let mk_node l m = 
  match Pk2NodeMap.find_opt l m with
  | Some n -> (n, m)
  | None ->
      let n = UnionFind.Tree.Node.mk_node l in
      (n, Pk2NodeMap.add l n m)

let eq (lhs: PointerKey.t) (rhs: PointerKey.t) m =
  if PointerKey.is_primitive lhs || PointerKey.is_primitive rhs then
    m
  else
    let n_lhs, m' = mk_node lhs m in
    let n_rhs, m'' = mk_node rhs m' in
    let p_lhs = UnionFind.find n_lhs in
    let p_rhs = UnionFind.find n_rhs in
    let () = if p_lhs <> p_rhs then UnionFind.union n_lhs n_rhs in m''

let empty = Pk2NodeMap.empty

