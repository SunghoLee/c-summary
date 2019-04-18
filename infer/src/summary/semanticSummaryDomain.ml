(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open Core
open Pervasives
module F = Format
module L = Logging

let widen_iter = 5

module Var = struct
  type t = {name: string; proc: var_scope; kind: var_kind}
  [@@deriving compare]

  and var_kind = Temp | Local | Global
  [@@deriving compare]

  and var_scope = GB | Proc of string

  let mk_scope s = Proc s

  let glob_scope = GB

  let scope_to_string = function
    | GB -> "global"
    | Proc p -> p

  let is_in scope {proc} = proc = scope

  let of_string ?(proc=GB) s =
    if String.is_prefix s ~prefix:"#GB" then
      {name = s; proc = proc; kind = Global}
    else if String.contains s '$' then
      {name = s; proc = proc; kind = Temp}
    else
      {name = s; proc = proc; kind = Local}

  let of_id ?(proc=GB) id = 
    let id_name = Ident.get_name id in
    let stamp = Ident.get_stamp id in
    of_string ((Ident.name_to_string id_name) ^ "$" ^ (string_of_int stamp)) ~proc

  let of_pvar ?(proc=GB) pvar = 
    (if Pvar.is_global pvar then
      match Pvar.get_translation_unit pvar with
      | Some s ->
          "#GB_" ^ (SourceFile.to_string s) ^ "_" ^ (Pvar.to_string pvar)
          |> of_string
      | None ->
          Pvar.to_string pvar
          |> of_string 
    else if Pvar.is_local pvar then
      Mangled.to_string (Pvar.get_name pvar)
      |> (fun x -> of_string x ~proc)
    else
      failwith "no other variable types in C")

  let is_global {kind} = 
    match kind with Global -> true | _ -> false

  let is_temporal {kind} = 
    match kind with Temp -> true | _ -> false

  let is_local {kind} = 
    match kind with Local -> true | _ -> false

  let pp fmt {name} = F.fprintf fmt "%s" name

  let pp_scope fmt proc = 
    match proc with
    | GB -> 
        F.fprintf fmt "Global"
    | Proc s ->
        F.fprintf fmt "%s" s

  let pp_var_scope fmt {proc} = pp_scope fmt proc

end

module Loc = struct
  type t = 
  | Explicit of Var.t
  | Implicit of string
  | Const of const_type
  | Pointer of t
  | FunPointer of Typ.Procname.t
  | Offset of t * t
  | Ret of string
  [@@deriving compare]

  and const_type = 
  | Integer of int
  | String of string
  [@@deriving compare]

  let is_explicit = function Explicit _ -> true | _ -> false

  let is_implicit = function Implicit _ -> true | _ -> false

  let is_const = function Const _ -> true | _ -> false

  let is_pointer = function Pointer _ -> true | _ -> false

  let is_fun_pointer = function FunPointer _ -> true | _ -> false

  let is_ret = function Ret _ -> true | _ -> false

  let is_offset = function Offset _ -> true | _ -> false

  let is_offset_of loc = function Offset (l, _) -> loc = l | _ -> false

  let mk_explicit v = Explicit v

  let mk_implicit s = Implicit s

  let mk_const_of_int i = Const (Integer i)

  let mk_const_of_string s = Const (String s)

  let mk_pointer i = Pointer i

  let mk_fun_pointer i = FunPointer i

  let mk_ret i = Ret i

  let mk_ret_of_pname pname = Typ.Procname.to_string pname |> mk_ret

  let mk_offset l i = Offset (l, i)

  let unwrap_ptr = function Pointer i -> i | _ -> failwith "it is not a pointer."

  let of_id ?proc id = Var.of_id ?proc id |> mk_explicit

  let of_pvar ?proc pvar = Var.of_pvar ?proc pvar |> mk_explicit

  let rec is_in scope loc =
    match loc with
    | Explicit var ->
        Var.is_in scope var 
    | Implicit _ ->
        true
    | Const _ ->
        true
    | Pointer t ->
        is_in scope t
    | FunPointer _ ->
        true
    | Offset (b, i) ->
        is_in scope b
    | Ret s ->
        (Var.scope_to_string scope) = s

  let rec leq_const lhs rhs =
    match lhs, rhs with
    | Integer lhs_int, Integer rhs_int ->
        lhs_int = rhs_int
    | String lhs_str, String rhs_str ->
        lhs_str = rhs_str
    | _ ->
        false

  and ( <= ) lhs rhs =
    match lhs, rhs with
    | Explicit lhs_const, Explicit rhs_const ->
        (Var.compare lhs_const rhs_const) = 0
    | Implicit lhs_str, Implicit rhs_str ->
        lhs_str = rhs_str
    | Const lhs_const, Const rhs_const ->
        leq_const lhs_const rhs_const
    | Pointer lhs_ptr, Pointer rhs_ptr ->
        lhs_ptr <= rhs_ptr
    | FunPointer lhs_ptr, FunPointer rhs_ptr ->
        (Typ.Procname.compare lhs_ptr rhs_ptr) = 0
    | Offset (lhs_loc, lhs_index), Offset (rhs_loc, rhs_index) ->
        (lhs_loc <= rhs_loc) && (lhs_index <= rhs_index)
    | Ret lhs_ret, Ret rhs_ret ->
        String.equal lhs_ret rhs_ret
    | _, _ ->
        false

  let rec pp fmt = function
    | Explicit i -> F.fprintf fmt "EX#%a(%a)" Var.pp i Var.pp_var_scope i
    | Implicit i -> F.fprintf fmt "IM#%s" i
    | Const i -> F.fprintf fmt "CONST#%a" pp_const i
    | Pointer p -> F.fprintf fmt "*(%a)" pp p
    | FunPointer p -> F.fprintf fmt "FN#%s" (Typ.Procname.to_string p)
    | Offset (l, i) -> F.fprintf fmt "%a@%a" pp l pp i
    | Ret s -> F.fprintf fmt "RET#%s" s
  and pp_const fmt = function
    | Integer i -> F.fprintf fmt "%d" i
    | String s -> F.fprintf fmt "%s" s

  let to_string i = F.asprintf "%a" pp i
end

module LocSet = struct 
  include PrettyPrintable.MakePPSet(Loc)
end

module JNIFun = struct
  type t = JF of string [@@deriving compare]

  let ( <= ) lhs rhs = (compare lhs rhs) = 0
    
  let of_string s = JF s

  let of_procname s = JF (Typ.Procname.to_string s)

  let pp fmt = function JF s -> F.fprintf fmt "%s" s
end

module JClass = struct
  type t = JC of string [@@deriving compare]

  let of_str s = JC s

  let pp fmt (JC s) = F.fprintf fmt "%s" s
end

module JMethodID = struct
  type t = JM of JClass.t * string [@@deriving compare]

  let of_str jc s = JM (jc, s)

  let pp fmt (JM (jc, m)) = F.fprintf fmt "%a.%s" JClass.pp jc m
end

module JFieldID = struct
  type t = JF of JClass.t * string [@@deriving compare]

  let of_str jc f = JF (jc, f)

  let pp fmt (JF (jc, f)) = F.fprintf fmt "%a.%s" JClass.pp jc f 
end

module Cst = struct
  open Z3

  type t = True
    | False
    | Or of t * t
    | And of t * t
    | Not of t
    | Eq of Loc.t * Loc.t
    [@@deriving compare]

  let cst_true = True

  let cst_false = False

  let cst_and c1 c2 = And (c1, c2)

  let cst_or c1 c2 = Or (c1, c2)

  let cst_eq c1 c2 = Eq (c1, c2)

  let cst_not c = Not c

  let rec pp fmt = function
    True -> 
      F.fprintf fmt "T"
    | False ->
      F.fprintf fmt "F"
    | Or (cst1, cst2) -> 
      F.fprintf fmt "(%a)|(%a)" pp cst1 pp cst2
    | And (cst1, cst2) -> 
      F.fprintf fmt "(%a)&(%a)" pp cst1 pp cst2
    | Not cst -> 
      F.fprintf fmt "!(%a)" pp cst
    | Eq (l1, l2) -> 
      F.fprintf fmt "(%a)=(%a)" Loc.pp l1 Loc.pp l2 

  module Z3Encoder = struct
    module Loc2SymMap = Caml.Map.Make(Loc)
    module Loc2IndexMap = Caml.Map.Make(Loc)

    let sym_map = ref Loc2SymMap.empty
    let index_map = ref Loc2SymMap.empty

    let index = ref 0 

    let get_index loc = 
      match Loc2IndexMap.find_opt loc !index_map with
      | Some i ->
          i
      | None ->
          (index := !index + 1; index_map := (Loc2IndexMap.add loc !index !index_map); !index)

    (* TODO: Should we handle constant locations as constant integers? *)
    let new_sym : context -> Loc.t -> Expr.expr = 
      fun ctxt loc ->
        let sort = Arithmetic.Integer.mk_sort ctxt in
        match loc with
        | Explicit _ | Implicit _ | Const _ ->
            (index := !index + 1; (Expr.mk_numeral_int ctxt (get_index loc) sort))
        | _ ->
            (index := !index + 1; (Expr.mk_const ctxt (Symbol.mk_int ctxt !index) sort))

    let find ctx loc = 
      match Loc2SymMap.find_opt loc !sym_map with
      | Some s -> 
          s
      | None -> 
          (let nsym = new_sym ctx loc in
          (sym_map := (Loc2SymMap.add loc nsym !sym_map); nsym))

    let rec encode ctx = function
       True -> 
         Boolean.mk_true ctx
       | False -> 
         Boolean.mk_false ctx
       | Or (cst1, cst2) -> 
         Boolean.mk_or ctx [encode ctx cst1; encode ctx cst2]
       | And (cst1, cst2) -> 
         Boolean.mk_and ctx [encode ctx cst1; encode ctx cst2]
       | Not cst -> 
         Boolean.mk_not ctx (encode ctx cst)
       | Eq (loc1, loc2) -> 
         Boolean.mk_eq ctx (find ctx loc1) (find ctx loc2)
  
    let decode e = 
      let module RevMap = Caml.Map.Make(
        struct 
           include Z3.Expr
           type t = expr
        end) 
      in
      let rev_map = 
        let f l s m = 
          match RevMap.find_opt s m with
          | Some _ -> 
            failwith "cannot be duplicated!"
          | None -> 
            RevMap.add s l m
        in
        Loc2SymMap.fold f !sym_map RevMap.empty 
      in 
      let find_loc e =
          match RevMap.find_opt e rev_map with
          | None -> 
            failwith "some constraints go wrong."
          | Some s -> 
            s
      in
      let rec impl e = 
        if Boolean.is_true e then cst_true
        else if Boolean.is_false e then cst_false
        else if Boolean.is_and e then
          match Expr.get_args e with
          | [fst; snd] -> 
          cst_and (impl fst) (impl snd)
          | fst :: rest -> 
          Caml.List.fold_left (fun i x -> cst_and i (impl x)) (impl fst) rest
          | _ -> 
          failwith "'and' boolean constraint does not have any sub-constraints."
        else if Boolean.is_or e then
          match Expr.get_args e with
          | [fst; snd] -> 
          cst_or (impl fst) (impl snd)
          | fst :: rest -> 
          Caml.List.fold_left (fun i x -> cst_or i (impl x)) (impl fst) rest
          | _ -> 
          failwith "'or' boolean constraint does not have any sub-constraints."
        else if Boolean.is_not e then
          match Expr.get_args e with
          | [fst] -> 
          cst_not (impl fst) 
          | _ -> 
          failwith "not constraint only has one argument."
        else if Boolean.is_eq e then
          match Expr.get_args e with
          | [fst; snd] -> 
          cst_eq (find_loc fst) (find_loc snd)
          | _ -> 
          failwith "0 constraint only has two arguments."
        else 
          failwith "Currnetly, this contraint is not considered as a Cst type."
      in
      impl e 
  end 

  module SMTSolver = struct
    let ctx = 
      let cfg = [("model", "true")] in
      mk_context cfg

    let is_sat cst = 
      let expr = Z3Encoder.encode ctx cst in
      let sim_expr = Expr.simplify expr None in
      let sim_cst = Z3Encoder.decode sim_expr in
      let solver = Solver.mk_solver ctx None in
      let res = Solver.check solver [sim_expr] in
      match res with
      | UNSATISFIABLE -> 
        (false, sim_cst)
      | UNKNOWN -> 
        (true, sim_cst)
      | _ -> 
        (true, sim_cst)
  end 
end

module LocCst = struct
  type t = Loc.t * Cst.t
    [@@deriving compare]
  
  let pp fmt (v, c) = 
      F.fprintf fmt "(%a, %a)" Loc.pp v Cst.pp c
end

module Val = struct
  include PrettyPrintable.MakePPSet(LocCst)

  (* partial order of AbstractValueSet *)
  let ( <= ) = subset

  let ( < ) lhs rhs = (lhs <= rhs) && (lhs <> rhs)

  let join = union

  let widen ~prev ~next =
    join prev next

  let pp fmt avs = 
    if is_empty avs then 
      F.fprintf fmt "ValBot"
    else
      pp fmt avs

  let optimize avs = 
    (fun (value, cst) avs -> 
      match Cst.SMTSolver.is_sat cst with
      | true, cst' ->
          add (value, cst') avs
      | _ ->
          avs)
    |> (fun f -> fold f avs empty)
end 

module InstEnv = struct
  module M = PrettyPrintable.MakePPMap(Loc)

  include (M: module type of M with type 'a t := 'a M.t)

  type t = Val.t M.t

  let rec find loc ienv =
    match find_opt loc ienv with
    | Some s -> 
        s
    | None ->
        Val.singleton (loc, Cst.cst_true)

  let add loc avs ienv =
    match find_opt loc ienv with
    | Some s ->
        add loc (Val.union s avs) ienv
    | _ ->
        add loc avs ienv

  let pp = pp ~pp_value: Val.pp

  let optimize ienv =
    fold 
      (fun loc v ienv ->
        let v' = Val.optimize v in
        if Val.is_empty v' then ienv else add loc v' ienv)
      ienv empty
end

module Heap = struct
  module M = PrettyPrintable.MakePPMap(Loc) 
  module LocSet = PrettyPrintable.MakePPSet(Loc)

  include (M: module type of M with type 'a t := 'a M.t)

  type t = Val.t M.t

  let pp = pp ~pp_value:Val.pp

  let compare h1 h2 =
    (fun avs1 avs2 ->
      Val.compare avs1 avs2)
    |> (fun x -> compare x h1 h2)

  let flatten_heap_locs heap =
  (fun loc v locset ->
    let locset' = LocSet.add loc locset in
    (fun ((l: Loc.t), _) locset ->
      LocSet.add loc locset)
    |> (fun f -> Val.fold f v locset'))
  |> (fun f -> fold f heap LocSet.empty)

  let find_offsets_of l heap =
    LocSet.filter  
      (fun (loc: Loc.t) -> match loc with Offset (base, _) -> base = l | _ -> false)
      (flatten_heap_locs heap)

  let find l heap =
    match find_opt l heap with
    | Some v ->
        v
    | None ->
        Val.singleton (Loc.mk_pointer l, Cst.cst_true)
        (*Val.empty*)

  let ( <= ) lhs rhs =
    let f = fun key val1 ->
      match find_opt key rhs with
      | Some val2 -> 
          Val.(val1 <= val2)
      | None -> 
          false
    in
    for_all f lhs

  let weak_update loc v heap =
    match find_opt loc heap with
    | Some v' -> 
        add loc (Val.union v v') heap
    | None ->
        add loc v heap

  let disjoint_union heap1 heap2 =
    union (fun _ _ _ -> failwith "Heaps are not disjoint!") heap1 heap2

  let opt_cst_in_heap heap =
    let opt_v_heap = fun loc v heap ->
      let v' = Val.optimize v in
      if Val.is_empty v' then
        heap
      else
        add loc v' heap
    in
    fold opt_v_heap heap empty

  let optimize ?scope ?flocs heap = 
    let is_seed = 
      match scope with
      | Some p ->
          Loc.is_in p
      | None ->
          fun (loc: Loc.t) -> match loc with Explicit _ -> true | _ -> false
    in
    let locs = (
      match flocs with
      | Some s ->
          s
      | None -> 
          fold (fun loc _ ls -> if is_seed loc then loc :: ls else ls) heap [])
    in
    let rec calc_closure acc queue = (* using BFS to find closures of a location *)
      if (Caml.Queue.length queue) = 0 then
        acc
      else
        let loc = Caml.Queue.pop queue in
        let offsets = find_offsets_of loc heap |> LocSet.filter (fun l -> not (LocSet.mem l acc)) in
        let acc' = LocSet.fold (fun l a -> Caml.Queue.push l queue; LocSet.add l a) offsets acc in
        let acc'' = (
          match find_opt loc heap with
          | Some v ->
              Val.fold (fun (l, _) a -> Caml.Queue.push l queue; LocSet.add l a)
                (Val.filter (fun (l, _) -> not (LocSet.mem l acc')) v)
                acc'
          | None ->
              acc')
        in
        calc_closure acc'' queue
    in
    let loc_closure = Caml.List.fold_left 
      (fun ls loc -> 
        let queue = Caml.Queue.create () in
        Caml.Queue.push loc queue;
        LocSet.union ls (calc_closure (LocSet.add loc LocSet.empty) queue))
      LocSet.empty locs in
    filter (fun loc _ -> LocSet.mem loc loc_closure) heap 
    |> opt_cst_in_heap


  let join lhs rhs = 
    (* TODO: need to revise this for performance *)
    let lhs' = opt_cst_in_heap lhs in
    let rhs' = opt_cst_in_heap rhs in
    union (fun key val1 val2 -> Some (Val.join val1 val2)) lhs' rhs'

  let widen ~prev ~next =
    (fun loc prev_v_opt next_v_opt ->
      match prev_v_opt, next_v_opt with
      | None, _ | _, None ->
          failwith "Cannot be widen: two heaps have different locations."
      | Some prev_v, Some next_v ->
          Some (Val.widen ~prev:prev_v ~next:next_v))
    |> (fun f -> merge f prev next)

end

module LogUnit = struct
  type t = 
    { rloc: Loc.t
    ; jfun: JNIFun.t
    ; args: Loc.t list
    ; heap: Heap.t }
    [@@deriving compare]

  let get_heap l = l.heap

  let get_args l = l.args

  let get_rloc l = l.rloc

  let get_jfun l = l.jfun

  let mk rloc' jfun' args' heap' = 
    { rloc = rloc'
    ; jfun = jfun'
    ; args = args'
    ; heap = heap' }

  let update_heap heap' l = { l with heap = heap' }

  let pp fmt = function {rloc; jfun; args; heap} -> 
    let rec pp_list fmt = function
      [] ->
        ()
      | h :: [] ->
        F.fprintf fmt "%a" Loc.pp h
      | h :: t ->
        F.fprintf fmt "%a, %a" Loc.pp h pp_list t
    in
    F.fprintf fmt "{%a; %a; %a; %a}" Loc.pp rloc JNIFun.pp jfun pp_list args Heap.pp heap

  let optimize u = { u with heap = (Heap.optimize u.heap ~flocs:u.args) }
end

module CallLogs = struct
  include PrettyPrintable.MakePPSet(LogUnit)

  let ( <= ) = subset

  let ( < ) lhs rhs = (lhs <= rhs) && (lhs <> rhs)

  let join = union

  let optimize ?scope logs = map LogUnit.optimize logs

  let find_all ret logs = 
    (fun log -> (Loc.compare ret log.LogUnit.rloc) = 0)
    |> (fun f -> filter f logs)

  let widen ~prev ~next =
    if prev < next then (* log size is increasing *)
      failwith "we hope that logs are not increased in a loop."
    else
      join prev next

end

module Domain = struct
  type t = 
    { heap: Heap.t
    ; logs: CallLogs.t }

  let empty = 
    { heap = Heap.empty
    ; logs = CallLogs.empty }

  let get_heap s = s.heap

  let get_logs s = s.logs

  let init = 
    { heap = Heap.empty
    ; logs = CallLogs.empty }

  let make heap' logs' = 
    { heap = heap'
    ; logs = logs' }

  let update_heap heap' s = { s with heap = heap' }

  let update_logs logs' s = { s with logs = logs' }

  let ( <= ) ~lhs ~rhs = 
    Heap.( lhs.heap <= rhs.heap ) 
    && CallLogs.( lhs.logs <= rhs.logs )

  let join lhs rhs = 
    { heap = ( Heap.join lhs.heap rhs.heap )
    ; logs = ( CallLogs.join lhs.logs rhs.logs ) }

  let widen ~prev ~next ~num_iters = 
    if num_iters >= widen_iter then
      (* TODO: need to widen for loop statements *)
      { heap = Heap.widen ~prev:prev.heap ~next:next.heap
      ; logs = CallLogs.widen ~prev:prev.logs ~next:next.logs }
    else 
      join prev next 

  let rm_redundant ?scope {heap; logs} = 
    let non_temp_locs = 
      (match scope with
      | Some f -> 
          [Loc.mk_ret (Var.scope_to_string f)]
          |> Heap.fold (fun loc _ loclist -> if Loc.is_explicit loc then loc::loclist else loclist) heap
      | None ->
          Heap.fold (fun loc _ loclist -> if Loc.is_explicit loc then loc::loclist else loclist) heap [])
    in
    let heap' = Heap.optimize heap ~flocs:non_temp_locs ?scope in
    let logs' = CallLogs.optimize ?scope logs in
    make heap' logs'

  let optimize ?scope astate = 
    rm_redundant astate ?scope

  let pp fmt { heap; logs } =
    F.fprintf fmt "===\n%a\n%a\n===" Heap.pp heap CallLogs.pp logs
end

include Domain
