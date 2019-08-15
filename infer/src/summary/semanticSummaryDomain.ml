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

let print_progress total cur = ()
  (*
  let total = float_of_int total in
  let cur = float_of_int cur in
  let base = if total <. 100.0 then 1.0 else total /. 100.0 in
  let c = cur /. base in
  let t = ( total /. base ) -. c in
  let str = ref "" in
  for i = 1 to (int_of_float c) do 
      str := !str ^ "="
  done;
  for i = 1 to (int_of_float t) do
      str := !str ^ " "
  done;
  L.progress "%s (%d / %d)\r" (!str ^ "|") (int_of_float cur) (int_of_float total)*)

module VVar = struct
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
  type ptr_typ = 
  | ConcreteLoc
  | LocVar
  [@@deriving compare] 
  
  and t = 
  | LocTop 
  | Explicit of VVar.t
  | Implicit of string
  | Const of const_type
  | Pointer of t * ptr_typ * bool (* bool is for weak update *)
  | FunPointer of Typ.Procname.t
  | Offset of t * t
  | Ret of string
  [@@deriving compare]

  and const_type = 
  | Z of Z.t
  | String of string
  [@@deriving compare]

  let rec pp fmt = function
    | LocTop -> F.fprintf fmt "LOCTOP"
    | Explicit i -> F.fprintf fmt "EX#%a(%a)" VVar.pp i VVar.pp_var_scope i
    | Implicit i -> F.fprintf fmt "IM#%s" i
    | Const i -> F.fprintf fmt "CONST#%a" pp_const i
    | Pointer (p, typ, b) -> F.fprintf fmt "*(%a, %d, %b)" pp p (match typ with ConcreteLoc -> 1 | _ -> 2) b
    | FunPointer p -> F.fprintf fmt "FN#%s" (Typ.Procname.to_string p)
    | Offset (l, i) -> F.fprintf fmt "%a@%a" pp l pp i
    | Ret s -> F.fprintf fmt "RET#%s" s
  and pp_const fmt = function
    | Z i -> F.fprintf fmt "%a" Z.pp_print i
    | String s -> F.fprintf fmt "%s" s

  let klimit = 10

  let rec get_k = function
  | LocTop -> 
      100000
  | Explicit _ ->
      1
  | Implicit _ ->
      1
  | Const _ ->
      1
  | Pointer (l, typ, _) ->
      1 + (get_k l)
  | FunPointer _ ->
      1
  | Offset (b, _) ->
      1 + (get_k b)
  | Ret _ ->
      1

  let is_top = function LocTop -> true | _ -> false

  let is_temporal = function Explicit v -> VVar.is_temporal v | _ -> false

  let is_explicit = function Explicit _ -> true | _ -> false

  let is_implicit = function Implicit _ -> true | _ -> false

  let is_const = function Const _ -> true | _ -> false

  let is_pointer = function Pointer _ -> true | _ -> false

  let is_fun_pointer = function FunPointer _ -> true | _ -> false

  let is_jni_fun_pointer = function 
    | FunPointer pn -> 
        let n = Typ.Procname.to_string pn in
        String.is_prefix n ~prefix:"_JNIEnv_" 
    | _ -> false

  let get_jni_fun_name_exn = function 
    |  FunPointer pn -> Typ.Procname.to_string pn
    | _ -> failwith "this is not a jni fun pointer"

  let is_ret = function Ret _ -> true | _ -> false

  let is_offset = function Offset _ -> true | _ -> false

  let is_offset_of loc = function Offset (l, _) -> loc = l | _ -> false

  let mk_explicit v = Explicit v

  let mk_implicit s = Implicit s

  let mk_const_of_z i = Const (Z i)

  let mk_const_of_string s = Const (String s)

  let mk_pointer ?dyn typ i = 
    match dyn with
    | Some true ->
        (*let () = L.progress "Make Dyn Loc: %a\n@." pp i in*)
        Pointer (i, typ, true)
    | _ ->
        Pointer (i, typ, false)

  let mk_concrete_pointer ?dyn = mk_pointer ?dyn ConcreteLoc

  let mk_var_pointer ?dyn = mk_pointer ?dyn LocVar

  let mk_fun_pointer i = FunPointer i

  let mk_ret i = Ret i

  let mk_ret_of_pname pname = Typ.Procname.to_string pname |> mk_ret

  let mk_offset l i = Offset (l, i)

  let rec is_concrete = function
    | LocTop | Explicit _ | Implicit _ | Const _ | FunPointer _ | Ret _ ->
        true
    | Pointer (l, typ, _) -> 
        typ = ConcreteLoc
    | Offset (l, _) ->
        is_concrete l

  let rec is_dyn = function
    | LocTop | Explicit _ | Implicit _ | Const _ | FunPointer _ | Ret _ ->
        false
    | Pointer (l, typ, b) -> 
        b || is_dyn l    
    | Offset (l, _) ->
        is_dyn l

  let get_base = function Offset (l, i) -> l | _ -> failwith "it is not an offset."

  let unwrap_ptr = function Pointer (i, _, _) -> i | _ -> failwith "it is not a pointer."

  let of_id ?proc id = VVar.of_id ?proc id |> mk_explicit

  let of_pvar ?proc pvar = VVar.of_pvar ?proc pvar |> mk_explicit

  let rec is_in scope loc =
    match loc with
    | LocTop ->
        true
    | Explicit var ->
        VVar.is_in scope var 
    | Implicit _ ->
        true
    | Const _ ->
        true
    | Pointer (t, typ, _) ->
        is_in scope t
    | FunPointer _ ->
        true
    | Offset (b, i) ->
        is_in scope b
    | Ret s ->
        (VVar.scope_to_string scope) = s

  let rec leq_const lhs rhs =
    match lhs, rhs with
    | Z lhs_int, Z rhs_int ->
        Z.(lhs_int = rhs_int)
    | String lhs_str, String rhs_str ->
        lhs_str = rhs_str
    | _ ->
        false

  and ( <= ) lhs rhs =
    match lhs, rhs with
    | _, LocTop ->
        true
    | LocTop, _ -> 
        false
    | Explicit lhs_const, Explicit rhs_const ->
        (VVar.compare lhs_const rhs_const) = 0
    | Implicit lhs_str, Implicit rhs_str ->
        lhs_str = rhs_str
    | Const lhs_const, Const rhs_const ->
        leq_const lhs_const rhs_const
    | Pointer (lhs_ptr, lhs_typ, _), Pointer (rhs_ptr, rhs_typ, _) ->
        lhs_typ = rhs_typ && lhs_ptr <= rhs_ptr
    | FunPointer lhs_ptr, FunPointer rhs_ptr ->
        (Typ.Procname.compare lhs_ptr rhs_ptr) = 0
    | Offset (lhs_loc, lhs_index), Offset (rhs_loc, rhs_index) ->
        (lhs_loc <= rhs_loc) && (lhs_index <= rhs_index)
    | Ret lhs_ret, Ret rhs_ret ->
        String.equal lhs_ret rhs_ret
    | _, _ ->
        false


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
        if Loc.is_concrete loc then
          (index := !index + 1; (Expr.mk_numeral_int ctxt (get_index loc) sort))
        else 
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

  module SimpleCSTSolver = struct
    type res_type = SOLVED of t | UNSOLVED

    let rec simplify = function
      | True ->
          SOLVED True
      | False ->
          SOLVED False
      | Or (cst1, cst2) -> (
          match simplify cst1, simplify cst2 with
          | SOLVED True, _ | _, SOLVED True ->
              SOLVED True
          | SOLVED False, SOLVED False ->
              SOLVED False
          | _ ->
              UNSOLVED)
      | And (cst1, cst2) -> (
          match simplify cst1, simplify cst2 with
          | SOLVED False, _ | _, SOLVED False ->
              SOLVED False
          | SOLVED True, SOLVED True ->
              SOLVED True
          | _ ->
              UNSOLVED)
      | Not cst -> (
          match simplify cst with
          | SOLVED True ->
              SOLVED False
          | SOLVED False ->
              SOLVED True
          | _ ->
              UNSOLVED)
      | Eq (loc1, loc2) -> (
          if not (Loc.is_pointer loc1) && not (Loc.is_pointer loc2) then (
            if loc1 = loc2 then
              SOLVED True
            else
              SOLVED False)
          else
            UNSOLVED
      )
  end

  module SMTSolver = struct
    let ctx = mk_context []

    let simplify l cst =
      match SimpleCSTSolver.simplify cst with
      | SOLVED cst' ->
          cst'
      | UNSOLVED ->
          let expr = Z3Encoder.encode ctx cst in
          Expr.simplify expr None |> Z3Encoder.decode


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

  let add (l, cst) v =
    match find_first_opt (fun (lv, cstv) -> l = lv) v with
    | Some (l, cstv) ->
        add (l, Cst.cst_or cst cstv) (remove (l, cstv) v)
    | None ->
        add (l, cst) v

  let pp fmt avs = 
    if is_empty avs then 
      F.fprintf fmt "ValBot"
    else
      pp fmt avs

  let union v1 v2 =
    let inter_v1, disjoint_v1 = partition (fun (l1, _) -> exists (fun (l2, _) -> l1 = l2) v2) v1 in
    let inter_v2, disjoint_v2 = partition (fun (l2, _) -> exists (fun (l1, _) -> l1 = l2) v1) v2 in
    let inter_merged_v = map (fun (l2, cst2) -> 
        let l1, cst1 = filter (fun (l1, cst1) -> l1 = l2) inter_v1 |> min_elt in
        (l1, Cst.cst_or cst1 cst2)) inter_v2
    in
    union (union inter_merged_v disjoint_v1) disjoint_v2

  let join = union

  let widen ~prev ~next =
    join prev next

  let optimize ?mornitor ?base_i avs = 
      let mornitor = match mornitor with Some m -> m | None -> (fun x -> ()) in
      let base_i = match base_i with Some i -> i | None -> 0 in
      let i = ref 0 in
      let res = (fun (value, cst) avs -> 
          let () = i := !i + 1 in
          let () = mornitor (base_i + !i) in
      match Cst.SMTSolver.simplify value cst with
      | False ->
          avs
      | _ as cst' ->
          add (value, cst') avs)
    |> (fun f -> fold f avs empty)
      in
      res
end 

module InstEnv = struct
  module M = PrettyPrintable.MakePPMap(Loc)

  include (M: module type of M with type 'a t := 'a M.t)

  type t = Val.t M.t

  let size ienv = fold (fun l v i -> i + (Val.cardinal v)) ienv 0

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
      let start_gettimeofday = Unix.gettimeofday () in
      let total = (size ienv) in
      (*let () = L.progress "#IENV optimizing(%d)!\n@." total in*)
      let cur = ref 0 in
      let ppp = print_progress total in
      let res = fold 
      (fun loc v ienv ->
        let v' = Val.optimize v ~mornitor:ppp ~base_i:!cur in
        let () = cur := (Val.cardinal v) + !cur in
        if Val.is_empty v' then ienv else add loc v' ienv)
      ienv empty in
      let end_gettimeofday = Unix.gettimeofday () in
      (*let () = L.progress "\n\tDone IENV Opt: %f\n@." (end_gettimeofday -. start_gettimeofday) in*)
      res

end

module Heap = struct
  module M = PrettyPrintable.MakePPMap(Loc) 
  module LocSet = PrettyPrintable.MakePPSet(Loc)

  include (M: module type of M with type 'a t := 'a M.t)

  type t = Val.t M.t

  let pp = pp ~pp_value:Val.pp

  let size h = fold (fun l v i -> i + (Val.cardinal v)) h 0

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

  let weak_update loc v heap =
    match find_opt loc heap with
    | Some v' -> 
        add loc (Val.union v' v) heap
    | None ->
        add loc v heap

  let add l v heap =
    if l = (Loc.mk_const_of_z Z.zero) then
      heap
    else if Loc.is_dyn l then
      weak_update l v heap
    else
      add l v heap
    
  let find_offsets_of l heap =
    LocSet.filter  
      (fun (loc: Loc.t) -> match loc with Offset (base, _) -> base = l | _ -> false)
      (flatten_heap_locs heap)

  let find l heap =
    match find_opt l heap with
    | Some v ->
        v
    | None ->
        Val.empty

  let ( <= ) lhs rhs =
    let f = fun key val1 ->
      match find_opt key rhs with
      | Some val2 -> 
          Val.(val1 <= val2)
      | None -> 
          false
    in
    for_all f lhs

  let disjoint_union heap1 heap2 =
    union (fun _ _ _ -> failwith "Heaps are not disjoint!") heap1 heap2

  module HeapDepGraph = struct

    module LocVertexMap = PrettyPrintable.MakePPMap(Loc)
    module ID2LocMap = PrettyPrintable.MakePPMap(Int)

    module Vertex = struct
      module VertexIdSet = PrettyPrintable.MakePPSet(Int)

      type t = {id: int (* unique id *); mutable ig: VertexIdSet.t; mutable og: VertexIdSet.t}
      [@@deriving compare]

      let make cur_id = {id = cur_id; ig = VertexIdSet.empty; og = VertexIdSet.empty}

      let pp fmt v = Format.fprintf fmt "V#%d" v.id

      let pp_name imap fmt v = 
        Format.fprintf fmt "V#%a" Loc.pp (ID2LocMap.find v.id imap);
        VertexIdSet.iter (fun id -> 
          Format.fprintf fmt "\n\t-> V#%a" Loc.pp (ID2LocMap.find id imap)) v.og
    end

    let id = ref 1

    type graph = {mutable vmap: Vertex.t LocVertexMap.t; mutable imap: Loc.t ID2LocMap.t}

    let pp_graph fmt g = 
      LocVertexMap.iter (fun loc v ->
        Format.fprintf fmt "%a\n" (Vertex.pp_name g.imap) v) g.vmap

    let init () = {vmap = LocVertexMap.empty; imap = ID2LocMap.empty}

    let new_vertex loc g =
      let cur_id = !id in
      (id := !id + 1
      ; g.imap <- ID2LocMap.add cur_id loc g.imap)
      ; let v = Vertex.make cur_id in
      (g.vmap <- LocVertexMap.add loc v g.vmap
      ; v)

    let get_vertex loc g = 
      match LocVertexMap.find_opt loc g.vmap with
      | Some v ->
       v 
    | None ->
       new_vertex loc g

    let add_edge (src: Vertex.t) (dst: Vertex.t) = 
      src.og <- Vertex.VertexIdSet.add dst.id src.og
      ; dst.ig <- Vertex.VertexIdSet.add src.id dst.ig

    let make heap = 
      let rec cedge (loc: Loc.t) g =
        match loc with
        | Pointer (base, typ, _) ->
            let tov = get_vertex loc g in
            let fromv = get_vertex base g in
            add_edge fromv tov; cedge base g
        | Offset (base, _) ->
            let tov = get_vertex loc g in
            let fromv = get_vertex base g in
            add_edge fromv tov; cedge base g
        | _ -> ()
      in
      fold (fun loc value g ->
        cedge loc g;
        let from_vertex = get_vertex loc g in 
        Val.iter (fun (loc, _) ->
          cedge loc g;
          let to_vertex = get_vertex loc g in
          add_edge from_vertex to_vertex) value; g) heap (init ())

    let get_closure locs heap g = 
      let module VSet = PrettyPrintable.MakePPSet(Vertex) in
      let rec impl acc visited queue =
        if (Caml.Queue.length queue) = 0 then
          acc
        else
          let v = Caml.Queue.pop queue in
          let acc', visited' = Vertex.VertexIdSet.fold (fun vid (acc, visited) ->
            let v = LocVertexMap.find (ID2LocMap.find vid g.imap) g.vmap in
              if not (VSet.mem v visited) then
                let l = ID2LocMap.find v.id g.imap in
                (Caml.Queue.push v queue; 
                (match find_opt l heap with
                | Some v' ->
                    add l v' acc
                | None ->
                    acc), VSet.add v visited)
              else
                acc, visited)
            v.og (acc, visited)
          in
          impl acc' visited' queue
      in
      let vertexes = Caml.List.fold_left (fun vl l -> (LocVertexMap.find l g.vmap) :: vl) [] locs in
      let heap' = Caml.List.fold_left (fun h l -> add l (find l heap) h) empty locs in
      let queue = Caml.Queue.create () in
      (Caml.Queue.add_seq queue (Caml.List.to_seq vertexes); impl heap' (VSet.of_seq (Caml.List.to_seq vertexes)) queue)
  end

  let opt_cst_in_heap heap =
    let total = (size heap) in
    let cur = ref 0 in
    let pp = print_progress total in
    let opt_v_heap = fun loc v heap ->
      let v' = Val.optimize v ~mornitor:pp ~base_i:!cur in
      let () = cur := (Val.cardinal v) + !cur in
      if Val.is_empty v' then
        heap
      else
        add loc v' heap
    in
    fold opt_v_heap heap empty

  let optimize ?scope ?flocs ?rm_tmp heap = 
    let no_rm_tmp = match rm_tmp with Some true -> false | None -> true in
    (*let () = L.progress "#Start Heap Optimization\n@." in*)
    let start_gettimeofday = Unix.gettimeofday () in
    let is_seed = 
      match scope with
      | Some p ->
          fun l -> (Loc.is_in p l || Loc.is_in VVar.glob_scope l) && (no_rm_tmp || not (Loc.is_temporal l))
      | None ->
          fun (loc: Loc.t) -> match loc with Explicit _ when (no_rm_tmp || not (Loc.is_temporal loc)) -> true | _ -> false
    in
    let hdg = HeapDepGraph.make heap in
    let seed_gettimeofday = Unix.gettimeofday () in
    (*let () = L.progress "\t Found seeds: %f\n@." (seed_gettimeofday -. start_gettimeofday) in*)
    let locs = (
      match flocs with
      | Some s ->
          fold (fun loc _ ls -> if Caml.List.mem loc s then loc :: ls else ls) heap []
      | None -> 
          fold (fun loc _ ls -> if is_seed loc then loc :: ls else ls) heap [])
    in
    let locs_gettimeofday = Unix.gettimeofday () in
    (*let () = L.progress "\t Initial Locs: %f\n@." (locs_gettimeofday -. seed_gettimeofday) in*)
    let heap' = HeapDepGraph.get_closure locs heap hdg in
    let closure_gettimeofday = Unix.gettimeofday () in
    (*let () = L.progress "\t Found Closures: %f\n@." (closure_gettimeofday -. locs_gettimeofday) in*)
    let res = opt_cst_in_heap heap' in
    let opt_gettimeofday = Unix.gettimeofday () in
    (*let () = L.progress "\n\t Opt: %f\n@." (opt_gettimeofday -. closure_gettimeofday) in*)
    (*let () = L.progress "\t Done.\n@." in*)
    res


  let join lhs rhs = 
    (* TODO: need to revise this for performance *)
    (*let () = L.progress "#Start Heap Optimization in Join!!.\n@." in*)
    let lhs' = opt_cst_in_heap lhs in
    let rhs' = opt_cst_in_heap rhs in
    union (fun key val1 val2 -> Some (Val.join val1 val2)) lhs' rhs'

  let widen ~prev ~next =
    join prev next
    (*
    (fun loc prev_v_opt next_v_opt ->
      match prev_v_opt, next_v_opt with
      | None, _ | _, None ->
          failwith "Cannot be widen: two heaps have different locations."
      | Some prev_v, Some next_v ->
          Some (Val.widen ~prev:prev_v ~next:next_v))
    |> (fun f -> merge f prev next)
    *)

end

module CallSite = struct
  type t =
    { caller: string (* function name *)
    ; ln: int (* line number of function call *)
    ; col: int } (* column number of function call *)
  [@@deriving compare]
  
  (* getters *)
  let get_caller l = l.caller
  let get_ln l = l.ln
  let get_col l = l.col

  (* constructor *)
  let mk caller' ln' col' =
    { caller = caller'
    ; ln = ln'
    ; col = col' }

  (* list comparator in lexicographical order *)
  let rec compare_list : t list -> t list -> int = function
    | [] -> (function
        | [] -> 0
        | y :: ys -> -1)
    | x :: xs -> (function
        | [] -> 1
        | y :: ys -> match compare x y with
            | 0 -> compare_list xs ys
            | r -> r)

  (* pretty printer *)
  let pp fmt = function {caller; ln; col} ->
    F.fprintf fmt "[%s:%d:%d]" caller ln col

  (* pretty printer for list of CallSites *)
  let pp_list fmt lst =
    let rec f s = function
      | [] -> s
      | {caller; ln; col} :: xs ->
          let sep = if String.length s > 0 then "," else "" in
          f (F.sprintf "%s%s%s:%d:%d" s sep caller ln col) xs
    in F.fprintf fmt "[%s]" (f "" lst)

  (* parser for CallSite
   * variable `s` must be in the format of "<FN_NAME>:<LN>:<COL>" *)
  let of_string s =
    match Str.split (Str.regexp ":") s with
      [fn; ln; col] -> mk (fn) (int_of_string ln) (int_of_string col)
    | _ -> failwith "failed LocPos.of_string: wrong format list"

  (* parser for CallSites
   * variable `s` must be in the format of
   * "<FN1>:<LN1>:<COL1>[,<FNi>:<LNi>:<COLi>]*" *)
  let list_of_string s =
    let rec f res = function
        [] -> List.rev res
      | s :: ss -> f (of_string s :: res) ss
    in
    if String.length s > 0
    then f [] (Str.split (Str.regexp ",") s)
    else []
end

module LogUnit = struct
  type t = 
    { call_sites: CallSite.t list
    ; rloc: Loc.t
    ; jfun: JNIFun.t
    ; args: Loc.t list
    ; heap: Heap.t }
    [@@deriving compare]

  let get_call_sites l = l.call_sites

  let get_heap l = l.heap

  let get_args l = l.args

  let get_rloc l = l.rloc

  let get_jfun l = l.jfun

  let mk call_sites' rloc' jfun' args' heap' = 
    { call_sites = call_sites'
    ; rloc = rloc'
    ; jfun = jfun'
    ; args = args'
    ; heap = heap' }

  let update_heap heap' l = { l with heap = heap' }

  let append_call_sites cs =
    function {call_sites} as l ->
      { l with call_sites = cs :: call_sites }

  let pp fmt = function {call_sites; rloc; jfun; args; heap} -> 
    let rec pp_list fmt = function
      [] ->
        ()
      | h :: [] ->
        F.fprintf fmt "%a" Loc.pp h
      | h :: t ->
        F.fprintf fmt "%a, %a" Loc.pp h pp_list t
    in
    F.fprintf fmt "%a: " CallSite.pp_list call_sites;
    F.fprintf fmt "{%a; %a; %a; %a}" Loc.pp rloc JNIFun.pp jfun pp_list args Heap.pp heap

  let optimize u = { u with heap = (Heap.optimize u.heap ~flocs:u.args) }
end

module CallLogs = struct
  include PrettyPrintable.MakePPSet(LogUnit)

  let ( <= ) = subset

  let ( < ) lhs rhs = (lhs <= rhs) && (not (equal lhs rhs))

  let join = union

  let optimize ?scope logs = map LogUnit.optimize logs

  let find_all ret logs = 
    (fun log -> (Loc.compare ret log.LogUnit.rloc) = 0)
    |> (fun f -> filter f logs)

  let widen ~prev ~next =
    if prev < next then (* log size is increasing *)
      failwith (F.asprintf "we hope that logs are not increased in a loop.\n#FST: %a\n#SND: %a" pp prev pp next)
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

  let rm_redundant ?scope ?rm_tmp {heap; logs} = 
    let non_temp_locs = 
      (match scope with
      | Some f -> 
          [Loc.mk_ret (VVar.scope_to_string f)]
          |> Heap.fold (fun loc _ loclist -> if Loc.is_explicit loc then loc::loclist else loclist) heap
      | None ->
          Heap.fold (fun loc _ loclist -> if Loc.is_explicit loc then loc::loclist else loclist) heap [])
    in
    let heap' = Heap.optimize heap (*~flocs:non_temp_locs*) ?scope ?rm_tmp in
    let logs' = CallLogs.optimize ?scope logs in
    make heap' logs'

  let optimize ?scope ?rm_tmp astate = 
    rm_redundant astate ?scope ?rm_tmp

  let pp fmt { heap; logs } =
    F.fprintf fmt "===\n%a\n%a\n===" Heap.pp heap CallLogs.pp logs
end

include Domain
