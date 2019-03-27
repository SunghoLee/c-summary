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
  type t = Bot
  | Top
  | ConstLoc of Var.t
  | DynLoc of string
  | Pointer of t
  | Offset of t * index
  | Ret of string
  [@@deriving compare]

  and index = IndexTop
  | I of int
  | F of string
  | E of t
  [@@deriving compare]

  let bot = Bot

  let top = Top

  let index_top = IndexTop

  let is_bot = function Bot -> true | _ -> false

  let is_top = function Top -> true | _ -> false

  let is_const = function ConstLoc _ -> true | _ -> false

  let is_dyn = function DynLoc _ -> true | _ -> false

  let is_pointer = function Pointer _ -> true | _ -> false

  let is_ret = function Ret _ -> true | _ -> false

  let is_offset = function Offset _ -> true | _ -> false

  let is_offset_of loc = function Offset (l, _) -> loc = l | _ -> false

  let mk_const v = ConstLoc v

  let mk_dyn s = DynLoc s

  let mk_pointer i = Pointer i

  let mk_ret i = Ret i

  let mk_ret_of_pname pname = Typ.Procname.to_string pname |> mk_ret

  let mk_offset l i = Offset (l, i)

  let mk_index_of_string s = F s

  let mk_index_of_int i = I i

  let mk_index_of_expr e = E e

  let unwrap_ptr = function Pointer l -> l | _ -> failwith "This location is not a pointer."

  let rec pp fmt = function
    Bot -> F.fprintf fmt "bot"
    | Top -> F.fprintf fmt "top"
    | ConstLoc i -> F.fprintf fmt "CL#%a(%a)" Var.pp i Var.pp_var_scope i
    | DynLoc i -> F.fprintf fmt "DL#%s" i
    | Pointer p -> F.fprintf fmt "*( %a )" pp p
    | Offset (l, i) -> F.fprintf fmt "%a@%a" pp l pp_index i
    | Ret s -> F.fprintf fmt "Ret#%s" s
  and pp_index fmt = function
    | IndexTop -> F.fprintf fmt "Top"
    | I i -> F.fprintf fmt "%d" i
    | F s -> F.fprintf fmt "%s" s
    | E l -> pp fmt l

  let to_string i = F.asprintf "%a" pp i
end

module LocSet = struct 
  include PrettyPrintable.MakePPSet(Loc)
end

module SStr = struct
  type t = Top | String of string [@@deriving compare]

  let top = Top 

  let of_string s = String s

  let pp fmt = function 
    Top -> 
      F.fprintf fmt "\'T_str\'"
    |  String s -> F.fprintf fmt "\'%s\'" s
end

module IInt = struct
  type t = Int of int | Top [@@deriving compare]

  let top = Top

  let is_top = function Top -> true | _ -> false

  let of_int i = Int i

  let to_int = function Int i -> i | _ -> failwith "Cannot unwrap the Top integer."

  let pp fmt = function Int i -> F.fprintf fmt "\'%d\'" i | Top -> F.fprintf fmt "T"

  let ( + ) lhs rhs =
    match lhs, rhs with
    | Top, _ | _, Top -> Top
    | Int i1, Int i2 ->
        Int (i1 + i2)

  let ( - ) lhs rhs = 
    match lhs, rhs with
    | Top, _ | _, Top -> Top
    | Int i1, Int i2 ->
        Int (i1 - i2)

  let ( * ) lhs rhs = 
    match lhs, rhs with
    | Top, _ | _, Top -> Top
    | Int i1, Int i2 ->
        Int (i1 * i2)

  let ( / ) lhs rhs = 
    match lhs, rhs with
    | Top, _ | _, Top -> Top
    | Int i1, Int i2 when i2 = 0 ->
        top
    | Int i1, Int i2 ->
        Int (i1 / i2)

  let ( % ) lhs rhs = 
    match lhs, rhs with
    | Top, _ | _, Top -> Top
    | Int i1, Int i2 when i2 = 0 ->
        top
    | Int i1, Int i2 ->
        Int (i1 % i2)

  let ( << ) lhs rhs = 
    match lhs, rhs with
    | Top, _ | _, Top -> Top
    | Int i1, Int i2 ->
        Int ((lsl) i1 i2)

  let ( >> ) lhs rhs = 
    match lhs, rhs with
    | Top, _ | _, Top -> Top
    | Int i1, Int i2 ->
        Int ((lsr) i1 i2)

  let ( < ) lhs rhs = 
    match lhs, rhs with
    | Top, _ | _, Top -> Top
    | Int i1, Int i2 ->
        if i1 < i2 then Int 1 else Int 0

  let ( <= ) lhs rhs = 
    match lhs, rhs with
    | Top, _ | _, Top -> Top
    | Int i1, Int i2 ->
        if i1 <= i2 then Int 1 else Int 0

  let ( > ) lhs rhs = 
    match lhs, rhs with
    | Top, _ | _, Top -> Top
    | Int i1, Int i2 ->
        if i1 > i2 then Int 1 else Int 0

  let ( >= ) lhs rhs = 
    match lhs, rhs with
    | Top, _ | _, Top -> Top
    | Int i1, Int i2 ->
        if i1 >= i2 then Int 1 else Int 0

  let ( = ) lhs rhs = 
    match lhs, rhs with
    | Top, _ | _, Top -> Top
    | Int i1, Int i2 ->
        if i1 = i2 then Int 1 else Int 0

  let ( != ) lhs rhs = 
    match lhs, rhs with
    | Top, _ | _, Top -> Top
    | Int i1, Int i2 ->
        if not (phys_equal i1 i2) then Int 1 else Int 0

  let ( & ) lhs rhs = 
    match lhs, rhs with
    | Top, _ | _, Top -> Top
    | Int i1, Int i2 ->
        Int ((land) i1 i2)

  let b_or lhs rhs = 
    match lhs, rhs with
    | Top, _ | _, Top -> Top
    | Int i1, Int i2 ->
        Int ((lor) i1 i2)

  let b_xor lhs rhs = 
    match lhs, rhs with
    | Top, _ | _, Top -> Top
    | Int i1, Int i2 ->
        Int ((lxor) i1 i2)

  let l_and lhs rhs = 
    match lhs, rhs with
    | Top, _ | _, Top -> Top
    | Int i1, Int i2 ->
        if i1 <> 0 && i2 <> 0 then Int 1 else Int 0

  let l_or lhs rhs = 
    match lhs, rhs with
    | Top, _ | _, Top -> Top
    | Int i1, Int i2 ->
        if i1 <> 0 || i2 <> 0 then Int 1 else Int 0
end

module JNIFun = struct
  type t = JF of string [@@deriving compare]

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

module Val = struct
  type t = Bot
    | Top
    | Loc of Loc.t
    | Str of SStr.t
    | Int of IInt.t
    | JClass of JClass.t
    | JMethodID of JMethodID.t
    | JFieldID of JFieldID.t
    [@@deriving compare]

  let pp fmt = function
    Bot -> 
      F.fprintf fmt "bot"
    | Top -> 
      F.fprintf fmt "top"
    | Loc l -> 
      F.fprintf fmt "%a" Loc.pp l
    | Str s -> 
      F.fprintf fmt "%a" SStr.pp s
    | Int i -> 
      F.fprintf fmt "%a" IInt.pp i
    | JClass jc -> 
      F.fprintf fmt "%a" JClass.pp jc
    | JMethodID jm -> 
      F.fprintf fmt "%a" JMethodID.pp jm
    | JFieldID jf -> 
      F.fprintf fmt "%a" JFieldID.pp jf

  let bot = Bot

  let top = Top

  let of_loc l = Loc l

  let of_str s = Str s
  
  let of_int i = Int i

  let of_jclass jc = JClass jc

  let of_jmethod_id jm = JMethodID jm

  let of_jfield_id jf = JFieldID jf

  let is_bot = function Bot -> true | _ ->false

  let is_top = function Top -> true | _ ->false

  let is_loc = function Loc _ -> true | _ -> false

  let is_str = function Str _ -> true | _ -> false

  let is_int = function Int _ -> true | _ -> false

  let is_jclass = function JClass _ -> true | _ -> false

  let is_jmethod_id = function JMethodID _ -> true | _ -> false

  let is_jfield_id  = function JFieldID _ -> true | _ -> false

  let to_loc = function Loc l -> l | _ as arg -> failwith (F.asprintf "Wrong type argument: %a" pp arg)

  let to_str = function Str s -> s | _ -> failwith "Wrong type argument."

  let to_int = function Int i -> i | _ -> failwith "Wrong type argument."

  let to_jclass = function JClass jc -> jc | _ -> failwith "Wrong type argument."

  let to_jmethod_id = function JMethodID jm -> jm | _ -> failwith "Wrong type argument."

  let to_jfield_id = function JFieldID ji -> ji | _ -> failwith "Wrong type argument."
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
        | ConstLoc _ ->
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

module ValCst = struct
  type t = Val.t * Cst.t
    [@@deriving compare]
  
  let top = Val.top, Cst.cst_true

  let pp fmt (v, c) = 
      F.fprintf fmt "(%a, %a)" Val.pp v Cst.pp c
end

module AVS = struct
  include PrettyPrintable.MakePPSet(ValCst)

  let top = singleton ValCst.top

  let bot = empty

  (* partial order of AbstractValueSet *)
  let ( <= ) = subset

  let ( < ) lhs rhs = (lhs <= rhs) && (lhs <> rhs)

  let join = union

  let widen ~prev ~next =
    if prev < next then (* value size is increasing *)
      if for_all (fun (value, cst) -> Val.is_int value) next then (* all values are integers *)
        add (Val.of_int IInt.top, Cst.cst_true) empty
      else if for_all (fun (value, cst) -> Val.is_loc value) next then (* all values are locations *)
        add (Val.of_loc Loc.top, Cst.cst_true) empty
      else if for_all (fun (value, cst) -> Val.is_str value) next then (* all values are strings *)
        add (Val.of_str SStr.top, Cst.cst_true) empty
      else (* others *)
        add (Val.top, Cst.cst_true) empty
    else
      join prev next

  let pp fmt avs = 
    if is_empty avs then 
      F.fprintf fmt "Bot"
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

module Env = struct
  module M = PrettyPrintable.MakePPMap(Var)

  include (M : module type of M with type 'a t := 'a M.t)

  type t = Loc.t M.t
  [@@deriving compare]

  let find v env = 
    match find_opt v env with
    | Some l ->
        l
    | None ->
        Loc.bot

  let ( <= ) lhs rhs = 
    let f = fun key value -> 
      match find_opt key rhs with
      | Some value' -> 
        (Loc.compare value value') = 0
      | None ->
        false
    in
    for_all f lhs 

  let join lhs rhs = 
    let f = fun key val1 val2 -> 
      if (Loc.compare val1 val2) = 0 then Some val1
      else failwith "A variable cannot have multiple addresses."
    in
    union f lhs rhs

  let widen ~prev ~next =
    (* Environment does not need to be widen *)
    if (compare prev next) <> 0 then
      failwith "Cannot be widen two different environments."
    else
      next

  let pp = pp ~pp_value:Loc.pp
end

module InstEnv = struct
  module M = PrettyPrintable.MakePPMap(Loc)

  include (M: module type of M with type 'a t := 'a M.t)

  type t = AVS.t M.t

  let find loc ienv =
    match find_opt loc ienv with
    | Some s -> 
        s
    | None ->
        AVS.bot

  let add loc avs ienv =
    match find_opt loc ienv with
    | Some s ->
        add loc (AVS.union s avs) ienv
    | _ ->
        add loc avs ienv

  let pp = pp ~pp_value: AVS.pp
end

module Heap = struct
  module M = PrettyPrintable.MakePPMap(Loc) 
  module LocSet = PrettyPrintable.MakePPSet(Loc)

  include (M: module type of M with type 'a t := 'a M.t)

  type t = AVS.t M.t

  let pp = pp ~pp_value:AVS.pp

  let compare h1 h2 =
    (fun avs1 avs2 ->
      AVS.compare avs1 avs2)
    |> (fun x -> compare x h1 h2)

  let flatten_heap_locs heap =
  (fun loc avs locset ->
    let locset' = LocSet.add loc locset in
    (fun ((value: Val.t), _) locset ->
      match value with
      | Loc loc ->
          LocSet.add loc locset
      | _ ->
          locset)
    |> (fun f -> AVS.fold f avs locset'))
  |> (fun f -> fold f heap LocSet.empty)

  let find_offsets_of l heap =
    (fun (loc: Loc.t) -> match loc with Offset (base, _) -> base = l | _ -> false)
    |> (fun f -> LocSet.filter f (flatten_heap_locs heap))

  let find l heap =
    match find_opt l heap with
    | Some avs ->
        avs
    | None ->
        AVS.bot

  let ( <= ) lhs rhs =
    let f = fun key val1 ->
      match find_opt key rhs with
      | Some val2 -> 
          AVS.(val1 <= val2)
      | None -> 
          false
    in
    for_all f lhs

  let weak_update loc avs heap =
    match find_opt loc heap with
    | Some avs' -> 
        add loc (AVS.union avs avs') heap
    | None ->
        add loc avs heap

  let disjoint_union heap1 heap2 =
    union (fun _ _ _ -> failwith "Heaps are not disjoint!") heap1 heap2

  let opt_cst_in_heap heap =
    let opt_avs_heap = fun loc avs heap ->
      let avs' = AVS.optimize avs in
      if AVS.is_empty avs' then
        heap
      else
        add loc avs' heap
    in
    fold opt_avs_heap heap empty

  let optimize heap locs = 
    let heap' = opt_cst_in_heap heap in
    let f_heap_closure = fun locset loc ->
      let rec f_avs_closure: ValCst.t -> LocSet.t -> LocSet.t  = 
        fun (value, _) locset ->
          match value with
          | Loc loc ->
              let locset' = 
                LocSet.add loc locset
                |> LocSet.union (find_offsets_of loc heap') 
              in
              (match find_opt loc heap' with
              | Some avs -> 
                  AVS.fold f_avs_closure avs locset'
              | None ->
                  locset')
          | _ -> 
              locset
      in
      let locset' = LocSet.add loc locset in
      match find_opt loc heap' with
      | Some avs -> 
          AVS.fold f_avs_closure avs locset'
      | None ->
          locset'
    in
    let loc_closure = Caml.List.fold_left f_heap_closure LocSet.empty locs in
    filter (fun loc _ -> LocSet.mem loc loc_closure) heap'

  let join lhs rhs = 
    let lhs' = opt_cst_in_heap lhs in
    let rhs' = opt_cst_in_heap rhs in
    union (fun key val1 val2 -> Some (AVS.join val1 val2)) lhs' rhs'

  let widen ~prev ~next =
    (fun loc prev_v_opt next_v_opt ->
      match prev_v_opt, next_v_opt with
      | None, _ | _, None ->
          failwith "Cannot be widen: two heaps have different locations."
      | Some prev_v, Some next_v ->
          Some (AVS.widen ~prev:prev_v ~next:next_v))
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
end

module CallLogs = struct
  include PrettyPrintable.MakePPSet(LogUnit)

  let ( <= ) = subset

  let ( < ) lhs rhs = (lhs <= rhs) && (lhs <> rhs)

  let join = union

  let optimize logs = 
    let opt_unit u = 
      let args = LogUnit.get_args u in
      let heap = LogUnit.get_heap u in
      { u with LogUnit.heap = (Heap.optimize heap args) }
    in
    map opt_unit logs

  let widen ~prev ~next =
    if prev < next then (* log size is increasing *)
      failwith "we hope that logs are not increased in a loop."
    else
      join prev next

end

module Domain = struct
  type t = 
    { env: Env.t
    ; heap: Heap.t
    ; logs: CallLogs.t }

  let empty = 
    { env = Env.empty
    ; heap = Heap.empty
    ; logs = CallLogs.empty }
  let get_env s = s.env

  let get_heap s = s.heap

  let get_logs s = s.logs

  let init = 
    { env = Env.empty
    ; heap = Heap.empty
    ; logs = CallLogs.empty }

  let make env' heap' logs' = 
    { env = env'
    ; heap = heap'
    ; logs = logs' }

  let update_env env' s = { s with env = env' }

  let update_heap heap' s = { s with heap = heap' }

  let update_logs logs' s = { s with logs = logs' }

  let ( <= ) ~lhs ~rhs = 
    Env.( lhs.env <= rhs.env ) 
    && Heap.( lhs.heap <= rhs.heap ) 
    && CallLogs.( lhs.logs <= rhs.logs )

  let join lhs rhs = 
    { env = ( Env.join lhs.env rhs.env )
    ; heap = ( Heap.join lhs.heap rhs.heap )
    ; logs = ( CallLogs.join lhs.logs rhs.logs ) }

  let widen ~prev ~next ~num_iters = 
    if num_iters >= widen_iter then
      (* TODO: need to widen for loop statements *)
      let () = L.progress "\t\t\ ### WIDENING(%d)!! ### \n@." num_iters in 
      { env = Env.widen ~prev:prev.env ~next:next.env
      ; heap = Heap.widen ~prev:prev.heap ~next:next.heap
      ; logs = CallLogs.widen ~prev:prev.logs ~next:next.logs }
    else 
      join prev next 

  let rm_redundant ?f_name {env; heap; logs} = 
    let env' = Env.filter (fun v _ -> not (Var.is_temporal v)) env in
    let non_temp_locs = (
      match f_name with
      | Some f -> 
          Env.fold (fun _ loc loclist -> loc :: loclist) env' [Loc.mk_ret f]
      | None ->
          Env.fold (fun _ loc loclist -> loc :: loclist) env' [])
    in
    let heap' = Heap.optimize heap non_temp_locs in
    let logs' = CallLogs.optimize logs in
    make env' heap' logs'

  let optimize ?f_name astate = 
    rm_redundant astate ?f_name

  let pp fmt { env; heap; logs } =
    F.fprintf fmt "===\n%a\n%a\n%a\n===" Env.pp env Heap.pp heap CallLogs.pp logs
end

include Domain
