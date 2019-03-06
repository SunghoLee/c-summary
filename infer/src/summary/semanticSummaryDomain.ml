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

module Loc = struct
  type t = Bot
  | ConstLoc of int
  | Pointer of t
  | Ret of string
  [@@deriving compare]

  let loc_index = ref 0

  let bot = Bot

  let is_bot = function Bot -> true | _ -> false

  let is_const = function ConstLoc _ -> true | _ -> false

  let is_pointer = function Pointer _ -> true | _ -> false

  let is_ret = function Ret _ -> true | _ -> false

  let mk_const i = ConstLoc i

  let mk_pointer i = Pointer i

  let mk_ret i = Ret i

  let mk_ret_of_pname pname = Typ.Procname.to_string pname |> mk_ret

  let unwrap_ptr = function Pointer l -> l | _ -> failwith "This location is not a pointer."

  let new_const_loc () = loc_index := !loc_index + 1; ConstLoc !loc_index

  let get_index = function ConstLoc i -> i | _ -> failwith "It is not a constant location."

  let rec pp fmt = function
    Bot -> F.fprintf fmt "bot"
    | ConstLoc i -> F.fprintf fmt "C#%d" i
    | Pointer p -> F.fprintf fmt "*( %a )" pp p
    | Ret s -> F.fprintf fmt "R#%s" s

  let rec to_string = function
    Bot -> "bot"
    | ConstLoc i -> Printf.sprintf "C#%d" i
    | Pointer p -> Printf.sprintf "*( %s )" (to_string p)
    | Ret s -> Printf.sprintf "R#%s" s

end

module Var = struct
  type scope = Local | Global | Temporal [@@deriving compare]

  type t = Var of string * scope [@@deriving compare]

  let of_string s = 
    if String.is_prefix s "#GB" then 
      (Var (s, Global))
    else if String.contains s '$' then 
      (Var (s, Temporal))
    else 
      (Var (s, Local))

  let of_id id = 
    let id_name = Ident.get_name id in
    let stamp = Ident.get_stamp id in
    of_string ((Ident.name_to_string id_name)
      ^ "$" 
      ^ (string_of_int stamp))

  let of_pvar pvar = 
    let name = 
      if Pvar.is_global pvar then
        let tran_unit = Pvar.get_translation_unit pvar in
        match tran_unit with
        | Some s -> 
            "#GB_" 
            ^ (SourceFile.to_string s) 
            ^ "_" 
            ^ (Pvar.to_string pvar)
        | None -> 
            Pvar.to_string pvar
        else if Pvar.is_local pvar then
          Mangled.to_string (Pvar.get_name pvar)
        else failwith "no other variable types in C"
    in
    of_string name 

  let is_global = function Var (_, Global) -> true | _ -> false

  let is_temporal = function Var (_, Temporal) -> true | _ -> false

  let is_local = function Var (_, Local) -> true | _ -> false 

  let pp fmt = function Var (s, _) -> F.fprintf fmt "%s" s
end

module SStr = struct
  type t = String of string [@@deriving compare]

  let of_string s = String s

  let pp fmt = function String s -> F.fprintf fmt "\'%s\'" s
end

module Int = struct
  type t = Int of int | Top [@@deriving compare]

  let top = Top

  let is_top = function Top -> true | _ -> false

  let of_int i = Int i

  let to_int = function Int i -> i | _ -> failwith "Cannot unwrap the Top integer."

  let pp fmt = function Int i -> F.fprintf fmt "\'%d\'" i | Top -> F.fprintf fmt "T"
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

module Array = struct
  include Caml.List

  type t = Loc.t list [@@deriving compare]

  let pp fmt arr = 
    let rec pp_list fmt = function 
      [] -> 
      ()
      | h :: [] -> 
        F.fprintf fmt "%a" Loc.pp h
      | h :: t -> 
        F.fprintf fmt "%a, %a" Loc.pp h pp_list t
    in
    F.fprintf fmt "[%a]" pp_list arr
end

module Struct = struct
  include PrettyPrintable.MakePPMap(String)

  let pp = pp ~pp_value:Loc.pp
end

module Val = struct
  type t = Bot
    | Top
    | Loc of Loc.t
    | Str of SStr.t
    | Int of Int.t
    | Struct of Loc.t Struct.t
    | Array of Array.t
    | JClass of JClass.t
    | JMethodID of JMethodID.t
    | JFieldID of JFieldID.t
    [@@deriving compare]

  let bot = Bot

  let top = Top

  let of_loc l = Loc l

  let of_str s = Str s
  
  let of_int i = Int i

  let of_struct s = Struct s

  let of_array a = Array a

  let of_jclass jc = JClass jc

  let of_jmethod_id jm = JMethodID jm

  let of_jfield_id jf = JFieldID jf

  let is_bot = function Bot -> true | _ ->false

  let is_top = function Top -> true | _ ->false

  let is_loc = function Loc _ -> true | _ -> false

  let is_str = function Str _ -> true | _ -> false

  let is_int = function Int _ -> true | _ -> false

  let is_struct = function Struct _ -> true | _ -> false

  let is_array = function Array _ -> true | _ -> false

  let is_jclass = function JClass _ -> true | _ -> false

  let is_jmethod_id = function JMethodID _ -> true | _ -> false

  let is_jfield_id  = function JFieldID _ -> true | _ -> false

  let to_loc = function Loc l -> l | _ -> failwith "Wrong type argument."

  let to_str = function Str s -> s | _ -> failwith "Wrong type argument."

  let to_int = function Int i -> i | _ -> failwith "Wrong type argument."

  let to_struct = function Struct s -> s | _ -> failwith "Wrong type argument."

  let to_array = function Array a -> a | _ -> failwith "Wrong type argument."

  let to_jclass = function JClass jc -> jc | _ -> failwith "Wrong type argument."

  let to_jmethod_id = function JMethodID jm -> jm | _ -> failwith "Wrong type argument."

  let to_jfield_id = function JFieldID ji -> ji | _ -> failwith "Wrong type argument."

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
      F.fprintf fmt "%a" Int.pp i
    | Struct s -> 
      F.fprintf fmt "%a" Struct.pp s
    | Array a -> 
      F.fprintf fmt "%a" Array.pp a
    | JClass jc -> 
      F.fprintf fmt "%a" JClass.pp jc
    | JMethodID jm -> 
      F.fprintf fmt "%a" JMethodID.pp jm
    | JFieldID jf -> 
      F.fprintf fmt "%a" JFieldID.pp jf
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

    let map = ref Loc2SymMap.empty

    let index = ref 0 

    (* TODO: Should we handle constant locations as constant integers? *)
    let new_sym : context -> Loc.t -> Expr.expr = 
      fun ctxt loc ->
        let sort = Arithmetic.Integer.mk_sort ctxt in
        if Loc.is_const loc then
          Expr.mk_numeral_int ctxt (Loc.get_index loc) sort
        else 
          (index := !index + 1; (Expr.mk_const ctxt (Symbol.mk_int ctxt !index) sort))

    let find ctx loc = 
      match Loc2SymMap.find_opt loc !map with
      | Some s -> 
          s
      | None -> 
          (let nsym = new_sym ctx loc in
          (map := (Loc2SymMap.add loc nsym !map); nsym))

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
        Loc2SymMap.fold f !map RevMap.empty 
      in 
      let find_loc e =
        (*if Expr.is_const e then *)
          match RevMap.find_opt e rev_map with
          | None -> 
            failwith "some constraints go wrong."
          | Some s -> 
            s
        (* else 
          
          let () = L.progress "%s\n@." (Expr.to_string e) in
          failwith "0 constraint only have loc arguments." *)
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

  let join = union
end 

module Env = struct
  include PrettyPrintable.MakePPMap(Var)

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

  let pp = pp ~pp_value:Loc.pp
end

module InstEnv = struct
  include PrettyPrintable.MakePPMap(Loc)
  let pp = pp ~pp_value: AVS.pp
end

module Heap = struct
  include PrettyPrintable.MakePPMap(Loc) 

  let ( <= ) lhs rhs =
    let f = fun key val1 ->
      match find_opt key rhs with
      | Some val2 -> (val1 <= val2)
      | None -> false
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

  let join lhs rhs = 
    union (fun key val1 val2 -> Some (AVS.join val1 val2)) lhs rhs

  let pp = pp ~pp_value:AVS.pp
end

module LogUnit = struct
  type t = 
    { ret_sym: Loc.t
    ; jfun: JNIFun.t
    ; args: Loc.t list
    ; heap: AVS.t Heap.t}
    [@@deriving compare]

  let get_heap l = l.heap

  let get_args l = l.args

  let get_ret l = l.ret_sym

  let get_jfun l = l.jfun

  let make ret_sym' jfun' args' heap' = 
    { ret_sym = ret_sym'
    ; jfun = jfun'
    ; args = args'
    ; heap = heap' }

  let update_heap heap' l = { l with heap = heap' }

  let pp fmt = function {ret_sym; jfun; args; heap} -> 
    let rec pp_list fmt = function
      [] ->
        ()
      | h :: [] ->
        F.fprintf fmt "%a" Loc.pp h
      | h :: t ->
        F.fprintf fmt "%a, %a" Loc.pp h pp_list t
    in
    F.fprintf fmt "{%a; %a; %a; %a}" Loc.pp ret_sym JNIFun.pp jfun pp_list args Heap.pp heap
end

module CallLogs = struct
  include PrettyPrintable.MakePPSet(LogUnit)

  let ( <= ) = subset

  let join = union
end

module Domain = struct
  type t = 
    { env: Loc.t Env.t
    ; heap: AVS.t Heap.t
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
    ( lhs.env <= rhs.env ) 
    && ( lhs.heap <= rhs.heap ) 
    && ( lhs.logs <= rhs.logs )

  let join lhs rhs = 
    { env = ( Env.join lhs.env rhs.env )
    ; heap = ( Heap.join lhs.heap rhs.heap )
    ; logs = ( CallLogs.join lhs.logs rhs.logs ) }

  let widen ~prev ~next ~num_iters = join prev next

  let pp fmt { env; heap; logs } =
    F.fprintf fmt "===\n%a\n%a\n%a\n===" Env.pp env Heap.pp heap CallLogs.pp logs

  let pp_summary fmt (pre, post) = 
    F.fprintf fmt "%a\n%a" pp pre pp post
end

include Domain
