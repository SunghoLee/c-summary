(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open Core
module F = Format
module L = Logging

let comp_int i1 i2 = 
    if i1 > i2 then 1
    else if i2 > i1 then -1
    else 0

module type Basic = sig
    type t
    val compare : t -> t -> int
    val pp : t -> string
end

module type SDomain = sig
    include Basic
    val empty : t
    val top : t
    val is_empty : t -> bool
    val is_top : t -> bool
    val ( <= ) : t -> t -> bool
    val join : t -> t -> t
end

module rec AbsLoc : sig
    include Basic 
    val equal : t -> t -> bool
    val top : t
    val ct_const : int -> t
    val ct_symbol : int -> t
    val ct_pointer : t -> t
    val is_symbol : t -> bool
    val is_const : t -> bool
    val is_pointer : t -> bool
    val is_top : t -> bool
end = struct
    type t = ConstLoc of int
    | Symbol of int
    | Pointer of t
    | LocTop

    let is_symbol = function 
        Symbol _ -> true
        | _ -> false
    let is_const = function
        ConstLoc _ -> true
        | _ -> false
    let is_pointer = function
        Pointer _ -> true
        | _ -> false
    let is_top = function
        LocTop -> true
        | _ -> false
    let ct_const i = ConstLoc i
    let ct_symbol i = Symbol i
    let ct_pointer i = Pointer i

    let rec compare lhs rhs = 
        match lhs, rhs with
        | LocTop, LocTop -> 0
        | LocTop, _ -> 1
        | _, LocTop -> -1
        | ConstLoc i1, ConstLoc i2 -> comp_int i1 i2
        | ConstLoc i1, Symbol i2 -> comp_int i1 i2
        | Symbol i1, ConstLoc i2 -> comp_int i1 i2
        | Symbol s1, Symbol s2 -> comp_int s1 s2
        | Pointer p1, Pointer p2 -> compare p1 p2
        | Pointer _, _ -> -1
        | _, Pointer _ -> 1
    let equal l1 l2 = 
        if (phys_equal (compare l1 l2) 0) then true
        else false
    let rec pp = function
        ConstLoc i -> Printf.sprintf "C#%d" i
        | Symbol i -> Printf.sprintf "S#%d" i
        | Pointer p -> "*(" ^ (pp p) ^ ")"
        | LocTop -> "LocTop"
    let top = LocTop
end


module Var : sig
    include Basic
    val equal : t -> t -> bool
    val create : string -> t
    val is_global : t -> bool
    val is_local : t -> bool
    val is_temporal : t -> bool
    val is_ret : t -> bool
end = struct
    type var_scope = Global
        | Local
        | Temporal

    type t = Var of string * var_scope

    let create s = 
        if String.is_prefix s "#GB" then (Var (s, Global))
        else if String.contains s '$' then (Var (s, Temporal))
        else (Var (s, Local))

    let is_global = function
        | Var (_, Global) -> true
        | _ -> false

    let is_temporal = function
        | Var (_, Temporal) -> true
        | _ -> false

    let is_local = function
        | Var (_, Local) -> true
        | _ -> false 
    let is_ret = function
        Var (n, _) -> String.equal n "return"

    let compare lhs rhs = 
        match lhs, rhs with
        | Var (s1, _), Var (s2, _) -> String.compare s1 s2

    let equal lhs rhs =
        if (phys_equal (compare lhs rhs) 0) then true
        else false

    let pp = function
        Var (s, _) -> Printf.sprintf "%s" s
end

module Str : sig
    include Basic 
    val create : string -> t
end = struct
    type t = String of string

    let create s = String s

    let compare lhs rhs = 
    match lhs, rhs with
    | String s1, String s2 -> String.compare s1 s2

    let pp = function
        String s -> Printf.sprintf "\"%s\"" s
end

module Int : sig
    include Basic 
    val create : int -> t
    val top : t
    val is_top : t -> bool
    val unwrap : t -> int
end = struct
    type t = Int of int
    | T

    let top = T

    let is_top = function
    T -> true
    | _ -> false

    let create s = Int s

    let unwrap = function
        | Int i -> i
        | _ -> failwith "Cannot unwrap the Top integer."

    let compare lhs rhs = 
    match lhs, rhs with
    | T, T -> 0
    | T, _ -> 1
    | _, T -> -1
    | Int s1, Int s2 -> if s1 < s2 then -1
    else if (phys_equal s1 s2) then 0
    else 1

    let pp = function
        Int s -> Printf.sprintf "\"%d\"" s
    | T -> "T"
end

module JNIFun : sig
    include Basic
    val create : string -> t
end = struct
    type t = JNIFun of string

    let create s = JNIFun s

    let compare lhs rhs = 
        match lhs, rhs with
        | JNIFun s1, JNIFun s2 -> String.compare s1 s2

    let pp = function
        JNIFun s -> Printf.sprintf "JF[%s]" s
end

module rec Sval : sig
    include Basic 
end = struct
    type t = JClass of string
    | JMethodID of Sval.t * string
    | JFieldID of Sval.t * string

    let rec compare lhs rhs =
    match lhs, rhs with
    | JClass s1, JClass s2 -> String.compare s1 s2
    | JClass _, _ -> -1 
    | _, JClass _ -> 1
    | JMethodID (sv1', s1), JMethodID (sv2', s2) -> 
        let cc = compare sv1' sv2' in
        (match cc with
        | 0 -> String.compare s1 s2
        | _ -> cc)
    | JMethodID _, _ -> 1
    | _, JMethodID _ -> -1
    | JFieldID (sv1', s1), JFieldID (sv2', s2) -> 
        let cc = compare sv1' sv2' in
        (match cc with
        | 0 -> String.compare s1 s2
        | _ -> cc)
    | JFieldID _, _ -> -1 
    | _, JFieldID _ -> 1 

    let rec pp = function
        JClass s -> Printf.sprintf "JClass: %s" s
        | JMethodID (sval, s) -> Printf.sprintf "%s - JMethod: %s" (pp sval) s
        | JFieldID (sval, s) -> Printf.sprintf "%s - JField: %s" (pp sval) s
end

module Array = struct
    include Caml.List

    type t = AbsLoc.t list

    let compare arr1 arr2 = 
        let comp_res = Caml.List.compare_lengths arr1 arr2 in
        if (not (phys_equal comp_res 0)) then comp_res
        else
            let res = ref 0 in
            let is_eq = Caml.List.for_all2 (fun x y -> 
                let loc_comp_res = AbsLoc.compare x y in
                if (not (phys_equal loc_comp_res 0)) then (res := loc_comp_res; false)
                else true) arr1 arr2
            in
            !res

    let pp arr = 
        if (phys_equal (length arr) 0) then "Bot"
        else
            let fst = ref true in
            let f i v = 
                if !fst then (fst := false ; Printf.sprintf "%s%s" i (AbsLoc.pp v))
                else Printf.sprintf "%s, %s" i (AbsLoc.pp v) 
            in
            Printf.sprintf "%s]" (fold_left f "[" arr)

end

module Struct = struct
    include Caml.Map.Make(Typ.Fieldname)

    let compare s1 s2 = 
        compare AbsLoc.compare s1 s2	

    let pp s = 
        if (phys_equal (cardinal s) 0) then "Bot"
        else
            let fst = ref true in
            let f k v i = 
                if !fst then (fst := false ; Printf.sprintf "%s %s -> %s" i (Typ.Fieldname.to_string k) (AbsLoc.pp v)) 
                else Printf.sprintf "%s, %s -> %s" i (Typ.Fieldname.to_string k) (AbsLoc.pp v) in
                Printf.sprintf "%s}" (fold f s "{")
end

module AbsVal : sig
    include Basic
    val top : t
    val bot : t
    val loc_to_val : AbsLoc.t -> t
    val str_to_val : Str.t -> t
    val int_to_val : Int.t -> t
    val sval_to_val : Sval.t -> t
    val struct_to_val : AbsLoc.t Struct.t -> t
    val array_to_val : Array.t -> t
    val val_to_loc : t -> AbsLoc.t
    val val_to_int : t -> Int.t
    val val_to_str : t -> Str.t
    val val_to_sval : t -> Sval.t
    val val_to_struct : t -> AbsLoc.t Struct.t
    val val_to_array : t -> Array.t
    val is_int : t -> bool
    val is_str : t -> bool
    val is_loc : t -> bool
    val is_sval : t -> bool
    val is_struct : t -> bool
    val is_array : t -> bool
end = struct
    type t = L of AbsLoc.t
    | ConstStr of Str.t
    | ConstInt of Int.t
    | Struct of AbsLoc.t Struct.t
    | Sval of Sval.t
    | Array of Array.t
    | T
    | B

    let pp = function
        L l -> AbsLoc.pp l
        | ConstStr s -> Str.pp s
        | ConstInt s -> Int.pp s
        | Sval s -> Sval.pp s
        | Struct s -> Struct.pp s
        | Array s -> Array.pp s
        | T -> "T"
        | B -> "Bot"

    let loc_to_val l = L l
    let str_to_val s = ConstStr s
    let int_to_val s = ConstInt s
    let sval_to_val s = Sval s
    let struct_to_val s = Struct s
    let array_to_val s = Array s

    let is_int = function
        | ConstInt _ -> true
        | _ -> false

    let is_str = function
        | ConstStr _ -> true
        | _ -> false

    let is_loc = function
        | L _ -> true
        | _ -> false

    let is_sval = function
        | Sval _ -> true
        | _ -> false

    let is_struct = function
        | Struct _ -> true
        | _ -> false

    let is_array = function
        | Array _ -> true
        | _ -> false

    let val_to_loc v = 
        match v with
        | L l -> l
        | _ -> failwith ("wrong type: " ^ (pp v))
    let val_to_str = function
        ConstStr s -> s
        | _ -> failwith "wrong type"
    let val_to_int = function
        ConstInt s -> s
        | _ -> failwith "wrong type"
    let val_to_sval = function
        Sval s -> s
        | _ -> failwith "wrong type"
    let val_to_struct = function
        Struct s -> s
        | _ -> failwith "wrong type"
    let val_to_array s = 
        match s with
        | Array s -> s
        | _ -> failwith ("wrong type" ^ (pp s))
    let top = T
    let bot = B
    let compare lhs rhs = 
        match lhs, rhs with
        | B, B -> 0
        | B, _ -> -1
        | _, B -> 1
        | T, T -> 0
        | T, _ -> 1
        | _, T -> -1
        | ConstInt i1, ConstInt i2 -> Int.compare i1 i2
        | ConstInt _, _ -> -1
        | _, ConstInt _ -> 1
        | ConstStr s1, ConstStr s2 -> compare s1 s2
        | ConstStr _, _ -> -1
        | _, ConstStr _ -> 1
        | Sval s1, Sval s2 -> compare s1 s2
        | Sval _, _ -> -1
        | _, Sval _ -> 1
        | L l1, L l2 -> compare l1 l2
        | L _, _ -> -1
        | _, L _ -> 1
        | Struct _, _ -> -1
        | _, Struct _ -> 1
        | Struct s1, Struct s2 -> Struct.compare s1 s2

end

module Cst : sig
    include Basic 
    val top : t
    val cst_t : t
    val cst_f : t
    val cst_and : t -> t -> t
    val cst_or : t -> t -> t
    val cst_eq : AbsLoc.t -> AbsLoc.t -> t
    val cst_not : t -> t
    val encode : Z3.context -> t -> Z3.Expr.expr
    val decode : Z3.Expr.expr -> t
end = struct
    open Z3
    type t = T
    | F
    | Or of t * t
    | And of t * t
    | Not of t
    | Eq of AbsLoc.t * AbsLoc.t
    | CstTop

    let top = CstTop

    let cst_t = T
    let cst_f = F
    let cst_and c1 c2 = And (c1, c2)
    let cst_or c1 c2 = Or (c1, c2)
    let cst_eq c1 c2 = Eq (c1, c2)
    let cst_not c = Not c

    module Loc2SymMap = Caml.Map.Make(AbsLoc)
    let map = ref Loc2SymMap.empty
    let index = ref 0 

    (* TODO: Should we handle constant locations as constant integers? *)
    let getNewSym ctx = 
        let sort = Arithmetic.Integer.mk_sort ctx in
        index := !index + 1; (Expr.mk_const ctx (Symbol.mk_int ctx !index) sort)

    let find ctx loc = 
       match Loc2SymMap.find_opt loc !map with
       | None -> 
               (let nsym = getNewSym ctx in
               (map := (Loc2SymMap.add loc nsym !map); nsym))
       | Some s -> s

    let rec encode ctx = function
       T -> Boolean.mk_true ctx
       | F -> Boolean.mk_false ctx
       | Or (cst1, cst2) -> Boolean.mk_or ctx [encode ctx cst1; encode ctx cst2]
       | And (cst1, cst2) -> Boolean.mk_and ctx [encode ctx cst1; encode ctx cst2]
       | Not cst -> Boolean.mk_not ctx (encode ctx cst)
       | Eq (loc1, loc2) -> Boolean.mk_eq ctx (find ctx loc1) (find ctx loc2)
       | CstTop -> failwith "Cannot handle top constraints"
    
    let decode e = 
        let module RevMap = Caml.Map.Make(
            struct 
               include Z3.Expr
               type t = expr
            end) in
        let rev_map = 
            let f l s m = 
                match RevMap.find_opt s m with
                | Some _ -> failwith "cannot be duplicated!"
                | None -> RevMap.add s l m
            in
            Loc2SymMap.fold f !map RevMap.empty 
        in 
        let find_loc e =
            let str = RevMap.fold (fun x v i -> i ^ (Expr.to_string x) ^ " -> " ^ (AbsLoc.pp v) ^ "\n") rev_map "" in
            if Expr.is_const e then
                match RevMap.find_opt e rev_map with
                | None -> failwith (str ^ "cannot find " ^ (Expr.to_string e) ^ "\n")
                | Some s -> s
            else failwith "0 constraint only have loc arguments."
        in
        let rec decode_impl e = 
            if Boolean.is_true e then cst_t
            else if Boolean.is_false e then cst_f
            else if Boolean.is_and e then
                match Expr.get_args e with
                | [fst; snd] -> cst_and (decode_impl fst) (decode_impl snd)
                | fst :: rest -> Caml.List.fold_left (fun i x -> cst_and i (decode_impl x)) (decode_impl fst) rest
                | _ -> failwith "'and' boolean constraint does not have any sub-constraints."
            else if Boolean.is_or e then
                match Expr.get_args e with
                | [fst; snd] -> cst_or (decode_impl fst) (decode_impl snd)
                | fst :: rest -> Caml.List.fold_left (fun i x -> cst_or i (decode_impl x)) (decode_impl fst) rest
                | _ -> failwith "'or' boolean constraint does not have any sub-constraints."
            else if Boolean.is_not e then
                match Expr.get_args e with
                | [fst] -> cst_not (decode_impl fst) 
                | _ -> failwith "not constraint only has one argument."
            else if Boolean.is_eq e then
                match Expr.get_args e with
                | [fst; snd] -> cst_eq (find_loc fst) (find_loc snd)
                | _ -> failwith "0 constraint only has two arguments."
            else 
                failwith "Currnetly, this contraint is not considered as a Cst type."
        in
        decode_impl e 

    let rec compare lhs rhs = 
        let ordering cst1 cst2 = 
            match compare cst1 cst2 with
            | 1 -> (cst2, cst1)
            | _ -> (cst1, cst2)
            in
            let ordered_cmp (c11, c12) (c21, c22) =
                let (oc11, oc12) = ordering c11 c12 in
                let (oc21, oc22) = ordering c21 c22 in
                let res = compare oc11 oc21 in
                (match res with
                | 0 -> compare oc12 oc22
                | _ -> res)
            in
            match lhs, rhs with
            | CstTop, CstTop -> 0
            | CstTop, _ -> 1
            | _, CstTop -> -1
            | T, T -> 0
            | T, _ -> 1
            | _, T -> -1
            | F, F -> 0
            | F, _ -> 1
            | _, F -> -1
            | Not c1', Not c2' -> compare c1' c2'
            | Not _, _ -> 1
            | _, Not _ -> -1
            | And (c11, c12), And (c21, c22) -> ordered_cmp (c11, c12) (c21, c22)
            | And _, _ -> 1
            | _, And _ -> -1
            | Or (c11, c12), Or (c21, c22) -> ordered_cmp (c11, c12) (c21, c22)
            | Or _, _ -> 1
            | _, Or _ -> -1
            | Eq (c11, c12), Eq (c21, c22) -> 
                let gt_fst, lt_fst = (match AbsLoc.compare c11 c12 with
                    | 1 -> c11, c12
                    | 0 -> c11, c12
                    | _ -> c12, c11)
                in
                let gt_snd, lt_snd = (match AbsLoc.compare c21 c22 with
                    | 1 -> c21, c22
                    | 0 -> c21, c22
                    | _ -> c22, c21)
                in
                let res = AbsLoc.compare gt_fst gt_snd in
                if phys_equal res 0 then
                    AbsLoc.compare lt_fst lt_snd
                else
                    res
            | Eq _, _ -> 1
            | _, Eq _ -> -1

    let rec pp = function
        T -> "T"
        | F -> "F"
        | Or (cst1, cst2) -> Printf.sprintf "(%s) || (%s)" (pp cst1) (pp cst2)
        | And (cst1, cst2) -> Printf.sprintf "(%s) && (%s)" (pp cst1) (pp cst2)
        | Not cst -> Printf.sprintf "!(%s)" (pp cst)
        | Eq (cst1, cst2) -> Printf.sprintf "(%s) == (%s)" (AbsLoc.pp cst1) (AbsLoc.pp cst2)
        | CstTop -> "CstTop"
end

module LocList : sig
    include Basic with type t = AbsLoc.t list
    val cons : AbsLoc.t -> t -> t
    val rev : t -> t
    val empty : t
end = struct
    type t = AbsLoc.t list

    let empty = []

    let cons a b = a :: b

    let rev l = Caml.List.rev l

    let rec compare l1 l2 =
        match l1, l2 with
        | h1::t1, h2::t2 -> 
            (match AbsLoc.compare h1 h2 with
            | i when not (phys_equal i 0) -> AbsLoc.compare h1 h2
            | _ -> compare t1 t2)
        | _::_, [] -> 1
        | [], _::_ -> -1
        | _, _ -> 0

    let pp ll = 
        let fst = ref true in
        let f i l = 
            if !fst then (fst := false ; Printf.sprintf "%s%s" i (AbsLoc.pp l)) 
            else Printf.sprintf "%s, %s" i (AbsLoc.pp l) in
            Printf.sprintf "%s]" (Caml.List.fold_left f "[" ll)
end

module AbsValWCst : sig
    include Basic with type t = AbsVal.t * Cst.t
    val top : t
    val create : AbsVal.t -> Cst.t -> t
    val get_val : t -> AbsVal.t
    val get_cst : t -> Cst.t
end
= struct
    type t = AbsVal.t * Cst.t
    
     let top = AbsVal.top, Cst.top

     let create v c = (v, c)

     let get_val = function
         v, _ -> v

     let get_cst = function
         _, cst -> cst

     let pp (av, cst) = (AbsVal.pp av) ^ " * " ^ (Cst.pp cst)

    (* Total order in abs_val domain. We assume more concrete one is bigger(-1) than others.*)
    let compare (t1:t) (t2:t) = (*(abs1, cst1: cst) (abs2, cst2: cst) = *)
        let ((abs1, cst1), (abs2, cst2)) = (t1, t2) in
        let abs_res = AbsVal.compare abs1 abs2 in
        let res = match abs_res with
        | 0 -> compare cst1 cst2
        | _ -> abs_res
        in
        res

end

module AbstractValueSet : sig
    include Basic
    val equal : t -> t -> bool
    val top : t
    val is_top : t -> bool
    val join : t -> t -> t
    val union : t -> t -> t
    val add : AbsValWCst.t -> t -> t
    val empty : t
    val is_empty : t -> bool
    val singleton : AbsValWCst.t -> t
    val find_first : (AbsValWCst.t -> bool) -> t -> AbsValWCst.t
    val fold: (AbsValWCst.t -> 'a -> 'a) -> t -> 'a -> 'a
    val cardinal : t -> int
    val elements : t -> AbsValWCst.t list
    val ( <= ) : t -> t -> bool
end = struct
    module Base = Caml.Set.Make(AbsValWCst)
    type t = AVS of Base.t
        | T
    let empty = AVS Base.empty
    let top = T
    let is_top = function
        T -> true
        | _ -> false
    let is_empty = function
        T -> false
        | AVS s' -> Base.is_empty s'
    let mem e s = 
        match s with
        | T -> true
        | AVS s' -> Base.mem e s'
    let add e s =
        match s with
        | T -> T
        | AVS s' -> AVS (Base.add e s')
    let singleton e = AVS (Base.singleton e)
    let remove e s = 
        match s with
        | T -> s
        | AVS s' -> AVS (Base.remove e s')
    let union s1 s2 =
        match s1, s2 with
        | T, _ -> T
        | _, T -> T
        | AVS s1', AVS s2' -> AVS (Base.union s1' s2')
    let compare s1 s2 =
        match s1, s2 with
        | T, T -> 0
        | T, _ -> 1
        | _, T -> -1
        | AVS s1', AVS s2' -> Base.compare s1' s2'
    let equal s1 s2 =
        match s1, s2 with
        | T, T -> true
        | T, _ -> false
        | _, T -> false
        | AVS s1', AVS s2' -> Base.equal s1' s2'
    let subset s1 s2 =
        match s1, s2 with
        | T, T -> true
        | T, _ -> false
        | _, T -> true
        | AVS s1', AVS s2' -> Base.subset s1' s2'
    let fold f s i = 
        match s with
        | T -> i
        | AVS s' -> Base.fold f s' i
    let pp = function
        T -> "T"
        | AVS s' -> 
            if (phys_equal (Base.cardinal s') 0) then "Bot"
            else
                let fst = ref true in
                let f (v, c) i = 
                    if !fst then (fst := false ; Printf.sprintf "%s%s in %s" i (AbsVal.pp v) (Cst.pp c)) 
                    else Printf.sprintf "%s, %s in %s" i (AbsVal.pp v) (Cst.pp c) in
                    Printf.sprintf "%s}" (Base.fold f s' "{")

    let find_first f s =
       match s with
       | T -> AbsValWCst.top
       | AVS s' -> 
           let b = Base.elements s' in
           let rec ff = function
               [] -> failwith "no!!"
               | h :: t -> 
                   if f h then h
                   else ff t
               in ff b
               (* Base.find_first f s'  *)

    let cardinal = function
        T -> -1
        | AVS s -> Base.cardinal s 
    let elements = function
        T -> [AbsValWCst.top]
        | AVS s -> Base.elements s
    let equal s1 s2 = 
        if (phys_equal (compare s1 s2) 0) then true
        else false
            
    (* partial order of AbstractValueSet *)
    let ( <= ) = subset

    let join lhs rhs = 
        match lhs, rhs with
        | _, T -> T
        | T, _ -> T
        | AVS s1, AVS s2 -> 
            AVS (Base.union s1 s2)

    let union lhs rhs = 
        match lhs, rhs with
        | _, T -> T
        | T, _ -> T
        | AVS s1, AVS s2 -> 
                let res = AVS (Base.union s1 s2) in
                res
end 

module Env : sig 
    include SDomain
    val add : Var.t -> AbsLoc.t -> t -> t
    val find : Var.t -> t -> AbsLoc.t
    val find_opt : Var.t -> t -> AbsLoc.t option
    val mem : Var.t -> t -> bool
end = struct
    module Base = Caml.Map.Make(Var)

    type t = E of (AbsLoc.t Base.t)
        | T
    let empty = E Base.empty
    let top = T
    let is_empty = function
        T -> false
        | E s -> Base.is_empty s
    let is_top = function
        T -> true
        | _ -> false
    let mem k m =
        match m with
        | T -> true
        | E m' -> Base.mem k m'
    let add k e m =
        match m with
        | T -> T
        | E m' -> E (Base.add k e m')
    let compare m1 m2 = 
        match m1, m2 with
        | T, T -> 0
        | T, _ -> 1
        | _, T -> -1
        (* tricky implementation *)
        | E m1', E m2' -> 
                if Base.equal AbsLoc.equal m1' m2' then 0
                else -1
    let find k m =
        match m with
        | T -> AbsLoc.top
        | E m' -> Base.find k m'
    let find_opt k m = 
        match m with
        | T -> (Some AbsLoc.top)
        | E m' -> Base.find_opt k m'
    let mem k m =
        match m with
        | T -> true
        | E m' -> Base.mem k m'
    let ( <= ) lhs rhs = 
        match lhs, rhs with
        | E s, _ when Base.is_empty s -> true
        | _, E s when Base.is_empty s -> false
        | _, T -> true
        | T, _ -> false
        | E l, E r -> 
            let p k v = 
                if Base.mem k r then
                    let rv = Base.find k r in
                        if phys_equal (AbsLoc.compare v rv) 0 then
                            true
                        else 
                            false
                else
                    false
                in
                Base.for_all p l

    let pp = function
        T -> "T"
        | E m -> 
            let fst = ref true in
            let f v l i = 
                if !fst then (fst := false 
                    ; Printf.sprintf "%s%s -> %s" i (Var.pp v) (AbsLoc.pp l)) 
                else 
                    Printf.sprintf "%s, %s -> %s" i (Var.pp v) (AbsLoc.pp l) 
            in
            Printf.sprintf "%s]" (Base.fold f m "[")

    let join lhs rhs = 
        L.progress "[Warn] Trying to join envs: \n fst: %s\n snd: %s\n@." (pp lhs) (pp rhs);
        match lhs, rhs with
        | _, E m when Base.is_empty m -> lhs
        | E m, _ when Base.is_empty m -> rhs
        | _, T -> T
        | T, _ -> T
        | E m1, E m2 ->
            E (Base.union (fun k v1 v2 -> 
                if (phys_equal (AbsLoc.compare v1 v2) 0) then Some v1 
                else failwith "A variable cannot have multiple addresses") m1 m2)
end

module Heap : sig
    include SDomain
    val add : AbsLoc.t -> AbstractValueSet.t -> t -> t
    val update : AbsLoc.t -> AbsValWCst.t -> t -> t
    val find : AbsLoc.t -> t -> AbstractValueSet.t
    val find_opt : AbsLoc.t -> t -> AbstractValueSet.t option
    val iter : (AbsLoc.t -> AbstractValueSet.t -> unit) -> t -> unit
    val fold : (AbsLoc.t -> AbstractValueSet.t -> 'a -> 'a) -> t -> 'a -> 'a
    val disjoint_union : t -> t -> t
end = struct
    module Base = Caml.Map.Make(AbsLoc)
    type t = H of (AbstractValueSet.t Base.t)
        | T
    let empty = H Base.empty  
    let top = T

    let is_empty = function
        | T -> false
        | H m -> Base.is_empty m
    let is_top = function
        | T -> true
        | _ -> false
    let mem k m =
        match m with
        | T -> true
        | H m' -> Base.mem k m'
    let add k e m =
        match m with
        | T -> T 
        | H m' -> H (Base.add k e m')
    let compare m1 m2 = 
        match m1, m2 with
        | T, T -> 0
        | T, _ -> 1
        | _, T -> -1
        (* tricky implementation *)
        | H m1', H m2' -> 
                if Base.equal AbstractValueSet.equal  m1' m2' then 0
                else -1
    let find k m =
        match m with
        | T -> AbstractValueSet.top
        | H m' -> 
                (match Base.find_opt k m' with
                | None -> AbstractValueSet.singleton (AbsVal.bot, Cst.cst_t)
                | Some s -> s)
    let find_opt k m = 
        match m with
        | T -> (Some AbstractValueSet.top)
        | H m' -> Base.find_opt k m'
    let disjoint_union h1 h2 =
        match h1, h2 with
        | T, _ | _, T -> T
        | H m1, H m2 -> 
                H (Base.union (fun x v1 v2 -> failwith "Two heaps are not disjoint.") m1 m2)
    let pp = function
        T -> "T"
        | H m -> 
                let fst = ref true in
                let f l vs i = if !fst then (fst := false ; Printf.sprintf "%s%s -> %s" i (AbsLoc.pp l) (AbstractValueSet.pp vs)) else Printf.sprintf "%s, %s -> %s" i (AbsLoc.pp l) (AbstractValueSet.pp vs) in
            Printf.sprintf "%s]" (Base.fold f m "[")

    let update f t m =
        match m with
        | T -> m
        | H m' -> 
                let uf p = 
                   (match p with
                   | None -> Some (AbstractValueSet.singleton t)
                   | Some s -> Some (AbstractValueSet.add t s)
                   )
                   in H (Base.update f uf m')

    let iter f m =
        match m with
        | T -> ()
        | H m' -> Base.iter f m'
    let fold f m i =
        match m with
        | T -> failwith "Cannot be a top"
        | H m' -> Base.fold f m' i

    let ( <= ) lhs rhs =
        match lhs, rhs with
        | _, T -> true
        | T, _ -> false
        | H m, _ when Base.is_empty m -> true
        | _, H m when Base.is_empty m -> false
        | H m1, H m2 -> 
                let f k v1 = 
                    match Base.find_opt k m2 with
                    | None -> false
                    | Some v2 -> (v1 <= v2)
                in
                Base.for_all f m1

    let join lhs rhs = 
        match lhs, rhs with
        | _, T -> T
        | T, _ -> T
        | H m1, H m2 -> 
            H (Base.union (fun k v1 v2 -> Some (AbstractValueSet.join v1 v2)) m1 m2)

end

module LogUnit : sig
    type t =
        { ret: AbsLoc.t
        ; jfun: JNIFun.t
        ; args: LocList.t
        ; heap: Heap.t
        }
        val compare : t -> t -> int
        val pp : t -> string
    val create: AbsLoc.t -> JNIFun.t -> LocList.t -> Heap.t -> t
    val get_heap : t -> Heap.t
end = struct
    type t = {ret: AbsLoc.t; jfun: JNIFun.t; args: LocList.t; heap: Heap.t}

    let create r j a h = {ret=r; jfun=j; args=a; heap=h}

    let get_heap l = l.heap

    let compare (t1: t) (t2: t) = 
        let res_loc = AbsLoc.compare t1.ret t2.ret
        in
        match res_loc with
        | 0 -> (let res_jfun = JNIFun.compare t1.jfun t2.jfun
            in 
            match res_jfun with
            | 0 -> (let res_args = LocList.compare t1.args t2.args 
                in
                match res_args with
                | 0 -> Heap.compare t1.heap t2.heap
                | _ -> res_args)
            | _ -> res_jfun)
        | _ -> res_loc

    let pp = function
        {ret = r; jfun = jf; args = ll; heap = h} -> 
            Printf.sprintf "{%s; %s; %s; %s}" (AbsLoc.pp r) (JNIFun.pp jf) (LocList.pp ll) (Heap.pp h) 
end

module CallLogs : sig
    include SDomain
    val fold : (LogUnit.t -> 'a -> 'a) -> t -> 'a -> 'a
    val union : t -> t -> t
    val add: LogUnit.t -> t -> t
    val map: (LogUnit.t -> LogUnit.t) -> t -> t
end = struct
    module Base = Caml.Set.Make(LogUnit)
    type t = L of Base.t
        | T
    let empty = L Base.empty
    let top = T
    let is_empty = function
        T -> false
        | L m -> Base.is_empty m
    let is_top = function
        T -> true
        | _ -> false
    let compare (t1: t) (t2: t) = 
        match t1, t2 with
        | T, T -> 0
        | T, _ -> 1
        | _, T -> -1
        | L s1, L s2 -> Base.compare s1 s2
    let fold f s i =
        match s with
        | T -> failwith "Impossible"
        | L s' -> Base.fold f s' i
    let union l r =
        match l, r with
        | T, _ | _, T -> T
        | L s1, L s2 -> L (Base.union s1 s2)
    let add l s =
        match s with
        | T -> T
        | L s' -> L (Base.add l s')
    let map f s =
        match s with
        | T -> T
        | L s' -> L (Base.map f s')
    let pp = function
        T -> "T"
        | L s -> 
                let fst = ref true in
                let f l i = if !fst then (fst := false ; Printf.sprintf "%s%s" i (LogUnit.pp l)) else Printf.sprintf "%s, %s" i (LogUnit.pp l) in
            Printf.sprintf "%s}" (Base.fold f s "{")

    let ( <= ) lhs rhs = 
        match lhs, rhs with
        | L m, _ when Base.is_empty m -> true
        | _, L m when Base.is_empty m -> false
        | _, T -> true
        | T, _ -> false
        | L m1, L m2 -> Base.subset m1 m2

    let join lhs rhs = 
        match lhs, rhs with
        | _, T -> T
        | T, _ -> T
        | L m1, L m2 -> L (Base.union m1 m2)
end

module Domain : sig
    include AbstractDomain.S
    val empty : t 
    val make : Env.t -> Heap.t -> CallLogs.t -> t
    val to_triple : t -> Env.t * Heap.t * CallLogs.t
    val pp_summary : F.formatter -> t * t -> unit
end = struct
    (* abstract state *)
    type t = {env:Env.t; heap:Heap.t; logs:CallLogs.t}

    let make e h l = {env=e; heap=h; logs=l}

    let to_triple astate = (astate.env, astate.heap, astate.logs)
    (* top and bottom *)
    let top = {env=Env.top; heap=Heap.top; logs=CallLogs.top}
    let empty = {env=Env.empty; heap=Heap.empty; logs=CallLogs.empty}

    (* is top or bottom? *)
    let is_top state = Env.is_top state.env || Heap.is_top state.heap || CallLogs.is_top state.logs
    let is_empty state = Env.is_empty state.env || Heap.is_empty state.heap || CallLogs.is_empty state.logs

    let ( <= ) ~lhs ~rhs = (lhs.heap <= rhs.heap) && (lhs.logs <= rhs.logs)

    let join lhs rhs = 
        {env = (Env.join lhs.env rhs.env); heap = (Heap.join lhs.heap rhs.heap); logs = (CallLogs.join lhs.logs rhs.logs)}

    let widen ~prev ~next ~num_iters = join prev next

    let pp f = function
        {env = e; heap = h; logs = l} -> F.pp_print_string f (Printf.sprintf "{\n Env: %s \n Heap: %s \n Logs:%s\n}" (Env.pp e) (Heap.pp h) (CallLogs.pp l))
        
    let pp_summary f = function
        {env = eb; heap = hb; logs = lb}, {env = ea; heap = ha; logs = la} -> F.pp_print_string f ((Printf.sprintf "{%s; %s; %s}" (Env.pp eb) (Heap.pp hb) (CallLogs.pp lb)) ^ (Printf.sprintf "{%s; %s; %s}" (Env.pp ea) (Heap.pp ha) (CallLogs.pp la)))
end

include Domain
