open SemanticSummaryDomain
open Pervasives

let unop_sem_fun : Unop.t -> (Int.t -> Int.t) =
  fun op ->
    match op with
    | Neg ->
        Int.( * ) (Int.of_int (-1))
    | BNot ->
        (* does not support *)
        (fun x -> Int.top)
    | LNot ->
        (* does not supprot *)
        (fun x -> Int.top)

let binop_sem_fun : Binop.t -> (Int.t -> Int.t -> Int.t) =
  fun op ->
    match op with
    | PlusPI | MinusPI | MinusPP ->
        failwith "Does not support arithmetic operations for pointer."
    | PlusA opt ->
        Int.(+)
    | MinusA opt ->
        Int.(-)
    | Mult opt (** * *) ->
        Int.( * )
    | Div (** / *) ->
        Int.(/)
    | Mod (** % *) ->
        Int.(%)
    | Shiftlt (** shift left *) -> 
        Int.(<<)
    | Shiftrt (** shift right *) ->
        Int.(>>)
    | Lt (** <  (arithmetic comparison) *) ->
        Int.(<)
    | Gt (** >  (arithmetic comparison) *) ->
        Int.(>)
    | Le (** <= (arithmetic comparison) *) ->
        Int.(<=)
    | Ge (** >= (arithmetic comparison) *) ->
        Int.(>=)
    | Eq (** == (arithmetic comparison) *) ->
        Int.(=)
    | Ne (** != (arithmetic comparison) *) ->
        Int.(!=)
    | BAnd (** bitwise and *) ->
        Int.(&)
    | BXor (** exclusive-or *) ->
        Int.b_xor
    | BOr (** inclusive-or *) ->
        Int.b_or
    | LAnd (** logical and. Does not always evaluate both operands. *) ->
        Int.l_and
    | LOr (** logical or. Does not always evaluate both operands. *) ->
        Int.l_or

let binop op lhs rhs =
  let iter_lhs : Val.t * Cst.t -> AVS.t -> AVS.t =
    fun (lhs_v, lhs_cst) avs -> 
      let iter_rhs : Val.t * Cst.t -> AVS.t -> AVS.t = 
        fun (rhs_v, rhs_cst) avs ->
          let cst = Cst.cst_and lhs_cst rhs_cst in
          match lhs_v, rhs_v with
          | Int lhs_vi, Int rhs_vi ->
              let sem = binop_sem_fun op in
              let res = sem lhs_vi rhs_vi in
              AVS.add (Val.of_int res, cst) avs
          | Loc _, _ | _, Loc _ ->
              AVS.add (Val.of_loc Loc.top, cst) avs
          | Top, _ | _, Top ->
              AVS.add (Val.top, cst) avs 
          | _, _ ->
              failwith "Not possible operations."
      in
      AVS.fold iter_rhs rhs avs
  in
  AVS.fold iter_lhs lhs AVS.empty

let unop op e_avs =
  let iter_avs : Val.t * Cst.t -> AVS.t -> AVS.t =
    fun (e_value, cst) avs ->
      match e_value with
      | Int vi ->
          let sem = unop_sem_fun op in
          let res = sem vi in
          AVS.add (Val.of_int res, cst) avs
      | Loc _ ->
          AVS.add (Val.of_loc Loc.top, cst) avs
      | Top ->
          AVS.add (Val.top, cst) avs
      | _ ->
          failwith "Not possible operations."
  in
  AVS.fold iter_avs e_avs AVS.empty



