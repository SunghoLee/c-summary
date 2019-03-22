open SemanticSummaryDomain
open Pervasives

let unop_sem_fun : Unop.t -> (IInt.t -> IInt.t) =
  fun op ->
    match op with
    | Neg ->
        IInt.( * ) (IInt.of_int (-1))
    | BNot ->
        (* does not support *)
        (fun x -> IInt.top)
    | LNot ->
        (* does not supprot *)
        (fun x -> IInt.top)

let binop_sem_fun : Binop.t -> (IInt.t -> IInt.t -> IInt.t) =
  fun op ->
    match op with
    | PlusPI | MinusPI | MinusPP ->
        failwith "Does not support arithmetic operations for pointer."
    | PlusA opt ->
        IInt.(+)
    | MinusA opt ->
        IInt.(-)
    | Mult opt (** * *) ->
        IInt.( * )
    | Div (** / *) ->
        IInt.(/)
    | Mod (** % *) ->
        IInt.(%)
    | Shiftlt (** shift left *) -> 
        IInt.(<<)
    | Shiftrt (** shift right *) ->
        IInt.(>>)
    | Lt (** <  (arithmetic comparison) *) ->
        IInt.(<)
    | Gt (** >  (arithmetic comparison) *) ->
        IInt.(>)
    | Le (** <= (arithmetic comparison) *) ->
        IInt.(<=)
    | Ge (** >= (arithmetic comparison) *) ->
        IInt.(>=)
    | Eq (** == (arithmetic comparison) *) ->
        IInt.(=)
    | Ne (** != (arithmetic comparison) *) ->
        IInt.(!=)
    | BAnd (** bitwise and *) ->
        IInt.(&)
    | BXor (** exclusive-or *) ->
        IInt.b_xor
    | BOr (** inclusive-or *) ->
        IInt.b_or
    | LAnd (** logical and. Does not always evaluate both operands. *) ->
        IInt.l_and
    | LOr (** logical or. Does not always evaluate both operands. *) ->
        IInt.l_or

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



