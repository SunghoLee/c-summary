(*
 * Copyright (c) 2020, SW@ Laboratory at CNU.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open Core
module F = Format
module L = Logging
module Domain = SemanticSummaryDomain
open Domain
open Pervasives
open SUtils

let rec is_finally_primitive typ = 
  match typ.Typ.desc with
  | Tint ikind -> (
    match ikind with
    | IChar -> 
      false
    | ISChar -> 
      false
    | IUChar -> 
      false
    | _ -> 
      true
  )
  | Tfloat _ ->
      true
  | Tvoid _ ->
      false
  | Tfun _ ->
      false
  | Tptr (typ', _) ->
      is_finally_primitive typ'
  | Tstruct _ -> 
      false
  | TVar _ ->
      false
  | Tarray {elt} ->
      is_finally_primitive elt

let is_not_allowed typ = is_finally_primitive typ

let is_allowed typ = not (is_not_allowed typ)

let lift_typ typ = Typ.mk (Tptr (typ, Pk_pointer))
