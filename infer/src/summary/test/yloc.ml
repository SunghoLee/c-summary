open SemanticSummaryDomain

type t = CONST of string | DYN of string | OFFSET of t * index | POINTER of t | RET
and index = INDEX of int | FIELD of string

let mk_offset l i = OFFSET (l, i)
let mk_index_of_int i = INDEX i
let mk_index_of_string s = FIELD s
let mk_const s = CONST s
let mk_dyn s = DYN s
let mk_pointer l = POINTER l
let mk_ret () = RET

let res = ref RET

let get_result () = !res
let set_result l = res := l

let to_domain_index index =
  match index with
  | INDEX i ->
      Loc.mk_index_of_int i
  | FIELD f ->
      Loc.mk_index_of_string f

let rec to_domain ?proc yloc = 
  let scope = 
    match proc with
    | Some s ->
        Var.mk_scope s
    | None ->
        Var.glob_scope
  in
  match yloc with
  | CONST s -> 
      Var.of_string ~proc:scope s
      |> Loc.mk_const
  | DYN s ->
      Loc.mk_dyn s
  | OFFSET (base, index) ->
      let base' = to_domain ?proc base in
      let index' = to_domain_index index in
      Loc.mk_offset base' index'
  | POINTER base ->
      let base' = to_domain ?proc base in
      Loc.mk_pointer base'
  | RET ->
      (match proc with
      | Some s ->
          Loc.mk_ret s
      | None ->
          failwith "Cannot make ret locations for empty functions.")


