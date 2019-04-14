open SemanticSummaryDomain

exception NullFile of string

type t = EX of string | IM of string | GB of string | FN of string | OFFSET of t * t | POINTER of t | RET | CONST_STRING of string | CONST_INT of int 

let rec pp fmt = function
  | EX s -> 
      Format.fprintf fmt "EX#%s" s
  | IM s -> 
      Format.fprintf fmt "IM#%s" s
  | GB s -> 
      Format.fprintf fmt "GB#%s" s
  | FN s -> 
      Format.fprintf fmt "FN#%s" s
  | OFFSET (b, i) -> 
      Format.fprintf fmt "%a@%a" pp b pp i
  | POINTER b ->
      Format.fprintf fmt "*(%a)" pp b
  | RET ->
      Format.fprintf fmt "RET" 
  | CONST_STRING s ->
      Format.fprintf fmt "CONST#%s" s 
  | CONST_INT s ->
      Format.fprintf fmt "CONST#%d" s 

let mk_offset l i = OFFSET (l, i)
let mk_ex s = EX s
let mk_im s = IM s
let mk_gb s = GB s
let mk_fn s = FN s
let mk_const_of_int i = CONST_INT i
let mk_const_of_string s = CONST_STRING s
let mk_pointer l = POINTER l
let mk_ret () = RET

let rec to_domain ?file ?proc ?ln yloc = 
  let scope = 
    match proc with
    | Some s ->
        Var.mk_scope s
    | None ->
        Var.glob_scope
  in
  match yloc with
  | EX s -> 
      Var.of_string ~proc:scope s
      |> Loc.mk_explicit 
  | GB s -> (
      match file with
      | Some f ->
          Var.of_string ("#GB_" ^ f ^ "_" ^ s)
          |> Loc.mk_explicit 
      | None ->
          raise (NullFile "No specific file..."))
  | FN s -> 
      InferIR.Typ.Procname.from_string_c_fun s 
      |> Loc.mk_fun_pointer 
  | IM s -> (
      match ln with
      | Some l ->
          if Caml.String.contains s ':' then
            Loc.mk_implicit s
          else
            Loc.mk_implicit (l ^ ":" ^ s)
      | None ->
          Loc.mk_implicit s)
  | OFFSET (base, index) ->
      let base' = to_domain ?file ?proc ?ln base in
      let index' = to_domain ?file ?proc ?ln index in
      Loc.mk_offset base' index'
  | POINTER base ->
      let base' = to_domain ?file ?proc ?ln base in
      Loc.mk_pointer base'
  | CONST_INT i ->
      Loc.mk_const_of_int i
  | CONST_STRING s ->
      Loc.mk_const_of_string s
  | RET ->
      (match proc with
      | Some s ->
          Loc.mk_ret s
      | None ->
          failwith "Cannot make ret locations for empty functions.")
