(* module JavaGeneratorModels
 * *)

open SemanticSummaryDomain

module Y = JoustSyntax
module F = Format

module ModelHelper = struct
  (* make string `s` into java-compatible name *)
  let encode_name s =
    let buf = Buffer.create (String.length s) in
    for i = 0 to String.length s - 1 do
      Buffer.add_char buf @@ match String.get s i with
        | ':' -> '$'
        | x -> x
    done;
    Buffer.contents buf

  (* convert Var.t into string *)
  let string_of_var Var.({name; proc; kind}) =
    let proc' = match proc with
      | Var.GB -> ""
      | Var.Proc f -> f in
    let kind' = match kind with
      | Var.Temp -> "T"
      | Var.Local -> "L"
      | Var.Global -> "G" in
    kind' ^ proc' ^ "$" ^ name

  (* destruct Loc.t and make flat string *)
  let rec simple_destruct_loc' =
    function
    | Loc.Explicit v -> "ex$$" ^ string_of_var v
    | Loc.Implicit s -> "im$$" ^ encode_name s
    | Loc.Const v ->
      (match v with
         | Loc.Integer i -> string_of_int i
         | Loc.String s -> "\"" ^ String.escaped s ^ "\"")
    | Loc.Pointer p -> "ptr$$" ^ simple_destruct_loc' p
    | Loc.FunPointer p -> "fp$$" ^ InferIR.Typ.Procname.to_string p
    | Loc.Offset (u, v) ->
      "off$$" ^ simple_destruct_loc' u ^ "$" ^ simple_destruct_loc' v
    | Loc.Ret s -> "ret$$" ^ s
  (* destruct Val.t and make flat string *)
  and simple_destruct_val' v =
    match Val.elements v with
    | [(l, c)] -> simple_destruct_loc' l
    | _ -> "unknown$$"

  (* destruct Loc.t and make Y.expr *)
  let simple_destruct_loc l = match l with
    | Loc.Const _ -> Y.Literal (simple_destruct_loc' l)
    | _ -> Y.Name [Y.ident (simple_destruct_loc' l) 0]
  (* destruct Val.t and make Y.expr *)
  let simple_destruct_val v = Y.Literal (simple_destruct_val' v)

  (* find return type of jni functions (by name) *)
  let get_jni_ret_type name =
    let simple s = Some (Y.(TypeName [ident s 0])) in
    match name with
    | "FindClass" -> simple "Class"
    | "GetSuperClass" -> simple "Class"
    | "GetObjectClass" -> simple "Class"

    | "GetMethodID" -> simple "Method"
    | "CallObjectMethod" -> simple "Object"
    | "CallIntMethod" -> simple "int"
    | "CallVoidMethod" -> None

    | "GetFieldID" -> simple "Field"
    | "GetObjectField" -> simple "Object"
    | "GetIntField" -> simple "int"
    | "SetObjectField" -> None
    | "SetIntField" -> None

    | "GetStaticMethodID" -> simple "Method"
    | "CallStaticObjectMethod" -> simple "Object"
    | "CallStaticIntMethod" -> simple "int"
    | "CallStaticVoidMethod" -> None

    | "GetStaticFieldID" -> simple "Field"
    | "GetStaticObjectField" -> simple "Object"
    | "GetStaticIntField" -> simple "Int"
    | "SetStaticObjectField" -> None
    | "SetStaticIntField" -> None

    | "GetStringLength" -> simple "int"

    | "NewStringUTF" -> simple "String"
    | "GetStringUTFLength" -> simple "int"
    | "GetStringUTFChars" -> simple "char[]"
    | "ReleaseStringUTFChars" -> None

    | "GetArrayLength" -> simple "int"

    | "NewObjectArray" -> simple "Object[]"
    | "GetObjectArray" -> simple "Object"
    | "SetObjectArray" -> None

    | _ -> None

  (* parse jni class signature and make list of Y.Ident *)
  let parse_class' cls =
    List.map (fun x -> Y.ident x 0) (String.split_on_char '/' cls)
  (* parse jni class signature and make Y.typ *)
  let parse_class cls =
    let e = parse_class' cls in
    Y.TypeName e

  (* parse single jni type signature and return Y.typ * parse_end_position *)
  let rec parse_field_sig' sign =
    match String.get sign 0 with
    | 'Z' -> Y.TypeName [Y.ident "boolean" 0], 1
    | 'B' -> Y.TypeName [Y.ident "byte" 0], 1
    | 'C' -> Y.TypeName [Y.ident "char" 0], 1
    | 'D' -> Y.TypeName [Y.ident "double" 0], 1
    | 'F' -> Y.TypeName [Y.ident "float" 0], 1
    | 'I' -> Y.TypeName [Y.ident "int" 0], 1
    | 'J' -> Y.TypeName [Y.ident "long" 0], 1
    | 'V' -> Y.TypeName [Y.ident "void" 0], 1
    | 'L' ->
      let i = String.index sign ';' in
      let c = String.sub sign 1 (i - 2) in
      parse_class c, i + 1
    | '[' ->
      let n = String.length sign in
      let t, p = parse_field_sig' (String.sub sign 1 (n - 1)) in
      Y.ArrayType t, p + 1
    | _ -> failwith "Parsing Signatures Failed"
  (* parse single jni type signature and return Y.typ *)
  let parse_field_sig sign =
    fst (parse_field_sig' sign)

  (* parse method jni type signature and return args Y.typ s and ret Y.typ *)
  let parse_method_sig sign =
    let o = String.index sign '(' in
    let c = String.index sign ')' in
    let n = String.length sign in
    let rec f lst i =
      if i >= c
      then lst
      else let t, o = parse_field_sig' (String.sub sign i (n - i)) in
           f (t :: lst) (i + o) in
    let ret = parse_field_sig (String.sub sign (c + 1) (n - c - 1)) in
    f [] (o + 1), ret
end
module H = ModelHelper (* alias *)

(*--------------------------------------------------------*)
module ProcInfo = struct
  type kind =
    | C
    | Static of string * string
    | Method of string * string
    | Other
  type t = { name: string;
             kind: kind;
             ret_type: Y.typ }

  let get_name {name} = name

  let get_kind {kind} = kind

  let get_ret_type {ret_type} = ret_type

  let get_arg_name_env {kind} = match kind with
    | Static (env, _) -> env
    | Method (env, _) -> env
    | _ -> ""

  let get_arg_name_this {kind} = match kind with
    | Static (_, this) -> this
    | Method (_, this) -> this
    | _ -> ""
end

(*--------------------------------------------------------*)

module type GeneratorModel = sig
  (* name of class containing pure c functions *)
  val c_fn_class_name : string
  (* name of class containing jni api (model) functions *)
  val jni_class_name : string

  (* function to parse a procedure  *)
  val method_body : ProcInfo.t -> (* procedure information *)
                    Heap.t -> (* heap *)
                    LogUnit.t list -> (* sorted log list *)
                    Y.stmt list (* generated AST *)
end

(*--------------------------------------------------------
 * SimpleModel
 * - Generate ONLY JNI-like function calls and return
 * - Variables are created for some JNI call with return values
 * - If it cannot specify arguments, it'll use TOP *)
module SimpleModel : GeneratorModel = struct
  let c_fn_class_name = "__C"
  let jni_class_name = "__JNI"
  let top_name = "TOP"

  (* return of jni function *)
  type java_val =
  | JClass of Y.typ
           (* cls *)
  | JMethod of Y.typ option * Y.name * Y.typ list * Y.typ
            (* cls          * name   * args type  * ret type *)
  | JField of Y.typ option * Y.ident * Y.typ
           (* cls          * name    * type *)
  let top = Y.(Call (Y.Name [ident jni_class_name 0;
                             ident top_name 0], []))

  (* make assignment stmt *)
  let mk_assign typ name init_val =
    match typ with
    | None -> Y.Expr init_val
    | Some typ ->
      Y.LocalVar (Y.({f_var = {v_mods = [];
                               v_type = typ;
                               v_name = Y.ident name 0};
                      f_init = Some (ExprInit init_val)}))

  (* get string constant from heap *)
  let rec get_string_from_heap loc heap =
    match loc with
    | Loc.Const (Loc.String s) -> Some s
    | _ -> match Heap.find_opt loc heap with
      | None -> None
      | Some x -> match Val.elements x with
        | [] -> None
        | (l, c) :: xs -> get_string_from_heap l heap

  (* get inner Loc.t from ptr#IM#*:*:arg* *)
  let unpack_arg heap loc = match loc with
    | Loc.Implicit t -> (match Heap.find_opt loc heap with
      | None -> loc
      | Some x -> match Val.elements x with
        | [] -> loc
        | (l, c) :: xs -> l)
    | _ -> loc

  let destruct_loc proc rets =
    function
      | Loc.Pointer (Loc.Explicit Loc.{name; proc = Proc p})
       when p = ProcInfo.get_name proc ->
        if name = ProcInfo.get_arg_name_this proc
        then Y.Name [Y.ident "this" 0]
        else Y.Name [Y.ident name 0]
      | x -> match H.simple_destruct_loc x with
        | Y.Name [id] when not (List.mem (Y.id_string id) rets) -> top
        | y -> y

  (* check given `s` is jni function name. if so, it'll return (jni_cls, fn)
   * otherwise, return (c_cls, fn) *)
  let jni_fn_name s =
    let l = String.length s in
    if l > 8 && String.sub s 0 8 = "_JNIEnv_"
    then jni_class_name, String.sub s 8 (l - 8)
    else c_fn_class_name, s

  (* function to process each log *)
  let method_body_sub proc (lst, rets) log =
    let JF jf_name = LogUnit.get_jfun log in
    let fn1, fn2 = jni_fn_name jf_name in
    let fn = Y.Name [Y.ident fn1 0; Y.ident fn2 0] in
    let heap = LogUnit.get_heap log in
    let args = List.map (unpack_arg heap) (LogUnit.get_args log) in
    let args' = args
      |> List.tl
      |> List.map (destruct_loc proc rets) in
    let ret = LogUnit.get_rloc log
      |> H.simple_destruct_loc' in
    let e = Y.Call (fn, args') in
    let s = mk_assign (H.get_jni_ret_type fn2) ret e in
    s :: lst, ret :: rets

  (* function to generate return stmt *)
  let method_body_ret proc rets heap =
    let f loc v y = match loc with
      | Loc.Ret x -> (match Val.elements v with
        | (l, c) :: _ -> Some l
        | _ -> failwith "return is wrong")
      | _ -> y in
    match Heap.fold f heap None with
    | None -> None
    | Some v ->
      let v' = destruct_loc proc rets v in
      let v'' = Y.Cast (ProcInfo.get_ret_type proc, v') in
      Some (Y.Return (Some v''))

  (* API *)
  let method_body proc heap logs =
    let b, rets = List.fold_left (method_body_sub proc) ([], []) logs in
    let b' = match method_body_ret proc rets heap with
             | None -> b
             | Some x -> x :: b in
    List.rev b'
end



(*--------------------------------------------------------*)
module OldSimpleModel : GeneratorModel = struct
  let c_fn_class_name = "__C"
  let jni_class_name = "__JNI"

  (* return of jni function *)
  type java_val =
  | JClass of Y.typ
           (* cls *)
  | JMethod of Y.typ option * Y.name * Y.typ list * Y.typ
            (* cls          * name   * args type  * ret type *)
  | JField of Y.typ option * Y.ident * Y.typ
           (* cls          * name    * type *)

  (* make assignment stmt *)
  let mk_assign typ name init_val =
    match typ with
    | None -> Y.Expr init_val
    | Some typ ->
      Y.LocalVar (Y.({f_var = {v_mods = [];
                               v_type = typ;
                               v_name = Y.ident name 0};
                      f_init = Some (ExprInit init_val)}))

  (* get string constant from heap *)
  let rec get_string_from_heap loc heap =
    match loc with
    | Loc.Const (Loc.String s) -> Some s
    | _ -> match Heap.find_opt loc heap with
      | None -> None
      | Some x -> match Val.elements x with
        | [] -> None
        | (l, c) :: xs -> get_string_from_heap l heap

  (* get inner Loc.t from ptr#IM#*:*:arg* *)
  let unpack_arg heap loc = match loc with
    | Loc.Implicit t -> (match Heap.find_opt loc heap with
      | None -> loc
      | Some x -> match Val.elements x with
        | [] -> loc
        | (l, c) :: xs -> l)
    | _ -> loc

  let destruct_loc proc =
    function
      | Loc.Pointer (Loc.Explicit Loc.{name; proc = Proc p})
       when p = ProcInfo.get_name proc ->
        if name = ProcInfo.get_arg_name_this proc
        then Y.Literal "this"
        else Y.Literal name
      | x -> H.simple_destruct_loc x

  (*  *)
  let update_stk proc stk heap rloc fn args =
    match fn, args with
    | "FindClass", [env; cls] ->
      (match get_string_from_heap cls heap with
      | Some cls_name ->
        let x = rloc, JClass (H.parse_class cls_name) in
        None, x :: stk
      | None -> None, stk)
    | "GetFieldID", [env; cls; name; sign] ->
      let name' = get_string_from_heap name heap in
      let sign' = get_string_from_heap sign heap in
      (match name', sign' with
      | Some n, Some s ->
        let l_c = H.simple_destruct_loc' cls in
        let cls' = match List.assoc_opt l_c stk with
          | Some (JClass t) -> Some t
          | _ -> None in
        let typ = H.parse_field_sig s in
        let x = rloc, JField (cls', Y.ident n 0, typ) in
        None, x :: stk
      | _ -> None, stk )
    | "GetIntField", [env; obj; fld] ->
      (match List.assoc_opt (H.simple_destruct_loc' fld) stk with
      | Some (JField (Some cls, name, typ)) ->
        let obj' = destruct_loc proc obj in
        let e = Y.Cast (typ, Y.Dot (Y.Cast (cls, obj'), name)) in
        let stmt = mk_assign (Some typ) rloc e in
        Some stmt, stk
      | _ -> None, stk )
    | "SetIntField", [env; obj; fld; v] ->
      (match List.assoc_opt (H.simple_destruct_loc' fld) stk with
      | Some (JField (Some cls, name, typ)) ->
        let obj' = destruct_loc proc obj in
        let e = Y.Dot (Y.Cast (cls, obj'), name) in
        let v' = Y.Cast (typ, destruct_loc proc v) in
        let stmt = Y.Expr (Y.Assignment (e, "=", v')) in
        Some stmt, stk
      | _ -> None, stk )
    | _ -> None, stk

  (* check given `s` is jni function name. if so, it'll return (jni_cls, fn)
   * otherwise, return (c_cls, fn) *)
  let jni_fn_name s =
    let l = String.length s in
    if l > 8 && String.sub s 0 8 = "_JNIEnv_"
    then jni_class_name, String.sub s 8 (l - 8)
    else c_fn_class_name, s

  (* function to process each log *)
  let method_body_sub proc (lst, stk) log =
    let JF jf_name = LogUnit.get_jfun log in
    let fn1, fn2 = jni_fn_name jf_name in
    let fn = Y.Name [Y.ident fn1 0; Y.ident fn2 0] in
    let heap = LogUnit.get_heap log in
    let args = List.map (unpack_arg heap) (LogUnit.get_args log) in
    let args' = args
      |> List.tl
      |> List.map (destruct_loc proc) in
    let ret = LogUnit.get_rloc log
      |> H.simple_destruct_loc' in
    let e = Y.Call (fn, args') in
    let s = mk_assign (H.get_jni_ret_type fn2) ret e in
    let s', stk' = update_stk proc stk heap ret fn2 args in
    let lst' = match s' with
      | None -> s :: lst
      | Some x -> x :: s :: lst in
    lst', stk'

  (* function to generate return stmt *)
  let method_body_ret proc rets heap =
    let f loc v y = match loc with
      | Loc.Ret x -> Some v
      | _ -> y in
    match Heap.fold f heap None with
    | None -> None
    | Some v -> Some (Y.Return (Some (H.simple_destruct_val v)))

  (* API *)
  let method_body proc heap logs =
    let b, rets = List.fold_left (method_body_sub proc) ([], []) logs in
    let b' = match method_body_ret proc rets heap with
             | None -> b
             | Some x -> x :: b in
    List.rev b'
end