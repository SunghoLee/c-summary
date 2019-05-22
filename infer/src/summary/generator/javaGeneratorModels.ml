(* module JavaGeneratorModels
 * *)
 
open SemanticSummaryDomain

module Y = JoustSyntax
module F = Format

module ModelHelper = struct
  (* make string `s` into java-compatible name *)
  let encode_name s =
    let buf = Buffer.create (2 * String.length s) in
    for i = 0 to String.length s - 1 do
      match String.get s i with
        | ':' -> Buffer.add_char buf '$'
        | '$' -> Buffer.add_string buf "_$"
        | x -> Buffer.add_char buf x
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


  let string_is_enclosed_in prefix suffix str =
    let pl = String.length prefix in
    let sl = String.length suffix in
    let l = String.length str in
    if l < pl + sl then None
    else
      let rec eq_l i =
        if i >= pl then true
        else if prefix.[i] != str.[i] then false
        else eq_l (i + 1) in
      let rec eq_r i =
        if i >= sl then true
        else if suffix.[sl - i - 1] != str.[l - i - 1] then false
        else eq_r (i + 1) in
      if eq_l 0 && eq_r 0
      then Some (String.sub str pl (l - sl - pl))
      else None

  let jni_primitive_type_alist =
    [ "Boolean", "boolean";
      "Byte", "byte";
      "Char", "char";
      "Short", "short";
      "Int", "int";
      "Long", "long";
      "Float", "float";
      "Double", "double" ]

  let jni_name_pt_list =
    [ "Get", "Field";
      "Call", "Method";
      "GetStatic", "Field";
      "CallStatic", "Method" ]

  let jni_name_pt_array_list = [
    "New", "Array" ]

  let get_jni_pt_ret_type' (lst : (string * string) list) suffix name =
    let rec f x = match x with
      | [] -> None
      | (p, s) :: xs -> match string_is_enclosed_in p s name with
        | None -> f xs
        | Some t -> match List.assoc_opt t jni_primitive_type_alist with
          | None -> f xs
          | Some t -> Some (t ^ suffix) in
    f lst

  let get_jni_pt_ret_type name =
    get_jni_pt_ret_type' jni_name_pt_list "" name

  let get_jni_pt_array_ret_type name =
    get_jni_pt_ret_type' jni_name_pt_array_list "[]" name

  let jni_name_type_alist =
    [ "GetVersion", "int";

      "DefineClass", "__Class";
      "FindClass", "__Class";
      "GetSuperClass", "__Class";
      "IsAssignableFrom", "boolean";

      "Throw", "int";
      "ThrowNew", "int";
      "ExceptionOccurred", "Throwable";

      "NewGlobalRef", "Object";
      "EnsureLocalCapacity", "int";
      "PushLocalFrame", "int";
      "PopLocalFrame", "Object";
      "NewLocalRef", "Object";

      "AllocObject", "Object";
      "NewObject", "Object";
      "NewObjectA", "Object";
      "NewObjectV", "Object";
      "GetObjectClass", "__Class";
      "IsInstanceOf", "boolean";
      "IsSameObject", "boolean";

      "GetFieldID", "__FieldID";
      "GetObjectField", "Object";

      "GetMethodId", "__MethodID";
      "CallObjectMethod", "Object";

      "GetStaticFieldID", "__FieldID";
      "GetStaticObjectField", "Object";
      
      "GetStaticMethodID", "__MethodID";
      "CallStaticObjectMethod", "Object";

      "NewString", "String";
      "GetStringLength", "int";
      "GetStringChars", "__Pointer";
      "NewStringUTF", "String";
      "GetStringUTFLength", "int";
      "GetStringUTFChars", "__Pointer";

      "GetArrayLength", "int";
      "NewObjectArray",  "Object[]";
      "GetObjectArrayElement", "Object";
      "GetPrimitiveArrayCritical", "__Pointer";

      "MonitorEnter", "int";
      "MonitorExit", "int";

      "NewDirectByteBuffer", "Object";
      "GetDirectBufferAddress", "__Pointer";
      "GetDirectBufferCapacity", "long";

      "FromReflectedMethod", "__MethodID";
      "ToReflectedMethod", "java.lang.reflect.Method";
      "FromReflectedMethod", "__FieldID";
      "ToReflectedField", "java.lang.reflect.Field";

      "GetJavaVM", "int" ]

  let get_jni_ret_type__procs =
    [ (fun x -> List.assoc_opt x jni_name_type_alist);
      get_jni_pt_ret_type;
      get_jni_pt_array_ret_type ]

  (* find return type of jni functions (by name) *)
  let get_jni_ret_type name =
    let rec reduce lst arg = match lst with
      | [] -> None
      | b :: bs -> match b arg with
        | None -> reduce bs arg
        | Some v -> Some v in
    match reduce get_jni_ret_type__procs name with
    | None -> None
    | Some s -> Some Y.(TypeName [ident s 0])

  (* parse jni class signature and make list of Y.Ident *)
  let parse_class' cls =
    List.map (fun x -> Y.ident x 0) (String.split_on_char '/' cls)
  (* parse jni class signature and make Y.typ *)
  let parse_class cls =
    let e = parse_class' cls in
    Y.TypeName e

  exception ParseSignatureError

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
    | 'L' -> (match String.index_opt sign ';' with
      | None -> raise ParseSignatureError
      | Some i ->
        let c = String.sub sign 1 (i - 2) in
        parse_class c, i + 1)
    | '[' ->
      let n = String.length sign in
      let t, p = parse_field_sig' (String.sub sign 1 (n - 1)) in
      Y.ArrayType t, p + 1
    | _ -> raise ParseSignatureError
  (* parse single jni type signature and return Y.typ *)
  let parse_field_sig sign =
    let n, p = parse_field_sig' sign in
    if p < String.length sign
    then raise ParseSignatureError
    else n

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

  let rec get_const_from_heap loc heap =
    match loc with
    | Loc.Const c -> Some c
    | _ -> match Heap.find_opt loc heap with
      | None -> None
      | Some x -> match Val.elements x with
        | [] -> None
        | (l, c) :: xs -> get_const_from_heap l heap

  let get_string_from_heap loc heap =
    match get_const_from_heap loc heap with
    | Some (Loc.String s) -> Some s
    | _ -> None

  let get_int_from_heap loc heap =
    match get_const_from_heap loc heap with
    | Some (Loc.Integer i) -> Some i
    | _ -> None

  (* get inner Loc.t from ptr#IM#*:*:arg* *)
  let unpack_arg heap loc = match loc with
    | Loc.Implicit t -> (match Heap.find_opt loc heap with
      | None -> loc
      | Some x -> match Val.elements x with
        | [] -> loc
        | (l, c) :: xs -> l)
    | _ -> loc
end
module H = ModelHelper (* alias *)

(*--------------------------------------------------------*)
module State = struct
  type parsed_name = (string list *
                      string *
                      string *
                      string option)
  type t = { mutable registered : (string * parsed_name) list }

  let mk_empty () =
    { registered = [] }

  let get_registered {registered} = registered

  let add_registered state c j =
    state.registered <- (c, j) :: state.registered

  let get_native state name = List.assoc_opt name state.registered

  let fold_name_of state name init cb =
    let rec f l v = match l with
      | [] -> v
      | (c, j) :: xs when c = name -> cb j v |> f xs
      | _ :: xs -> f xs v in
    f state.registered init
end

(*--------------------------------------------------------*)
module ProcInfo = struct
  type kind =
    | C
    | Static of string * string
    | Method of string * string
    | Other
  type t = { name: string;
             kind: kind;
             ret_type: Y.typ;
             is_entry: bool }

  let get_name {name} = name

  let get_kind {kind} = kind

  let get_ret_type {ret_type} = ret_type

  let is_entry {is_entry} = is_entry

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
  (* package name of (JNI) model *)
  val model_pkg_name : string
  (* name of class containing pure c functions *)
  val c_fn_pkg_name : string
  (* name of class containing pure c functions *)
  val c_fn_class_name : string
  (* name of class containing jni api (model) functions *)
  val jni_class_name : string

  (* names of possible entries in c *)
  val possible_entries : string list

  (* function to parse a procedure  *)
  val method_body : State.t ->
                    ProcInfo.t -> (* procedure information *)
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
  let model_pkg_name = "__Model"
  let c_fn_pkg_name = "__C"
  let c_fn_class_name = "Fn"
  let jni_class_name = "__JNI"
  let top_name = "TOP"

  let possible_entries = ["main"; "android_main"; "JNI_OnLoad"]

  (* return of jni function *)
  type java_val =
  | JUnknown
  | JClass of Y.typ
           (* cls *)
  | JMethod of Y.typ option * Y.ident * Y.typ list * Y.typ
            (* cls          * name   * args type  * ret type *)
  | JField of Y.typ option * Y.ident * Y.typ
           (* cls          * name    * type *)
  type stack = (string * java_val) list
  let top = Y.(Call (Y.Name [ident jni_class_name 0;
                             ident top_name 0], []))

  (* make assignment stmt *)
  let mk_assign typ name init_val =
    match typ with
    | None -> Y.Expr init_val
    | Some typ ->
      let f_var = Y.({v_mods = [];
                      v_type = typ;
                      v_name = Y.ident name 0}) in
      Y.LocalVar (Y.({f_var = f_var;
                      f_init = Some (ExprInit init_val)}))

  let destruct_loc proc stk =
    function
      | Loc.Pointer (Loc.Explicit Loc.{name; proc = Proc p})
          when p = ProcInfo.get_name proc ->
        let id = if name = ProcInfo.get_arg_name_this proc
          then "this" else name in
        Y.Name [Y.ident id 0]
      | x -> match H.simple_destruct_loc x with
        | Y.Name [id] when not (List.mem_assoc (Y.id_string id) stk) -> top
        | y -> y

  let handle_register_natives state heap stk cls mths n =
    let l_cls = H.simple_destruct_loc' cls in
    match List.assoc_opt l_cls stk with
    | Some (JClass (Y.TypeName l)) ->
      (match l |> List.map Y.id_string |> List.rev with
      | [] -> ()
      | cls' :: pkg_r ->
        let pkg = List.rev pkg_r in
        let rec g cond i =
          if not (cond i) then ()
          else let l_arr = Loc.Offset (mths, Loc.Const (Loc.Integer i)) in
               let l_ptr = Loc.Pointer l_arr in
               let off s = Loc.Offset (l_ptr, Loc.Const (Loc.String s)) in
               let find field =
                 match Heap.find_opt (off field) heap with
                 | None -> None
                 | Some x -> match Val.elements x with
                   | [l, c] -> Some l
                   | _ -> None in
               match find "fnPtr", find "signature", find "name" with
               | Some (Loc.FunPointer fn_ptr),
                 Some (Loc.Const (Loc.String sign)),
                 Some (Loc.Const (Loc.String name)) ->
                  State.add_registered state
                     (InferIR.Typ.Procname.to_string fn_ptr)
                     (pkg, cls', name, Some sign);
                 g cond (i + 1)
               | _ -> () in
        match H.get_int_from_heap n heap with 
        | None -> g (fun _ -> true) 0
        | Some n' -> g (fun x -> x < n') 0)
    | _ -> ()

  (* update_stk: push class/method/field information into stack *)
  let update_stk state proc stk heap rloc fn args =
    match fn, args with
    | "FindClass", [env; cls] ->
      (match H.get_string_from_heap cls heap with
      | Some cls_name ->
        let x = rloc, JClass (H.parse_class cls_name) in
        x :: stk
      | None -> (rloc, JUnknown) :: stk)
    | "GetMethodID", [env; cls; name; sign] ->
      let name' = H.get_string_from_heap name heap in
      let sign' = H.get_string_from_heap sign heap in
      (match name', sign' with
      | Some n, Some s ->
        let l_c = H.simple_destruct_loc' cls in
        let cls' = match List.assoc_opt l_c stk with
          | Some (JClass t) -> Some t
          | _ -> None in
        let args, ret = H.parse_method_sig s in
        let x = rloc, JMethod (cls', Y.ident n 0, args, ret) in
        x :: stk
      | _ -> (rloc, JUnknown) :: stk )
    | "GetFieldID", [env; cls; name; sign] ->
      let name' = H.get_string_from_heap name heap in
      let sign' = H.get_string_from_heap sign heap in
      (match name', sign' with
      | Some n, Some s ->
        let l_c = H.simple_destruct_loc' cls in
        let cls' = match List.assoc_opt l_c stk with
          | Some (JClass t) -> Some t
          | _ -> None in
        let typ = H.parse_field_sig s in
        let x = rloc, JField (cls', Y.ident n 0, typ) in
        x :: stk
      | _ -> (rloc, JUnknown) :: stk )
    | "RegisterNatives", [env; cls; mths; n] when ProcInfo.is_entry proc ->
      handle_register_natives state heap stk cls mths n;
      stk
    | _ -> (rloc, JUnknown) :: stk

  (* check given `s` is jni function name. if so, it'll return (jni_cls, fn)
   * otherwise, return (c_cls, fn) *)
  let jni_fn_name s =
    let l = String.length s in
    if l > 8 && String.sub s 0 8 = "_JNIEnv_"
    then jni_class_name, String.sub s 8 (l - 8)
    else c_fn_class_name, s

  (* function to process each log *)
  let method_body_sub state proc (lst, stk) log =
    let JF jf_name = LogUnit.get_jfun log in
    let cls, mth = jni_fn_name jf_name in
    let fn = Y.Name [Y.ident cls 0; Y.ident mth 0] in
    let heap = LogUnit.get_heap log in
    let args = List.map (H.unpack_arg heap) (LogUnit.get_args log) in
    let args' = args
      |> List.tl
      |> List.map (destruct_loc proc stk) in
    let ret = LogUnit.get_rloc log
      |> H.simple_destruct_loc' in
    let e = Y.Call (fn, args') in
    let s = mk_assign (H.get_jni_ret_type mth) ret e in
    let stk' = update_stk state proc stk heap ret mth args in
    s :: lst, stk'

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
      let ret_type = ProcInfo.get_ret_type proc in
      let v' = match ret_type with
        | Y.TypeName [n] when Y.id_string n = model_pkg_name ^ ".__Unknown" ->
          top
        | _ -> destruct_loc proc rets v in
      let v'' = Y.Cast (ret_type, v') in
      Some (Y.Return (Some v''))

  (* API *)
  let method_body state proc heap logs =
    let b, rets = List.fold_left (method_body_sub state proc) ([], []) logs in
    let b' = match method_body_ret proc rets heap with
             | None -> b
             | Some x -> x :: b in
    List.rev b'
end
