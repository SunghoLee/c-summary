(* module JavaGeneratorModels
 * *)
 
open SemanticSummaryDomain

module Y = JoustSyntax
module F = Format

module CFG = ControlFlowGraph
module CFGG = ControlFlowGraph.Graph
module CFGN = ControlFlowGraph.Node
module CFGNLoc = ControlFlowGraph.NodeLoc
module CFGH = ControlFlowGraph.GraphHelper

module GlobalVars = struct
  module SS = Set.Make(String)

  let set = ref SS.empty

  let add v = set := SS.add v (!set)
end

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

  let encode_global_name s =
    "G$$" ^ encode_name s

  let encode_global_full_name s =
    "__Model.__Global." ^ encode_global_name s

  (* convert Var.t into string *)
  let string_of_var VVar.({name; proc; kind}) =
    let proc' = match proc with
      | VVar.GB -> ""
      | VVar.Proc f -> f in
    let kind' = match kind with
      | VVar.Temp -> "T"
      | VVar.Local -> "L"
      | VVar.Global -> "G" in
    kind' ^ proc' ^ "$" ^ name

  let simple_destruct_const_typ = function
    | Loc.String s -> "\"" ^ String.escaped s ^ "\""
    | Loc.Z z -> Z.to_string z
    | Loc.ConstTop -> "__T"

  (* destruct Loc.t and make flat string *)
  let rec simple_destruct_loc' glocs loc = match loc with
    | Loc.Explicit v -> "ex$$" ^ string_of_var v
    | Loc.Implicit s ->
        if LocSet.mem loc glocs
        then (GlobalVars.add s;
              encode_global_full_name s)
        else "im$$" ^ encode_name s
    | Loc.Const v ->
        simple_destruct_const_typ v
    | Loc.Pointer (p, _, _) -> "ptr$$" ^ simple_destruct_loc' glocs p
    | Loc.FunPointer p -> "fp$$" ^ InferIR.Typ.Procname.to_string p
    | Loc.Offset (u, v) ->
      "off$$" ^ simple_destruct_loc' glocs u ^ "$" ^ simple_destruct_const_typ v
    | Loc.Ret s -> "ret$$" ^ s
    | _ -> "UNEX"
  (* destruct Val.t and make flat string *)
  and simple_destruct_val' glocs v =
    match Val.elements v with
    | [(l, c)] -> simple_destruct_loc' glocs l
    | _ -> "unknown$$"

  (* destruct Loc.t and make Y.expr *)
  let simple_destruct_loc glocs l = match l with
    | Loc.Const _ -> Y.Literal (simple_destruct_loc' glocs l)
    | _ -> Y.Name [Y.ident (simple_destruct_loc' glocs l) 0]
  (* destruct Val.t and make Y.expr *)
  let simple_destruct_val glocs v = Y.Literal (simple_destruct_val' glocs v)

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
      "CallStatic", "Method";
      "Get", "ArrayElement" ]

  let jni_name_pt_array_list = [
    "New", "Array";
    "Get", "ArrayElements"]

  let get_jni_pt_ret_type' (lst : (string * string) list) suffix name =
    let rec f = function
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
      "ExceptionCheck", "boolean";

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

      "GetMethodID", "__MethodID";
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
      "GetObjectArrayElements", "Object[]";

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
    let rec reduce arg = function
      | [] -> None
      | b :: bs -> match b arg with
        | None -> reduce arg bs
        | Some v -> Some v in
    match reduce name get_jni_ret_type__procs with
    | None -> None
    | Some s -> Some Y.(TypeName [ident s 0])

  (* parse jni class signature and make list of Y.Ident *)
  let parse_class' cls =
    List.map (fun x -> Y.ident x 0) (String.split_on_char '/' cls)
  (* parse jni class signature and make Y.typ *)
  let parse_class cls = Y.TypeName (parse_class' cls)

  exception ParseSignatureError

  (* parse single jni type signature and return Y.typ * parse_end_position *)
  let rec parse_field_sig' sign =
    match String.get sign 0 with
    | 'Z' -> Y.TypeName [Y.ident "boolean" 0], 1
    | 'B' -> Y.TypeName [Y.ident "byte" 0], 1
    | 'C' -> Y.TypeName [Y.ident "char" 0], 1
    | 'S' -> Y.TypeName [Y.ident "short" 0], 1
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
    | Some (Loc.Z i) -> Some (Z.to_int i)
    | _ -> None

  (* get inner Loc.t from ptr#IM#*:*:arg* *)
  let unpack_arg heap loc = match loc with
    | Loc.Implicit t -> (match Heap.find_opt loc heap with
      | None -> loc
      | Some x ->
        let f (l, cst) = not (Loc.is_numeric_const l) in
        match Val.filter f x |> Val.elements with
        | [] -> loc
        | (l, c) :: xs -> l)
    | _ -> loc
    
  let unpack_arg' heap loc = match loc with
    | Loc.Implicit t -> (match Heap.find_opt loc heap with
      | None -> [loc]
      | Some x ->
        let f (l, cst) = not (Loc.is_numeric_const l) in
        match Val.filter f x |> Val.elements with
        | [] -> [loc]
        | y -> List.map (fun (x, y) -> x) y)
    | _ -> [loc]

  (* box given type if possible *)
  let box_type = function
    | Y.TypeName [n] -> Y.TypeName [Y.ident (match Y.id_string n with
        | "byte" -> "Byte"
        | "boolean" -> "Boolean"
        | "char" -> "Character"
        | "short" -> "Short"
        | "int" -> "Integer"
        | "long" -> "Long"
        | "float" -> "Float"
        | "double" -> "Double"
        | x -> x) 0]
    | x -> x

  let extract_typename typ = match typ with
    | Y.TypeName [n] -> Some n
    | _ -> None

  let typename_is name typ = match typ with
    | Y.TypeName [n] when Y.id_string n = name -> true
    | _ -> false
end
module H = ModelHelper (* alias *)

(*--------------------------------------------------------*)
(* GlobalState: State containing global information. (registered functions) *)
module GlobalState = struct
  type parsed_name = (string list *
                      string *
                      string *
                      string option)
  type t = { mutable registered : (string * parsed_name) list }

  let mk_empty () = { registered = [] }

  let get_registered { registered } = registered

  let add_registered state c j =
    state.registered <- (c, j) :: state.registered

  let get_native state name = List.assoc_opt name state.registered

  let fold_name_of state name cb init =
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
             is_entry: bool;
             formals: Y.var list }

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
type java_val =
| JUnknown
| JClass of Y.typ list
         (* cls *)
| JMethod of Y.typ option * Y.ident * Y.typ list * Y.typ
          (* cls          * name   * args type  * ret type *)
| JField of Y.typ option * Y.ident * Y.typ
         (* cls          * name    * type *)


(*--------------------------------------------------------*)
(* LocalState: State for each functions *)
module LocalState = struct
  type stmts = Y.stmt list
  type stmts2 = stmts * stmts

  let stmts2_empty = [], []

  type block_kind = KIf | KWhile | KDoWhile

  type block_body =
    | BTop
    | BIf of block_t * Y.expr * stmts2 option
    | BWhile of block_t * Y.expr
    | BDoWhile of block_t * Y.expr
    | BReserved of block_t
  and block_t = block_body * stmts2

  type t = { glocs: LocSet.t;
             gheap: Heap.t;
             cfg: Loc.t CFGG.t;
             proc: ProcInfo.t;
             stack: (string * java_val) list;
             blocks: block_t;
             vars: stmts }

  (* Constructor *)
  let mk_empty glocs gheap cfg proc init_stk =
    { glocs; gheap; cfg; proc;
      stack = init_stk;
      blocks = BTop, stmts2_empty;
      vars = [] }

  (* Getter *)
  let get_glocs { glocs } = glocs
  let get_gheap { gheap } = gheap
  let get_proc { proc } = proc
  let get_stack { stack } = stack
  let get_blocks { blocks } = blocks

  (* Stack *)
  let lookup_stack name s =
    let rec f name = function
      | [] -> None
      | (k, v) :: xs when k = name -> Some v
      | _ :: xs -> f name xs in
    f name s.stack

  let mem_stack name s = match lookup_stack name s with
    | Some _ -> true
    | None -> false

  let push_stack name value s =
    { s with stack = (name, value) :: s.stack }

  (* stmts2 *)
  let stmts2_add_forward stmt (bw, fw) = bw, (stmt :: fw)
  let stmts2_add_backward stmt (bw, fw) = (stmt :: bw), fw
  let rec stmts2_flatten (bw, fw) = match fw with
    | [] -> bw
    | f :: fs -> stmts2_flatten (f :: bw, fs)

  (* Blocks *)
  let push_stmt stmt s = match s.blocks with
    | body, stmts -> { s with blocks = body, stmts2_add_forward stmt stmts }

  let push_stmt_backward stmt s = match s.blocks with
    | body, stmts -> { s with blocks = body, stmts2_add_backward stmt stmts }

  let push_stmts_backward stmts s =
    let rec f s = function
      | [] -> s
      | x :: xs -> f (push_stmt_backward x s) xs
    in f s (List.rev stmts)

  let push_block kind cond s =
    let blocks = match kind with
      | KIf -> BIf (s.blocks, cond, None)
      | KWhile -> BWhile (s.blocks, cond)
      | KDoWhile -> BDoWhile (s.blocks, cond) in
    { s with blocks = blocks, stmts2_empty }

  let push_reserved s = { s with blocks = BReserved s.blocks, stmts2_empty }

  let pop_block s = match s.blocks with
    | BTop, sts ->
        let lst = stmts2_flatten sts in
        { s with blocks = BTop, ([], lst) }
    | BIf (parent, cond, saved_s), sts ->
        (match saved_s with
             | None -> { s with blocks = BIf (parent, cond, Some sts), stmts2_empty }
             | Some if_s ->
               let if_s' = stmts2_flatten if_s in
               let else_s' = stmts2_flatten sts in
               let packed = Y.If (cond, Y.Block if_s', Some (Y.Block else_s')) in
               push_stmt packed { s with blocks = parent })
    | BWhile (parent, cond), sts ->
        let stmts' = stmts2_flatten sts in
        let packed = Y.While (cond, Y.Block stmts') in
        push_stmt packed { s with blocks = parent }
    | BDoWhile (parent, cond), sts ->
        let stmts' = stmts2_flatten sts in
        let packed = Y.Do (Y.Block stmts', cond) in
        push_stmt packed { s with blocks = parent }
    | BReserved parent, sts ->
        let stmts' = stmts2_flatten sts in
        push_stmts_backward stmts' { s with blocks = parent }

  let rec flatten_blocks s = match s.blocks with
    | BTop, stmts ->
        let s' = pop_block s in
        (match s'.blocks with
        | BTop, ([], fw) -> s', (List.rev s'.vars @ fw)
        | _ -> failwith "Impossible")
    | _ -> pop_block s |> flatten_blocks

  (* vars *)
  let add_var_stmt stmt s = { s with vars = stmt :: s.vars }
                             
  let add_var typ name s =
    let f_var = Y.{v_mods = []; v_type = typ; v_name = Y.ident name 0} in
    let obj_typ = Y.TypeName [Y.ident "Object" 0] in
    let t = match typ with
      | Y.TypeName [n] -> Y.id_string n
      | _ -> "" in
    let init = Y.Literal (match t with
                          | "boolean" -> "false"
                          | "long" -> "0L"
                          | "float" -> "0.0f"
                          | "double" -> "0.0"
                          | "byte" -> "(byte)0"
                          | "char" -> "(char)0"
                          | "short" -> "(short)0"
                          | "int" -> "0"
                          | _ -> "null") in
    let f_init = Some (Y.ExprInit init) in
    let e = Y.(LocalVar {f_var; f_init}) in
    add_var_stmt e s
end


(*--------------------------------------------------------*)
module GeneratorOptions = struct
  type t =
    { use_prune_info: bool }
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
  val method_body : GeneratorOptions.t ->
                    GlobalState.t ->
                    LocSet.t ->
                    ProcInfo.t -> (* procedure information *)
                    Heap.t -> (* heap *)
                    LogUnit.t list -> (* sorted log list *)
                    Loc.t CFGG.t -> (* control flow graph *)
                    Y.stmt list (* generated AST *)
end

(*--------------------------------------------------------
 * SimpleModel
 * - Generate ONLY JNI-like function calls and return
 * - Variables are created for some JNI call with return values
 * - If it cannot specify arguments, it'll use TOP *)
module SimpleModel : GeneratorModel = struct
  module GS = GlobalState
  module LS = LocalState

  let model_pkg_name = "__Model"
  let c_fn_pkg_name = "__C"
  let c_fn_class_name = "Fn"
  let jni_class_name = "__JNI"
  let top_name = "TOP"

  let possible_entries = ["main"; "android_main"; "JNI_OnLoad"]

  (* return of jni function *)
  let top_y_name = Y.(Name [ident jni_class_name 0; ident top_name 0])
  let top = Y.(Call (top_y_name, []))
  let top_with_comment comment =
    Y.(Call (top_y_name, [Y.Literal ("/* " ^ comment ^ " */")]))

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

  let get_this_expr proc = match ProcInfo.get_kind proc with
    | Static _ -> Y.Literal "__JNI.ThisClass()"
    | _ -> Y.Name [Y.ident "this" 0]

  let destruct_loc glocs proc stk =
    function
      | Loc.Pointer (Loc.Explicit Loc.{name; proc = Proc p}, _, _)
          when p = ProcInfo.get_name proc ->
        if name = ProcInfo.get_arg_name_this proc
          then get_this_expr proc
          else if List.mem_assoc name stk
               then Y.Name [Y.ident name 0]
               else top_with_comment "undeclared"
      | x -> match H.simple_destruct_loc glocs x with
        | Y.Name [id] when not (List.mem_assoc (Y.id_string id) stk) ->
            (* print_string ("TOP for " ^ Y.id_string id ^ "\n"); *)
            top_with_comment "undeclared"
        | y -> y

  let typed_simple_destruct_loc glocs typ = function
    | Loc.Const (Loc.Z value) ->
      let s = Z.to_string value in
      let t = match typ with
        | Y.TypeName [n] -> Y.id_string n
        | _ -> "" in
      (* print_string ("TypeName: " ^ t ^ "\n"); *)
      Y.Literal (match t with
                | "boolean" ->
                    if s = "0" then "false" else "true"
                | "long" -> s ^ "L"
                | "float" -> s ^ ".f"
                | "double" -> s ^ ".0"
                | "byte" -> "(byte)" ^ s
                | "char" -> "(char)" ^ s
                | "short" -> "(short)" ^ s
                | "int" -> s
                | _ -> jni_class_name ^ "." ^ top_name ^ "()")
    | Loc.Const (Loc.String s) ->
      Y.Literal ("\"" ^ String.escaped s ^ "\"")
    | x -> Y.Name [Y.ident (H.simple_destruct_loc' glocs x) 0]

  let typed_destruct_loc glocs proc typ stk = function
      | Loc.Pointer (Loc.Explicit Loc.{name; proc = Proc p}, _, _)
          when p = ProcInfo.get_name proc ->
        if name = ProcInfo.get_arg_name_this proc
          then get_this_expr proc
          else if List.mem_assoc name stk
               then Y.Name [Y.ident name 0]
               else top
      | x -> match typed_simple_destruct_loc glocs typ x with
        | Y.Name [id] when not (List.mem_assoc (Y.id_string id) stk) ->
            top
        | y -> y

  let typed_destruct_val glocs proc typ stk v =
    let f (l, c) res =
      (* F.printf "tdv.f : %a\n" Loc.pp l; *)
      if res = top
      then typed_destruct_loc glocs proc typ stk l
      else res
    in Val.fold f v top

  let handle_register_natives gs (LS.{glocs; gheap; stack} as ls) cls mths =
    let l_cls = H.simple_destruct_loc' glocs cls in
    match LS.lookup_stack l_cls ls with
    | Some (JClass ls) ->
      ls |> List.iter (function
        | Y.TypeName l ->
          (match l |> List.map Y.id_string |> List.rev with
          | [] -> ()
          | cls' :: pkg_r ->
            let pkg = List.rev pkg_r in
            let ptr t = Loc.Pointer (t, Loc.ConcreteLoc, false) in
            let box c t = Loc.Offset (ptr t, c) in
            let rec g cond i =
              (*F.printf "i = %d\n" i;
              F.printf "heap = %a\n" Heap.pp heap;*)
              if not (cond i) then ()
              else let l_ptr = box (Loc.Z (Z.of_int i)) mths in
                   let off s = box (Loc.String s) l_ptr in
                 (*F.printf "RegNat: %a\n" Loc.pp (off "FIELD"); *)
                   let find_from_heap field =
                     match Heap.find_opt (off field) gheap with
                     | None -> None
                     | Some x -> match Val.elements x with
                       | (l, c) :: _ -> Some l
                       | _ -> None in
                   (*let find_from_locset locs field =
                     match LocSet.find_opt (ptr (off field)) locs with
                     | None -> None
                     | Some x -> match Val.elements x with
                       | (l, c) :: _ -> Some l
                       | _ -> None in*)
                   let find field = find_from_heap field in
                     (*match find_from_heap heap field with
                     | None -> find_from_locset glocs field
                     | Some x -> Some x in*)
                   match find "fnPtr", find "signature", find "name" with
                   | Some (Loc.FunPointer fn_ptr),
                     Some (Loc.Const (Loc.String sign)),
                     Some (Loc.Const (Loc.String name)) ->
                      GS.add_registered gs
                         (InferIR.Typ.Procname.to_string fn_ptr)
                         (pkg, cls', name, Some sign);
                      g cond (i + 1)
                   | _ -> F.printf " - handled: %d\n" i in
            (*match H.get_int_from_heap n heap with 
            | None -> g (fun _ -> true) 0
            | Some n' -> g (fun x -> x < n') 0)*)
            g (fun _ -> true) 0)
        | _ -> () )
    | _ -> ()

  let init_stack glocs ProcInfo.{kind; formals} =
    let mk name = name, JUnknown in
    (match kind with
      | ProcInfo.Static (env, this) -> [mk env; mk this]
      | ProcInfo.Method (env, this) -> [mk env; mk this]
      | _ -> [])
    |> List.fold_right
      (fun e s -> match e with
         | { Y.v_name = v_name } -> mk (Y.id_string v_name) :: s)
      formals
    |> LocSet.fold
      (fun e s -> match e with
         | Loc.Implicit name -> mk (H.encode_global_full_name name) :: s
         | _ -> s) glocs

  (* update_stack: push class/method/field information into stack *)
  let update_stack gs (LS.{glocs; gheap; proc; stack} as ls) heap rloc fn args =
    match fn, args with
    | "FindClass", [env; cls] -> (
        let t = List.fold_right (fun c l ->
          (match H.get_string_from_heap c heap with
          | Some cls_name -> H.parse_class cls_name :: l
          | None -> l)) cls [] in
        match t with
        | [] -> LS.push_stack rloc JUnknown ls
        | _ -> LS.push_stack rloc (JClass t) ls )
    (*| "GetMethodID", [env; cls; name; sign] ->
      let name' = H.get_string_from_heap name heap in
      let sign' = H.get_string_from_heap sign heap in
      (match name', sign' with
      | Some n, Some s ->
        let l_c = H.simple_destruct_loc' glocs cls in
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
        let l_c = H.simple_destruct_loc' glocs cls in
        let cls' = match List.assoc_opt l_c stk with
          | Some (JClass t) -> Some t
          | _ -> None in
        let typ = H.parse_field_sig s in
        let x = rloc, JField (cls', Y.ident n 0, typ) in
        x :: stk
      | _ -> (rloc, JUnknown) :: stk )*)
    | "RegisterNatives", [env; cls; mths; n] when ProcInfo.is_entry proc ->
      F.printf "[INFO] Handle RegisterNatives\n";
      List.iter (fun c -> 
        List.iter (fun m ->
          handle_register_natives gs ls c m
        ) mths
      ) cls;
      ls
    | _ -> LS.push_stack rloc JUnknown ls

  (* check given `s` is jni function name. if so, it'll return (jni_cls, fn)
   * otherwise, return (c_cls, fn) *)
  let jni_fn_name s =
    let l = String.length s in
    if l > 8 && String.sub s 0 8 = "_JNIEnv_"
    then jni_class_name, String.sub s 8 (l - 8)
    else c_fn_class_name, s

  let mk_phi glocs proc stk args =
    let args' =
      List.map (destruct_loc glocs proc stk) args in
    Y.(Call (Y.Name [ident "__JNI.Phi" 0], args'))

  let rec mk_all_assigns ls cur cb = function
    | [] -> LS.push_stmt (cb cur) ls
    | x :: xs ->
        let f e ls = mk_all_assigns ls (e :: cur) cb xs in
        List.fold_right f x ls

  (* function to process each log *)
  let method_body_sub gs (LS.{glocs; gheap; proc; stack; blocks} as ls) log =
    let JF jf_name = LogUnit.get_jfun log in
    let cls, mth = jni_fn_name jf_name in
    let fn = Y.Name [Y.ident cls 0; Y.ident mth 0] in
    let heap = LogUnit.get_heap log in
    let args = List.map (H.unpack_arg' heap) (LogUnit.get_args log) in
    let args' = args
      |> List.tl
      |> List.map (List.map (destruct_loc glocs proc stack)) in
    let ret = LogUnit.get_rloc log
      |> H.simple_destruct_loc' glocs in
    let ls' = match H.get_jni_ret_type mth with
      | Some t when not (LS.mem_stack ret ls) ->
          LS.add_var t ret ls
      | _ -> ls in
    let mk_a = mk_assign (
      match H.get_jni_ret_type mth with
      | None -> None
      | Some _ -> (Some (Y.TypeName [Y.ident "" 0]))) in
    let cb args = mk_a ret (Y.Call (fn, args)) in
    let ls'' = mk_all_assigns ls' [] cb (List.rev args') in
    update_stack gs ls'' heap ret mth args

  (* function to generate return stmt *)
  let method_body_ret LS.{glocs; gheap; proc; stack; blocks} =
    let f loc v y = match loc with
      | Loc.Ret x -> Some v(*match Val.elements v with
        | (l, c) :: _ -> Some l
        | _ -> failwith "return is wrong"*)
      | _ -> y in
    match Heap.fold f gheap None with
    | None -> None
    | Some v ->
      let ret_type = ProcInfo.get_ret_type proc in
      let v' = if H.typename_is (model_pkg_name ^ ".__Unknown") ret_type
        then top
        else typed_destruct_val glocs proc ret_type stack v in
      let ret_type' = H.box_type ret_type in
      let v'' = Y.Cast (ret_type', v') in
      Some (Y.Return (Some v''))

  let return_top proc =
    let ret_type = proc |> ProcInfo.get_ret_type |> H.box_type in
    let v = Y.Cast (ret_type, top) in
    Y.Return (Some v)



  (*---------------------------------------------------------------
  type pt = CFGNLoc.t
  type prune_info = PIIf of pt * pt * pt option (* end_scope *)
                  | PIWhile of pt
                  | PIDoWhile of pt * pt


  (*let destruct_cond ls e =
    match Heap.find_opt e ls.LS.gheap with
    | None ->
        print_string "Not in heap\n";
        F.printf "Heap: %a\n" Heap.pp ls.LS.gheap;
        None
    | Some x ->
        print_string "E in heap\n";
        match Val.elements x with
      | [] -> None
      | (l, c) :: xs -> Some (H.simple_destruct_loc ls.LS.glocs l)*)
  let exp_to_Yexpr ls e =
    match e with
    | CFGN.EIsTrue e ->
        H.simple_destruct_loc ls.LS.glocs e
    | CFGN.EIsFalse e ->
        let dest =
          H.simple_destruct_loc ls.LS.glocs e in
        Y.Prefix ("!", dest)
    | _ -> top

  let analysis_node cfg n (ls, stk) =
    let loc = CFGN.get_loc n in
    let sort = List.sort CFGNLoc.compare_by_idx in
    let preds = CFGN.get_pred_list n |> sort |> List.rev in
    let succs = CFGN.get_succ_list n |> sort in 
    let (ls, stk) = match stk with
      | PIWhile e :: ss when CFGNLoc.compare e loc = 0 ->
          (LS.pop_block ls, ss)
      | PIDoWhile (_, e) :: ss when CFGNLoc.compare e loc = 0 ->
          (LS.pop_block ls, ss)
      | _ -> (ls, stk) in
    match preds, succs with
    | (p :: ps), [s1; s2] when CFGNLoc.compare p loc > 0 -> (* while *)
        let cond =
          match CFGH.find_next_prune cfg s1 with
          | None -> top
          | Some e -> exp_to_Yexpr ls e in
        (LS.push_block LS.KWhile cond ls, PIWhile p :: stk)
    | (p :: ps), [s] when CFGNLoc.compare p loc > 0 -> (* do-while *)
        (match CFGH.find_last_branch cfg p with
         | None -> (ls, stk)
         | Some cond ->
             let dw = PIDoWhile (cond, p) in
             let cond =
               match CFGH.find_next_prune cfg p with
               | None -> top
               | Some e -> exp_to_Yexpr ls e in
             (LS.push_block LS.KDoWhile cond ls, dw :: stk))
    | _, [s1; s2] -> (* if start *)
        (match stk with
         | PIDoWhile (cond, _end) :: ss when CFGNLoc.compare cond loc = 0 ->
             (ls, stk)
         | _ ->
             let cond =
               match CFGH.find_next_prune cfg s1 with
               | None -> top
               | Some e -> exp_to_Yexpr ls e in
             (LS.push_block LS.KIf cond ls, PIIf (s1, s2, None) :: stk)
        )
    | [p1; p2], _ -> (* if end *)
        (match stk with
         | PIIf (_, _, Some _) :: ss -> (LS.pop_block ls, ss) 
         | _ -> (ls, stk))
    | _, [s] ->
        (match stk with
         | PIIf (t, f, None) :: ss when CFGNLoc.compare s f > 0 ->
             (LS.pop_block ls, PIIf (t, f, Some s) :: ss)
         | _ -> (ls, stk))
    | _ -> (ls, stk)

  let track_cfg (LS.{cfg} as ls) = 
    let rec f (ls, stk) = function
      | [] -> (ls, stk)
      | n :: ns -> f (analysis_node cfg n (ls, stk)) ns
    in cfg.CFGG.nodes |> f (ls, []) |> fun (ls, stk) -> ls
  ---------------------------------------------------------------*)

  let bool_cast e =
    let bool_typ = Y.TypeName [Y.ident "Boolean" 0] in
    Y.(Cast (bool_typ, e))

  let top_cond =
    let t = top_with_comment "unknown condition" in
    bool_cast t

  let exp_to_Yexpr ls e =
    match e with
    | CFGN.EIsTrue e ->
        let s = H.simple_destruct_loc' ls.LS.glocs e in
        if LS.mem_stack s ls
        then Some (bool_cast (Y.Literal s))
        else None 
    | CFGN.EIsFalse e ->
        let s = H.simple_destruct_loc' ls.LS.glocs e in
        if LS.mem_stack s ls
        then Some (Y.Prefix ("!", bool_cast (Y.Literal s)))
        else None
    | _ -> None

  let condition_to_Yexpr (LS.{cfg} as ls) loc =
    match CFGH.find_next_prune cfg loc with
    | None -> None
    | Some n -> match n.CFGN.kind with
      | CFGN.KPruneT v -> exp_to_Yexpr ls v
      | CFGN.KPruneF v -> exp_to_Yexpr ls v
      | _ -> None

  type prune_info = PIIf of CFGNLoc.t * CFG.NodeLocSet.t
                  | PIIfNot of CFGNLoc.t * CFG.NodeLocSet.t
                  | PITop of CFGNLoc.t * CFG.NodeLocSet.t
                  | PITopNot of CFGNLoc.t * CFG.NodeLocSet.t
  type track_state = LS.t * prune_info list

  let analyze_node node (LS.{cfg} as ls, stk, visited) =
    let rec pop (ls, stk, vis) = match stk with
      | PIIfNot (l, v) :: ss -> pop (LS.pop_block ls, ss, vis)
      | PIIf (l, v) :: ss -> (LS.pop_block ls, PIIfNot (l, v) :: ss, v)
      | PITopNot (l, v) :: ss -> pop (ls, ss, vis)
      | PITop (l, v) :: ss -> (LS.pop_block ls, PIIfNot (l, vis) :: ss, vis)
      | _ -> (ls, stk, vis) in
    if CFG.NodeLocSet.mem node.CFGN.loc visited
    then let ls, stk, vis = pop (ls, stk, visited) in ([], ls, stk, vis)
    else
      let v' = CFG.NodeLocSet.add node.CFGN.loc visited in
      let succs = CFGN.get_succ_list node |> CFGH.sort_locs_by_idx in
      let succs = CFGH.loc_list_to_node_list succs cfg in
      match succs with
      | [] ->
          let ls, stk, v'' = pop (ls, stk, v') in
          [], ls, stk, v''
      | [x; y] ->
          (match condition_to_Yexpr ls x.CFGN.loc with
            | None ->
                succs, LS.push_reserved ls,
                PITop (node.CFGN.loc, v') :: stk, v'
            | Some e ->
                succs,
                LS.push_block LS.KIf e ls,
                PIIf (node.CFGN.loc, v') :: stk,
                v')
      | _ -> succs, ls, stk, v'

  let track_possible_paths_of_cfg loc_callback (LS.{cfg} as ls) =
    match CFGH.find_first_node cfg with
    | None -> ls
    | Some n ->
        let rec f (ls, stk, visited) = function
          | [] -> ls
          | x :: xs ->
              let xloc = x.CFGN.loc in
              let ls = loc_callback xloc ls in
              let nexts, ls, stk, v' = analyze_node x (ls, stk, visited) in
              f (ls, stk, v') (nexts @ xs) in
        f (ls, [], CFG.NodeLocSet.empty) [n]
    

  let loc_callback gs logs loc ls =
    let rec f ls = function
      | [] -> ls
      | x :: xs ->
          let ls' = if 0 = CFGNLoc.compare x.LogUnit.nloc loc
                    then method_body_sub gs ls x else ls in
          f ls' xs in
    f ls logs

  (* API *)
  let method_body options gs glocs proc heap logs graph =
    let stk = init_stack glocs proc in
    let ls = LS.mk_empty glocs heap graph proc stk in
    let ls =
      if options.GeneratorOptions.use_prune_info
      then track_possible_paths_of_cfg (loc_callback gs logs) ls
      else List.fold_left (method_body_sub gs) ls logs in
    let ls, _ = LS.flatten_blocks ls in
    let ls = match method_body_ret ls with
             | None when H.typename_is "void" (ProcInfo.get_ret_type proc) -> ls
             | Some x -> LS.push_stmt x ls
             | _ -> LS.push_stmt (return_top proc) ls in
    let _, lst = LS.flatten_blocks ls in
    lst
end
