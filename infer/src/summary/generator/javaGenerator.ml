(* module JavaGenerator
 * generate java code from summary *)

open SemanticSummaryDomain

module Y = JoustSyntax
module P = JoustPretty
module F = Format

module H = JavaGeneratorModels.ModelHelper
module S = JavaGeneratorModels.State
module M = JavaGeneratorModels.SimpleModel

module ProcInfo = JavaGeneratorModels.ProcInfo

(* -- Util -- *)
(* make_string: make comp_unit into string(java code) *)
let make_string comp =
  let old = F.flush_str_formatter () in
  P.print F.str_formatter comp;
  let s = F.flush_str_formatter () in
  F.fprintf F.str_formatter "%s" old;
  s

(* simple_type: make syntax typename *)
let simple_type name = Y.TypeName [Y.ident name 0]

(* mk_method: make syntax method *)
let mk_method mods name ret_typ args throws body =
  Y.({ m_var = { v_mods = mods;
                 v_type = ret_typ;
                 v_name = ident name 0 };
       m_formals = args;
       m_throws = throws;
       m_body = body })

(* mk_public_class: make syntax public class *)
let mk_public_class name body =
  Y.({ cl_mods = [Public];
       cl_name = ident name 0;
       cl_super = None;
       cl_impls = [];
       cl_body = body })

(* mk_compilation_unit: make syntax compilation unit *)
let mk_compilation_unit package decls =
  Y.({ package = package;
       imports = [[ident "__Model.*" 0]];
       decls = decls;
       comments = [] })

(* -- Main -- *)

(* get_all_procs: load all procs from infer-out
 * return is `string * bool`, the 1st is Proc and the 2nd is whether the proc
 * is entry *)
let get_all_procs () =
  (* reorder: bring all possible entries to the front of the list *)
  let rec reorder entries others lst = match lst with
    | [] -> entries @ others
    | x :: xs ->
      if List.mem (InferIR.Typ.Procname.to_string x) M.possible_entries
      then reorder ((x, true) :: entries) others xs
      else reorder entries ((x, false) :: others) xs in
  InferBase.ResultsDir.assert_results_dir "";
  Procedures.get_all (fun x y -> true) ()
  |> reorder [] []

exception ParseException of string

(* unescape_java_name: parse native function name in c for java *)
let unescape_java_name name =
  let l = String.length name in
  let buf = Buffer.create l in
  let add_c = Buffer.add_char buf in
  let rec f idx =
    if idx >= l then Buffer.contents buf
    else
      let c = String.get name idx in
      if c = '_'
      then match String.get name (idx + 1) with
        | '_' -> add_c ':'; f (idx + 2)
        | '1' -> add_c '_'; f (idx + 2)
        | '2' -> add_c ';'; f (idx + 2)
        | '3' -> add_c '['; f (idx + 2)
        | _ -> add_c ' '; f (idx + 1)
      else (add_c c; f (idx + 1)) in
  let res = f 0 in
  let res', sign = match String.split_on_char ':' res with
                   | [x] -> x, None
                   | [x; s] -> x, Some s
                   | [] -> raise (ParseException "name is empty")
                   | _ -> raise (ParseException "too many colons in name") in
  let pkg, cls, mth = match String.split_on_char ' ' res' with
                      | "Java" :: xs -> (match List.rev xs with
                        | mth :: cls :: pkg -> List.rev pkg, cls, mth
                        | _ ->
                          raise (ParseException "wrong format of java name"))
                      | _ -> raise (ParseException "not java name") in
  pkg, cls, mth, sign

(* parse_java_name: parse function name
 *** IT DOES NOT HANDLE OVERLOEADED METHOD *)
let parse_java_name name =
  try true, unescape_java_name name
  with _ -> false, ([M.c_fn_pkg_name], M.c_fn_class_name, name, None)

(* extract_struct_name: extract struct/class name from type *)
let extract_struct_name c = InferIR.Typ.(match c with
  | CStruct n -> InferIR.QualifiedCppName.to_qual_string n
  | CppClass (n, _) -> InferIR.QualifiedCppName.to_qual_string n
  | _ -> raise (ParseException "unknown struct name") )

(* is_jclass: check whether given type is 'jclass' *)
let is_jclass typ = InferIR.Typ.(match typ with
  | { desc = Tptr ({ desc = Tstruct c; _ }, Pk_pointer); _} ->
    (try extract_struct_name c = "_jclass"
     with _ -> false)
  | _ -> false )

let unknown_type_name = M.model_pkg_name ^ ".__Unknown"
(* parse_type: parse type and make syntax type
 * if given type may not be represented in Java, it'll return
 * `__Model.Unknown` *)
let parse_type typ = InferIR.Typ.(match typ with
  | { desc = Tint i; _ } ->
      let x = match i with
        | IUChar -> "boolean"
        | IChar | ISChar -> "byte"
        | IUShort -> "char"
        | IShort -> "short"
        | IInt -> "int"
        | ILongLong -> "long"
        | _ -> unknown_type_name
      in simple_type x
  | { desc = Tfloat f; _ } ->
      let x = match f with
        | FFloat -> "float"
        | FDouble -> "double"
        | _ -> unknown_type_name
      in Y.TypeName [Y.ident x 0]
  | { desc = Tvoid; _ } -> simple_type "void"
  | { desc = Tptr ({ desc = Tstruct c; _}, Pk_pointer); _ } ->
    extract_struct_name c
    |> (function
        | "_jobject" -> "Object"
        | "_jclass" -> "Class"
        | "_jthrowable" -> "Throwable"
        | "_jstring" -> "String"
        | "_jarray" -> "Array"
        | "_jbooleanArray" -> "boolean[]"
        | "_jbyteArray" -> "byte[]"
        | "_jcharArray" -> "char[]"
        | "_jshortArray" -> "short[]"
        | "_jintArray" -> "int[]"
        | "_jlongArray" -> "long[]"
        | "_jfloatArray" -> "float[]"
        | "_jdoubleArray" -> "double[]"
        | "_jobjectArray" -> "Object[]"
        | _ -> unknown_type_name)
    |> simple_type
  | { desc = Tptr ({ desc = Tvoid; _}, Pk_pointer); _ } ->
    simple_type "Object"
  | _ -> simple_type unknown_type_name)

(* parse_formals: parse formals, and return (is_static, parsed_formals) *)
let parse_formals is_java
                  (formals: (InferIR.Mangled.t * InferIR.Typ.t) list) =
  let f = List.map (fun (m, t) ->
            Y.({v_mods = [];
                v_type = parse_type t;
                v_name = ident (InferIR.Mangled.to_string m) 0})) in
  if is_java
  then
    let fs = List.tl formals in
    let is_static = is_jclass (snd (List.hd fs)) in
    let res = List.tl fs |> f in
    let env, this = match formals with
      | (e, _) :: (t, _) :: _ ->
        InferIR.Mangled.to_string e, InferIR.Mangled.to_string t
      | _ -> failwith "env argument is not found" in
    let kind =
      if is_static
      then ProcInfo.Static (env, this)
      else ProcInfo.Method (env, this) in
    kind, is_static, res
  else
    ProcInfo.C, false, (*f formals*) []

(* sort_logs: sort logs *)
let sort_logs =
  let cmp {LogUnit.call_sites=c1} {LogUnit.call_sites=c2} =
    CallSite.compare_list c1 c2 in
  List.sort cmp

(* get_summary_k: get procedure's summary and pass it to the callback `cb` *)
let get_summary_k proc default cb =
  match Summary.get proc with
  | None -> default ()
  | Some s -> match s.Summary.payloads.Payloads.semantic_summary with
    | None -> default ()
    | Some ss -> cb s ss

let parse_body state name {heap; logs} =
  CallLogs.fold (fun e l -> e :: l) logs []
  |> sort_logs
  |> M.method_body state name heap
  
(* Generator *)
(* PkgClss: (key=Package-Class, value=methods) map *)
module PkgClss = Map.Make(struct
  type t = string list * string

  let compare a b = compare a b
end)

(* insert_method: insert method into PkgClss-Methods map *)
let insert_method pkgclss (pkg, cls, mth, sign)
                  static ret_type formals body =
  let mods = [Y.Public] @ if static then [Y.Static] else [] in
  let m = mk_method mods mth ret_type formals [] body in
  PkgClss.update
    (pkg, cls)
    (function
     | None -> Some [m]
     | Some s -> Some (m :: s))
    pkgclss

(* gen_compilation_units: make compilation_units from PkgClss-Methods map *)
let gen_compilation_units pkgclss =
  PkgClss.fold
    (fun (pkg, cls) a b ->
      let c = mk_public_class (cls ^ "_") (List.map (fun x -> Y.Method x) a) in
      let p = match pkg with
        | [] -> None
        | _ -> Some (List.map (fun x -> Y.ident x 0) pkg) in
      (pkg, cls, mk_compilation_unit p [Y.Class c]) :: b)
    pkgclss []

(* each_proc_cb: process for procedures *)
let each_proc_cb state res (proc, is_ent) =
  let procname = InferIR.Typ.Procname.to_string proc in
  let is_java, parsed = parse_java_name procname in
  get_summary_k proc (fun () -> res) (fun s ss ->
    let attr = Summary.get_attributes s in
    let ret_type = parse_type (attr.ret_type) in
    let kind, is_static, formals = parse_formals is_java attr.formals in
    let proc = ProcInfo.({name = procname;
                          kind = kind;
                          ret_type = ret_type;
                          is_entry = is_ent}) in
    let body = Y.Block (parse_body state proc ss) in
    let res' = S.fold_name_of state procname res
      (fun name res -> insert_method res name is_static ret_type formals body)
    in insert_method res' parsed is_static ret_type formals body)

(* generate: generate compilation_units from infer-out *)
let generate () =
  let procs = get_all_procs () in
  print_string ("#procs = " ^ string_of_int (List.length procs) ^ "\n");
  let state = S.mk_empty () in
  List.fold_left (each_proc_cb state) PkgClss.empty procs
  |> gen_compilation_units

(* write_as_files: generate java files from the result of `generate` *)
let write_as_files base_dir result =
  let cwd = Sys.getenv "INFER_CWD" in
  Sys.chdir cwd;
  Printf.printf "Make a directory \"%s\"...\n" base_dir;
  (if Sys.file_exists base_dir
  then if Sys.is_directory base_dir
       then ()
       else failwith ("there is a file named `" ^ base_dir ^ "`")
  else let _ = Sys.command ("mkdir \"" ^ base_dir ^ "\"") in ());
  Sys.chdir (cwd ^ "/" ^ base_dir);
  List.iter (fun (pkg, cls, compl_unit) ->
               let dir = if List.length pkg = 0
                         then "."
                         else String.concat "/" pkg in
               let f = dir ^ "/" ^ cls ^ "_.java" in
               Printf.printf "Make a file \"%s\"...\n" f;
               let _ = Sys.command ("mkdir -p \"" ^ dir ^ "\"") in
               let oc = open_out f in
               Printf.fprintf oc "%s" (make_string compl_unit);
               close_out oc)
            result

(* MAIN *)
let _ =
  print_string "## [JavaGenerator]\n";
  let result = generate () in
  write_as_files "java-gen-out" result;
  result
  |> List.map (fun (_, _, cmpl) -> make_string cmpl)
  |> String.concat "\n;;;\n"
  |> print_string
