(* module JavaGenerator
 * generate java code from summary *)

open SemanticSummaryDomain

module Y = JoustSyntax
module P = JoustPretty
module F = Format


(* Util *)
(* make_string: make comp_unit into string(java code) *)
let make_string comp =
  let old = F.flush_str_formatter () in
  P.print F.str_formatter comp;
  let s = F.flush_str_formatter () in
  F.fprintf F.str_formatter "%s" old;
  s

(* ident: make syntax identifier *)
let ident name = Y.ident name 0
(* simple_type: make syntax typename *)
let simple_type name = Y.TypeName [ident name]

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

(* mk_unit: make syntax compilation unit *)
let mk_unit package decls =
  Y.({ package = package;
       imports = [];
       decls = decls;
       comments = [] })

(* Main *)

(* get_all_procs: load all procs from infer-out *)
let get_all_procs () =
  InferBase.ResultsDir.assert_results_dir "";
  Procedures.get_all (fun x y -> true) () 

(* get_semantic_summary: load semantic summary from infer-out *)
let get_semantic_summary f =
  let proc_opt = get_all_procs () |>
    Caml.List.find_opt (fun p -> (InferIR.Typ.Procname.to_string p) = f) in
  match proc_opt with
  | None -> None
  | Some p -> match Summary.get p with
    | None -> None
    | Some s ->
      match s.Summary.payloads.Payloads.semantic_summary with
      | None -> None
      | Some _ as o -> o

exception ParseException

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
                   | _ -> raise ParseException in
  let pkg, cls, mth = match String.split_on_char ' ' res' with
                      | "Java" :: xs -> (match List.rev xs with
                        | mth :: cls :: pkg -> List.rev pkg, cls, mth
                        | _ -> raise ParseException)
                      | _ -> raise ParseException in
  pkg, cls, mth, sign

(* parse_java_name: parse function name. *)
let parse_java_name name =
  try unescape_java_name name
  with _ -> ["___C"], "___Fn", name, None

(* extract_struct_name: extract struct/class name from type *)
let extract_struct_name c = InferIR.Typ.(match c with
  | CStruct n -> InferIR.QualifiedCppName.to_qual_string n
  | CppClass (n, _) -> InferIR.QualifiedCppName.to_qual_string n
  | _ -> raise ParseException )

(* is_jclass: check whether given type is 'jclass' *)
let is_jclass typ = InferIR.Typ.(match typ with
  | { desc = Tptr ({ desc = Tstruct c; _ }, Pk_pointer); _} ->
    (try extract_struct_name c = "_jclass"
     with _ -> false)
  | _ -> false )

(* parse_type: parse type and make syntax type *)
let parse_type typ = InferIR.Typ.(match typ with
  | { desc = Tint i; _ } ->
      let x = match i with
        | IUChar -> "boolean"
        | IChar | ISChar -> "byte"
        | IUShort -> "char"
        | IShort -> "short"
        | IInt -> "int"
        | ILongLong -> "long"
        | _ -> raise ParseException
      in simple_type x
  | { desc = Tfloat f; _ } ->
      let x = match f with
        | FFloat -> "float"
        | FDouble -> "double"
        | _ -> raise ParseException
      in Y.TypeName [ident x]
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
        | _ -> raise ParseException)
    |> simple_type
  | { desc = Tptr ({ desc = Tvoid; _}, Pk_pointer); _ } ->
    simple_type "Object"
  | _ -> raise ParseException)

(* parse_formals: parse formals. make static + formals *)
let parse_formals (formals: (InferIR.Mangled.t * InferIR.Typ.t) list) =
  let f = List.tl formals in
  let s = is_jclass (snd (List.hd f)) in
  let res = List.tl f |> List.map (fun (m, t) ->
              Y.({ v_mods = [];
                   v_type = parse_type t;
                   v_name = ident (InferIR.Mangled.to_string m) 0 })) in
  s, res

(* Generator *)
module PkgCls = struct
  type t = string list * string
  let compare a b = compare a b
end
module PkgClss = Map.Make(PkgCls)

(* insert_method: insert method into PkgClss-Methods map *)
let insert_method pkgclss (pkg, cls, mth, sign) static ret_type formals ss =
  let mods = [Y.Public] @ if static then [Y.Static] else [] in
  let m = mk_method mods mth ret_type formals [] (Y.Block []) in
  PkgClss.update
    (pkg, cls)
    (fun x -> match x with
     | None -> Some [m]
     | Some s -> Some (m :: s))
    pkgclss

(* gen_cmpls: make compilation_units from PkgClss-Methods map *)
let gen_cmpls pkgclss =
  PkgClss.fold
    (fun (pkg, cls) a b ->
      let c = mk_public_class cls (List.map (fun x -> Y.Method x) a) in
      mk_unit (Some (List.map ident pkg)) [Y.Class c] :: b)
    pkgclss []

(* each_proc: process for procedures *)
let each_proc res proc =
  let procname = InferIR.Typ.Procname.to_string proc in
  let parsed = parse_java_name procname in
  let summ = Summary.get proc in
  match summ with
  | None -> res
  | Some s -> match s.Summary.payloads.Payloads.semantic_summary with
    | None -> res
    | Some ss -> 
      let attr = Summary.get_attributes s in
      let ret_type = parse_type (attr.ret_type) in
      let static, formals = parse_formals attr.formals in
      insert_method res parsed static ret_type formals ss
      
(* generate: generate compilation_units from infer-out *)
let generate () =
  let procs = get_all_procs () in
  print_string ("#procs = " ^ string_of_int (List.length procs) ^ "\n");
  List.fold_left each_proc PkgClss.empty procs
  |> gen_cmpls

(* MAIN *)
let _ =
  let key = "t" in
  print_string "----------------------------------------\n";
  print_string ("KEY = " ^ key ^ "\n");
  print_string "## [JavaGenerator]\n";
  generate ()
  |> List.map make_string
  |> String.concat "\n;;;\n"
  |> print_string