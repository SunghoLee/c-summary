open! IStd
open Core
module F = Format
module L = Logging

let skip_prefixs = 
  [ "_JNIEnv_"
  ; "JavaVM" ]

let jni_entry = 
  [ "OnLoad" ]

let android_entry =
  [ "android_main" ]

let jni_struct = 
  [ "android_app"
  ; "_JNIEnv" 
  ; "JNINativeInterface"
  ]

let jni_obj_typ = 
  [ "_jobject"
  ; "_jclass"
  ; "_jthrowable"
  ; "_jstring"
  ; "_jarray"
  ; "_jbooleanArray"
  ; "_jbyteArray"
  ; "_jcharArray"
  ; "_jshortArray"
  ; "_jintArray"
  ; "_jlongArray"
  ; "_jfloatArray"
  ; "_jdoubleArray"
  ; "_jobjectArray" ]

let is_jni f = 
  let name = Typ.Procname.to_string f in
  let matched = Caml.List.filter (fun prefix -> 
    String.is_prefix name prefix) skip_prefixs in
  not ((Caml.List.length matched) = 0)

let is_java_native f = 
  let name = Typ.Procname.to_string f in
  String.is_prefix name "Java_"

let is_jni_obj_typ typ = 
  match Typ.name typ with
  | Some s ->
      Caml.List.mem (Typ.Name.name s) jni_obj_typ  
  | None ->
      false

let is_jni_struct typ =
  match Typ.name typ with
  | Some s ->
      Caml.List.mem (Typ.Name.name s) jni_struct
  | None ->
      false

let is_entry f = 
  let name = Typ.Procname.to_string f in
  Caml.List.mem name (jni_entry @ android_entry)

let is_callable_from_java f =
  if is_entry f then
    true
  else
    let name = Typ.Procname.to_string f in
    String.is_prefix name ~prefix:"Java_"
  
