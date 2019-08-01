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

let is_jni_env_for_c typ =
  match Typ.name typ with
  | Some s -> 
      (Typ.Name.name s) = "JNINativeInterface_" || (Typ.Name.name s) = "JNINativeInterface"
  | None ->
      false

let is_jni_env_ptr_for_c typ =
  match typ.Typ.desc with
  | Tptr (typ', _) ->
      is_jni_env_for_c typ'
  | _ ->
      false

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
  
let put_jni_env_modeling loc heap = 
  let mk_fun_ptr = Typ.Procname.from_string_c_fun in
  let add_fun_field loc fun_name heap =
    let open SemanticSummaryDomain in
    let jni_fun_name = "_JNIEnv_" ^ fun_name in
    let f = Loc.mk_const_of_string fun_name in
    let offset = Loc.mk_offset loc f in
    let f_loc = mk_fun_ptr jni_fun_name |> Loc.mk_fun_pointer in
    Heap.add offset (Val.singleton (f_loc, Cst.cst_true)) heap
  in
  add_fun_field loc "GetVersion" heap
  |> add_fun_field loc "DefineClass" 
  |> add_fun_field loc "FindClass" 
  |> add_fun_field loc "FromReflectedMethod"
  |> add_fun_field loc "FromReflectedField"
  |> add_fun_field loc "ToReflectedMethod"
  |> add_fun_field loc "GetSuperclass"
  |> add_fun_field loc "IsAssignableFrom"
  |> add_fun_field loc "ToReflectedField"
  |> add_fun_field loc "Throw"
  |> add_fun_field loc "ThrowNew"
  |> add_fun_field loc "ExceptionOccurred"
  |> add_fun_field loc "ExceptionDescribe"
  |> add_fun_field loc "ExceptionClear"
  |> add_fun_field loc "FatalError"
  |> add_fun_field loc "PushLocalFrame"
  |> add_fun_field loc "PopLocalFrame"
  |> add_fun_field loc "NewGlobalRef"
  |> add_fun_field loc "DeleteGlobalRef"
  |> add_fun_field loc "DeleteLocalRef"
  |> add_fun_field loc "IsSameObject"
  |> add_fun_field loc "NewLocalRef"
  |> add_fun_field loc "EnsureLocalCapacity"
  |> add_fun_field loc "AllocObject"
  |> add_fun_field loc "NewObject"
  |> add_fun_field loc "NewObjectV"
  |> add_fun_field loc "NewObjectA"
  |> add_fun_field loc "GetObjectClass"
  |> add_fun_field loc "IsInstanceOf"
  |> add_fun_field loc "GetMethodID"
  |> add_fun_field loc "CallObjectMethod"
  |> add_fun_field loc "CallObjectMethodV"
  |> add_fun_field loc "CallObjectMethodA"
  |> add_fun_field loc "CallBooleanMethod"
  |> add_fun_field loc "CallBooleanMethodV"
  |> add_fun_field loc "CallBooleanMethodA"
  |> add_fun_field loc "CallByteMethod"
  |> add_fun_field loc "CallByteMethodV"
  |> add_fun_field loc "CallByteMethodA"
  |> add_fun_field loc "CallCharMethod"
  |> add_fun_field loc "CallCharMethodV"
  |> add_fun_field loc "CallCharMethodA"
  |> add_fun_field loc "CallShortMethod"
  |> add_fun_field loc "CallShortMethodV"
  |> add_fun_field loc "CallShortMethodA"
  |> add_fun_field loc "CallIntMethod"
  |> add_fun_field loc "CallIntMethodV"
  |> add_fun_field loc "CallIntMethodA"
  |> add_fun_field loc "CallLongMethod"
  |> add_fun_field loc "CallLongMethodV"
  |> add_fun_field loc "CallLongMethodA"
  |> add_fun_field loc "CallFloatMethod"
  |> add_fun_field loc "CallFloatMethodV"
  |> add_fun_field loc "CallFloatMethodA"
  |> add_fun_field loc "CallDoubleMethod"
  |> add_fun_field loc "CallDoubleMethodV"
  |> add_fun_field loc "CallDoubleMethodA"
  |> add_fun_field loc "CallVoidMethod"
  |> add_fun_field loc "CallVoidMethodV"
  |> add_fun_field loc "CallVoidMethodA"
  |> add_fun_field loc "CallNonvirtualObjectMethod"
  |> add_fun_field loc "CallNonvirtualObjectMethodV"
  |> add_fun_field loc "CallNonvirtualObjectMethodA"
  |> add_fun_field loc "CallNonvirtualBooleanMethod"
  |> add_fun_field loc "CallNonvirtualBooleanMethodV"
  |> add_fun_field loc "CallNonvirtualBooleanMethodA"
  |> add_fun_field loc "CallNonvirtualByteMethod"
  |> add_fun_field loc "CallNonvirtualByteMethodV"
  |> add_fun_field loc "CallNonvirtualByteMethodA"
  |> add_fun_field loc "CallNonvirtualCharMethod"
  |> add_fun_field loc "CallNonvirtualCharMethodV"
  |> add_fun_field loc "CallNonvirtualCharMethodA"
  |> add_fun_field loc "CallNonvirtualShortMethod"
  |> add_fun_field loc "CallNonvirtualShortMethodV"
  |> add_fun_field loc "CallNonvirtualShortMethodA"
  |> add_fun_field loc "CallNonvirtualIntMethod"
  |> add_fun_field loc "CallNonvirtualIntMethodV"
  |> add_fun_field loc "CallNonvirtualIntMethodA"
  |> add_fun_field loc "CallNonvirtualLongMethod"
  |> add_fun_field loc "CallNonvirtualLongMethodV"
  |> add_fun_field loc "CallNonvirtualLongMethodA"
  |> add_fun_field loc "CallNonvirtualFloatMethod"
  |> add_fun_field loc "CallNonvirtualFloatMethodV"
  |> add_fun_field loc "CallNonvirtualFloatMethodA"
  |> add_fun_field loc "CallNonvirtualDoubleMethod"
  |> add_fun_field loc "CallNonvirtualDoubleMethodV"
  |> add_fun_field loc "CallNonvirtualDoubleMethodA"
  |> add_fun_field loc "CallNonvirtualVoidMethod"
  |> add_fun_field loc "CallNonvirtualVoidMethodV"
  |> add_fun_field loc "CallNonvirtualVoidMethodA"
  |> add_fun_field loc "GetFieldID"
  |> add_fun_field loc "GetObjectField"
  |> add_fun_field loc "GetBooleanField"
  |> add_fun_field loc "GetByteField"
  |> add_fun_field loc "GetCharField"
  |> add_fun_field loc "GetShortField"
  |> add_fun_field loc "GetIntField"
  |> add_fun_field loc "GetLongField"
  |> add_fun_field loc "GetFloatField"
  |> add_fun_field loc "GetDoubleField"
  |> add_fun_field loc "SetObjectField"
  |> add_fun_field loc "SetBooleanField"
  |> add_fun_field loc "SetByteField"
  |> add_fun_field loc "SetCharField"
  |> add_fun_field loc "SetShortField"
  |> add_fun_field loc "SetIntField"
  |> add_fun_field loc "SetLongField"
  |> add_fun_field loc "SetFloatField"
  |> add_fun_field loc "SetDoubleField"
  |> add_fun_field loc "GetStaticMethodID"
  |> add_fun_field loc "CallStaticObjectMethod"
  |> add_fun_field loc "CallStaticObjectMethodV"
  |> add_fun_field loc "CallStaticObjectMethodA"
  |> add_fun_field loc "CallStaticBooleanMethod"
  |> add_fun_field loc "CallStaticBooleanMethodV"
  |> add_fun_field loc "CallStaticBooleanMethodA"
  |> add_fun_field loc "CallStaticByteMethod"
  |> add_fun_field loc "CallStaticByteMethodV"
  |> add_fun_field loc "CallStaticByteMethodA"
  |> add_fun_field loc "CallStaticCharMethod"
  |> add_fun_field loc "CallStaticCharMethodV"
  |> add_fun_field loc "CallStaticCharMethodA"
  |> add_fun_field loc "CallStaticShortMethod"
  |> add_fun_field loc "CallStaticShortMethodV"
  |> add_fun_field loc "CallStaticShortMethodA"
  |> add_fun_field loc "CallStaticIntMethod"
  |> add_fun_field loc "CallStaticIntMethodV"
  |> add_fun_field loc "CallStaticIntMethodA"
  |> add_fun_field loc "CallStaticLongMethod"
  |> add_fun_field loc "CallStaticLongMethodV"
  |> add_fun_field loc "CallStaticLongMethodA"
  |> add_fun_field loc "CallStaticFloatMethod"
  |> add_fun_field loc "CallStaticFloatMethodV"
  |> add_fun_field loc "CallStaticFloatMethodA"
  |> add_fun_field loc "CallStaticDoubleMethod"
  |> add_fun_field loc "CallStaticDoubleMethodV"
  |> add_fun_field loc "CallStaticDoubleMethodA"
  |> add_fun_field loc "CallStaticVoidMethod"
  |> add_fun_field loc "CallStaticVoidMethodV"
  |> add_fun_field loc "CallStaticVoidMethodA"
  |> add_fun_field loc "GetStaticFieldID"
  |> add_fun_field loc "GetStaticObjectField"
  |> add_fun_field loc "GetStaticBooleanField"
  |> add_fun_field loc "GetStaticByteField"
  |> add_fun_field loc "GetStaticCharField"
  |> add_fun_field loc "GetStaticShortField"
  |> add_fun_field loc "GetStaticIntField"
  |> add_fun_field loc "GetStaticLongField"
  |> add_fun_field loc "GetStaticFloatField"
  |> add_fun_field loc "GetStaticDoubleField"
  |> add_fun_field loc "SetStaticObjectField"
  |> add_fun_field loc "SetStaticBooleanField"
  |> add_fun_field loc "SetStaticByteField"
  |> add_fun_field loc "SetStaticCharField"
  |> add_fun_field loc "SetStaticShortField"
  |> add_fun_field loc "SetStaticIntField"
  |> add_fun_field loc "SetStaticLongField"
  |> add_fun_field loc "SetStaticFloatField"
  |> add_fun_field loc "SetStaticDoubleField"
  |> add_fun_field loc "NewString"
  |> add_fun_field loc "GetStringLength"
  |> add_fun_field loc "GetStringChars"
  |> add_fun_field loc "ReleaseStringChars"
  |> add_fun_field loc "NewStringUTF"
  |> add_fun_field loc "GetStringUTFLength"
  |> add_fun_field loc "GetStringUTFChars"
  |> add_fun_field loc "ReleaseStringUTFChars"
  |> add_fun_field loc "GetArrayLength"
  |> add_fun_field loc "NewObjectArray"
  |> add_fun_field loc "GetObjectArrayElement"
  |> add_fun_field loc "SetObjectArrayElement"
  |> add_fun_field loc "NewBooleanArray"
  |> add_fun_field loc "NewByteArray"
  |> add_fun_field loc "NewCharArray"
  |> add_fun_field loc "NewShortArray"
  |> add_fun_field loc "NewIntArray"
  |> add_fun_field loc "NewLongArray"
  |> add_fun_field loc "NewFloatArray"
  |> add_fun_field loc "NewDoubleArray"
  |> add_fun_field loc "GetBooleanArrayElements"
  |> add_fun_field loc "GetByteArrayElements"
  |> add_fun_field loc "GetCharArrayElements"
  |> add_fun_field loc "GetShortArrayElements"
  |> add_fun_field loc "GetIntArrayElements"
  |> add_fun_field loc "GetLongArrayElements"
  |> add_fun_field loc "GetFloatArrayElements"
  |> add_fun_field loc "GetDoubleArrayElements"
  |> add_fun_field loc "ReleaseBooleanArrayElements"
  |> add_fun_field loc "ReleaseByteArrayElements"
  |> add_fun_field loc "ReleaseCharArrayElements"
  |> add_fun_field loc "ReleaseShortArrayElements"
  |> add_fun_field loc "ReleaseIntArrayElements"
  |> add_fun_field loc "ReleaseLongArrayElements"
  |> add_fun_field loc "ReleaseFloatArrayElements"
  |> add_fun_field loc "ReleaseDoubleArrayElements"
  |> add_fun_field loc "GetBooleanArrayRegion"
  |> add_fun_field loc "GetByteArrayRegion"
  |> add_fun_field loc "GetCharArrayRegion"
  |> add_fun_field loc "GetShortArrayRegion"
  |> add_fun_field loc "GetIntArrayRegion"
  |> add_fun_field loc "GetLongArrayRegion"
  |> add_fun_field loc "GetFloatArrayRegion"
  |> add_fun_field loc "GetDoubleArrayRegion"
  |> add_fun_field loc "SetBooleanArrayRegion"
  |> add_fun_field loc "SetByteArrayRegion"
  |> add_fun_field loc "SetCharArrayRegion"
  |> add_fun_field loc "SetShortArrayRegion"
  |> add_fun_field loc "SetIntArrayRegion"
  |> add_fun_field loc "SetLongArrayRegion"
  |> add_fun_field loc "SetFloatArrayRegion"
  |> add_fun_field loc "SetDoubleArrayRegion"
  |> add_fun_field loc "RegisterNatives"
  |> add_fun_field loc "UnregisterNatives"
  |> add_fun_field loc "MonitorEnter"
  |> add_fun_field loc "MonitorExit"
  |> add_fun_field loc "GetJavaVM"
  |> add_fun_field loc "GetStringRegion"
  |> add_fun_field loc "GetStringUTFRegion"
  |> add_fun_field loc "GetPrimitiveArrayCritical"
  |> add_fun_field loc "ReleasePrimitiveArrayCritical"
  |> add_fun_field loc "GetStringCritical"
  |> add_fun_field loc "ReleaseStringCritical"
  |> add_fun_field loc "NewWeakGlobalRef"
  |> add_fun_field loc "DeleteWeakGlobalRef"
  |> add_fun_field loc "ExceptionCheck"
  |> add_fun_field loc "NewDirectByteBuffer"
  |> add_fun_field loc "GetDirectBufferAddress"
  |> add_fun_field loc "GetDirectBufferCapacity"
  |> add_fun_field loc "GetObjectRefType"
