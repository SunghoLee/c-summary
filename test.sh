#!/bin/bash

ANDROID_NDK=/home/eshaj/Downloads/ndk/android-ndk-r18b
ANDROID_NDK_USR=$ANDROID_NDK/sysroot/usr/include
ANDROID_NDK_GLUE=$ANDROID_NDK/sources/android/native_app_glue
C_SUMMARY_DIR=/home/eshaj/Documents/repo/c-summary/

JNI_H_FOR_C="$C_SUMMARY_DIR/jni_c.h"
CLANG=clang++

files=""

for var in "$@" 
do 
  files="$files $var"
done

echo "Capturing..."
LD_LIBRARY_PATH=/home/eshaj/.opam/ocaml-variants.4.07.1+flambda/lib/z3 infer capture --debug -- $CLANG -include $JNI_H_FOR_C -I $ANDROID_NDK_USR -I $ANDROID_NDK_GLUE -D __ANDROID__ -c $files &> capture_log.out
echo "Performing preanalysis for the global environment..."
LD_LIBRARY_PATH=/home/eshaj/.opam/ocaml-variants.4.07.1+flambda/lib/z3 infer analyze --ssp-only &> pre_log.out
echo "Generating semantic summary for $files..."
LD_LIBRARY_PATH=/home/eshaj/.opam/ocaml-variants.4.07.1+flambda/lib/z3 infer analyze --ss-only &> analysis_log.out

export INFER_CWD=`pwd`
pushd .
cd $C_SUMMARY_DIR
echo "Run tests in $INFER_CWD"
LD_LIBRARY_PATH=/home/eshaj/.opam/ocaml-variants.4.07.1+flambda/lib/z3 dune runtest --force
popd

# Gen Java
# pushd .
# cd $C_SUMMARY_DIR
# echo "Run tests in $INFER_CWD"
# LD_LIBRARY_PATH=/home/eshaj/.opam/ocaml-variants.4.07.1+flambda/lib/z3 dune build JavaGenerator.exe
# LD_LIBRARY_PATH=/home/eshaj/.opam/ocaml-variants.4.07.1+flambda/lib/z3 $C_SUMMAR_DIR/infer/src/_build/default/JavaGenerator.exe
# popd
