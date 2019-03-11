#!/bin/bash

ANDROID_NDK=/home/eshaj/Downloads/ndk/android-ndk-r18b
ANDROID_NDK_USR=$ANDROID_NDK/sysroot/usr/include
ANDROID_NDK_GLUE=$ANDROID_NDK/sources/android/native_app_glue

files=""

for var in "$@" 
do 
    files="$files $var"
  done

echo "Capturing..."
LD_LIBRARY_PATH=/home/eshaj/.opam/ocaml-variants.4.07.1+flambda/lib/z3 infer capture --debug -- clang++ -I $ANDROID_NDK_USR -I $ANDROID_NDK_GLUE -c $files
echo "Performing preanalysis for the global environment..."
LD_LIBRARY_PATH=/home/eshaj/.opam/ocaml-variants.4.07.1+flambda/lib/z3 infer analyze --ssp-only
echo "Generating semantic summary for $files..."
LD_LIBRARY_PATH=/home/eshaj/.opam/ocaml-variants.4.07.1+flambda/lib/z3 infer analyze --ss-only
