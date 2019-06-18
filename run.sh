#!/bin/bash

ANDROID_NDK=/home/eshaj/Documents/repo/android_ndks/r18b
ANDROID_NDK_USR=$ANDROID_NDK/sysroot/usr/include
ANDROID_NDK_GLUE=$ANDROID_NDK/sources/android/native_app_glue

files=""

if [ $1 -eq "-c" ]; then
  do_capture=1
  shift
else
  do_capture=0
fi

for var in "$@" 
do 
    files="$files $var"
  done

if [ $do_capture -eq 1 ]; then
  echo "Capturing..."
  LD_LIBRARY_PATH=/home/eshaj/.opam/ocaml-variants.4.07.1+flambda/lib/z3 infer capture --debug -- clang++ -I $ANDROID_NDK_USR -I $ANDROID_NDK_GLUE -D __ANDROID__ -c $files
fi
:w

echo "Performing preanalysis for alias relations..."
LD_LIBRARY_PATH=/home/eshaj/.opam/ocaml-variants.4.07.1+flambda/lib/z3 infer analyze -P --pp-only
rm targets.dat &> /dev/null
echo "Performing preanalysis for the global environment..."
LD_LIBRARY_PATH=/home/eshaj/.opam/ocaml-variants.4.07.1+flambda/lib/z3 infer analyze -P --ssp-only
echo "Generating semantic summary for $files..."
LD_LIBRARY_PATH=/home/eshaj/.opam/ocaml-variants.4.07.1+flambda/lib/z3 infer analyze -P --ss-only
rm *.dat &> /dev/null
