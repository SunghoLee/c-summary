#!/bin/bash

BASE_DIR=/home/eshaj/Documents/repo/c-summary/

ANDROID_NDK=/home/eshaj/Downloads/ndk/android-ndk-r18b
ANDROID_NDK_USR=$ANDROID_NDK/sysroot/usr/include
ANDROID_NDK_GLUE=$ANDROID_NDK/sources/android/native_app_glue

ANDROID_NDK=/home/lumiknit/Others/ndk/android-ndk-r19c
ANDROID_NDK_USR=$ANDROID_NDK/sysroot/usr/include
ANDROID_NDK_GLUE=$ANDROID_NDK/sources/android/native_app_glue
C_SUMMARY_DIR=$BASE_DIR

files=""

INFER=$BASE_DIR/infer/bin/infer

export LD_LIBRARY_PATH=/home/eshaj/.opam/ocaml-variants.4.07.1+flambda/lib/z3

for var in "$@" 
do 
    files="$files $var"
  done

echo "Capturing..."
$INFER capture --debug -- clang++ -I $ANDROID_NDK_USR -I $ANDROID_NDK_GLUE -D __ANDROID__ -c $files  &> capture_log.out
echo "Performing preanalysis for the global environment..."
$INFER analyze --ssp-only &> pre_log.out
echo "Generating semantic summary for $files..."
$INFER analyze --ss-only &> analysis_log.out

# export INFER_CWD=`pwd`
# pushd .
# cd $C_SUMMARY_DIR
# echo "Run tests in $INFER_CWD"
# dune runtest --force
# popd


export INFER_CWD=`pwd`
pushd .
cd $C_SUMMARY_DIR/infer/src
echo "Run tests in $INFER_CWD"
dune build JavaGenerator.exe > OUT
$C_SUMMARY_DIR/infer/src/_build/default/JavaGenerator.exe
popd
