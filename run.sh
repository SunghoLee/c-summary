#!/bin/bash

INFER=/home/eshaj/Documents/repo/c-summary/infer/bin/infer
ANDROID_NDK=/home/eshaj/Documents/repo/android_ndks/r18b
ANDROID_NDK_USR=$ANDROID_NDK/sysroot/usr/include
ANDROID_NDK_GLUE=$ANDROID_NDK/sources/android/native_app_glue

files=""

if [ $1 == "-c" ]; then
  do_capture=1
  MAKE=clang
  shift
elif [ $1 == "-c++" ]; then
  do_capture=1
  MAKE=clang++
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
  $INFER capture --debug -- $MAKE -I $ANDROID_NDK_USR -I $ANDROID_NDK_GLUE -D __ANDROID__ -c $files
fi

echo "Performing preanalysis for alias relations..."
  $INFER analyze -P --pp-only
rm targets.dat &> /dev/null
echo "Performing preanalysis for the global environment..."
  $INFER analyze -P --ssp-only
echo "Generating semantic summary for $files..."
  $INFER analyze -P --ss-only
rm *.dat &> /dev/null
