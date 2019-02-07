#!/bin/bash
files=""

for var in "$@" 
do 
    files="$files $var"
  done

echo "Capturing..."
infer/bin/infer capture --debug -- clang++ -c $files
echo "Performing preanalysis for the global environment..."
infer/bin/infer analyze --ssp-only
echo "Generating semantic summary for $files..."
infer/bin/infer analyze --ss-only
