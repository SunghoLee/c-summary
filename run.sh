#!/bin/bash
files=""

for var in "$@" 
do 
    files="$files $var"
  done

echo "Capturing..."
LD_LIBRARY_PATH=/home/eshaj/.opam/ocaml-variants.4.07.1+flambda/lib/z3 infer/bin/infer capture --debug -- clang++ -c $files
echo "Performing preanalysis for the global environment..."
LD_LIBRARY_PATH=/home/eshaj/.opam/ocaml-variants.4.07.1+flambda/lib/z3 infer/bin/infer analyze --ssp-only
echo "Generating semantic summary for $files..."
LD_LIBRARY_PATH=/home/eshaj/.opam/ocaml-variants.4.07.1+flambda/lib/z3 infer/bin/infer analyze --ss-only
