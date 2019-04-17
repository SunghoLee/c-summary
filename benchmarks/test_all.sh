#!/bin/bash
SILENT=true

benchmarks=('b1' 'b2' 'b3' 'b5' 'b6' 'b7' 'b8' 'b9' 'b10' 'b11' 'b12' 'b13' 'b14' 'b15' 'b16' 'b17' 'b18' 'b19' 'b20' 'b21' 'b22' 'b23')

run_command() {
  if $SILENT; then
    $1 &> /dev/null
  else
    $1
  fi
}

for dir in ${benchmarks[@]}
do
  run_command "pushd ."
  run_command "cd $dir"
  if [ $dir = "b12" ]; then
    bash ../../test.sh "$dir"_1.cpp "$dir"_2.cpp
  else
    bash ../../test.sh $dir.cpp
  fi
  run_command "popd" 
done
