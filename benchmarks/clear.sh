#!/bin/bash

extension=(o out ast.sh dat)

for ext in "${extension[@]}"
do
  echo "Remove all .$ext files..."
  find . -type f -name *.$ext -delete &> /dev/null
done

echo "Remove infer-out dir..."
find . -name 'infer-out' -type d -exec rm "{}" \; &> /dev/null
