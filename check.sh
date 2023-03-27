#!/usr/bin/env bash

set -e

EXAMPLES=$(ls ./examples/*.bite 2>/dev/null | sed 's|.*/\(.*\)\.bite$|\1|')

echo "Checking examples..."

for name in $EXAMPLES; do
    echo "checking example ${name}"
    dune exec bite -- -l ./examples/${name}.bite -o ./examples/${name}.compiled.c -oir ./examples/${name}.ir
    clang -O3 -o ./examples/${name}.exe ./examples/${name}.compiled.c
    clang-format -i ./examples/${name}.compiled.c
    ./examples/${name}.exe
done

echo "Check completed successfully!"