#!/usr/bin/env bash

EXAMPLES=$(ls ./examples/*.bite 2>/dev/null | sed 's|.*/\(.*\)\.bite$|\1|')

echo "Checking examples..."

for name in $EXAMPLES; do
    echo "checking example ${name}"
    dune exec bite -- -l ./examples/${name}.bite -o ./examples/${name}.compiled.c
    clang -O3 -o ./examples/${name}.exe ./examples/${name}.compiled.c
    ./examples/${name}.exe
done

echo "Check complete without error."