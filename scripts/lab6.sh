#!/bin/bash

set -e

test_case=testdata/lab5or6/testcases/$1.tig
runtime_path=src/tiger/runtime/runtime.c
# ref_file=testdata/lab5or6/refs/$1.out
./build/tiger-compiler "$test_case"
x86_64-linux-gnu-gcc -Wl,--wrap,getchar -m64 "$test_case.s" "$runtime_path" -o "x64/$1.out"