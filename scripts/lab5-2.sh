#!/bin/bash

test_case=testdata/lab5or6/testcases/$1.tig
ref_file=testdata/lab5or6/refs/$1.out
./build/test_codegen "$test_case"
mv "$test_case.s" "$test_case.lab5.s"

echo "------------------------"
python3 interpret.py "$test_case.lab5.s"
echo "------------------------"
cat $ref_file