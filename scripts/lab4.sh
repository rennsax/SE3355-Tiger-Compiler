#!/bin/bash

testcase=$1
: ${testcase:=1}

test_some_case() {
  local test_file_path=testdata/lab4/testcases/test$1.tig
  local ref_file_path=testdata/lab4/refs/test$1.out
  # local tmp_file_path=/tmp/$1.out

  # also output the original output
  diff -s <( \
    ./build/test_semant ${test_file_path} 2>&1 \
    | tee /dev/stderr \
    | awk -F: '{print $3}' \
    | sed 's/[[:space:]]*//' \
    ; echo "--------" >&2 \
    ) \
    <(awk -F: '{print $3}' ${ref_file_path} \
    | sed 's/[[:space:]]*//' \
    )
}

echo "test $testcase"
test_some_case $testcase