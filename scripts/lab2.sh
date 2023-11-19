WORKDIR=$(dirname "$(dirname "$(readlink -f "$0")")")

score_str="LAB2 SCORE"
testcase_dir=${WORKDIR}/testdata/lab2/testcases
ref_dir=${WORKDIR}/testdata/lab2/refs

testcase=$testcase_dir/$1

testcase_name=$(basename "$testcase" | cut -f1 -d".")
ref=${ref_dir}/${testcase_name}.out

./build/test_lex "$testcase" >&/tmp/output.txt
diff /tmp/output.txt "${ref}"
if [[ $? != 0 ]]; then
  echo "Error: Output mismatch"
  echo "${score_str}: 0"
  exit 1
fi

echo "[^_^]: Pass"
echo "${score_str}: 100"