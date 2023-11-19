testcase=$1
: ${testcase:=test1}

test_file_path=testdata/lab3/testcases/${testcase}.tig
ref_file_path=testdata/lab3/refs/${testcase}.out

echo Test ${testcase}

diff ${ref_file_path} <(build/test_parse ${test_file_path} 2>&1)