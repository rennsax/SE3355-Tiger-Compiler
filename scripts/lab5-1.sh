testcase=$1
: ${testcase:=test1}

test_file_path=testdata/lab5or6/testcases/${testcase}.tig
ref_file_path=testdata/lab5or6/refs-part1/${testcase}.out

echo Test ${testcase}

diff ${ref_file_path} <(build/test_translate ${test_file_path} 2>&1)