#!/bin/bash
set -e

if [ "$(basename "$PWD")" != "compiler" ]; then
  echo "Please run this script from the root directory"
  exit 1
fi

# Build the compiler
rm -rf build
mkdir build && cd build || exit
cmake .. && make -j tiger-compiler
cd .. || exit
bash ./scripts/grade.sh lab6
