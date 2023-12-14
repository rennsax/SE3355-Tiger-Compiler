#!/bin/bash

set -e

target_rollback='src/tiger/output/output.cc'

if git status | grep ${target_rollback}; then
  echo "[ERROR] Refuse to rollback because output.cc is modified"
  exit 1
fi

git checkout lab5 -- ${target_rollback}
make gradelab5-2
git checkout HEAD -- ${target_rollback}