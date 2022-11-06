#! /bin/bash
#
matlab -nodisplay -nosplash -nodesktop -batch \
  "run('fem_basis_test.m');exit;" > fem_basis_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
