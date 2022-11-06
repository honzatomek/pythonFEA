#! /bin/bash
#
matlab -nodisplay -nosplash -nodesktop -batch \
  "run('exactness_test.m');exit;" > exactness_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
