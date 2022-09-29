#! /bin/bash
#
matlab -nodisplay -nosplash -nodesktop -batch \
  "run('cauchy_method_test.m');exit;" > cauchy_method_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
