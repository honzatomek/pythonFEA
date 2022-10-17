#! /bin/bash
#
matlab -nodisplay -nosplash -nodesktop -batch \
  "run('theta_method_test.m');exit;" > theta_method_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."