#! /bin/bash
#
matlab -nodisplay -nosplash -nodesktop -batch \
  "run('backward_euler_test.m');exit;" > backward_euler_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
