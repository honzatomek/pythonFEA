#! /bin/bash
#
matlab -nodisplay -nosplash -nodesktop -batch \
  "run('rk23_test.m');exit;" > rk23_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
