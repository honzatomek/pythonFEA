#! /bin/bash
#
matlab -nodisplay -nosplash -nodesktop -batch \
  "run('rk34_test.m');exit;" > rk34_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."