#! /bin/bash
#
matlab -nodisplay -nosplash -nodesktop -batch \
  "run('rk45_test.m');exit;" > rk45_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
