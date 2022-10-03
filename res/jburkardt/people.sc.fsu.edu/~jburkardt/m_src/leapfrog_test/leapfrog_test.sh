#! /bin/bash
#
matlab -nodisplay -nosplash -nodesktop -batch \
  "run('leapfrog_test.m');exit;" > leapfrog_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
