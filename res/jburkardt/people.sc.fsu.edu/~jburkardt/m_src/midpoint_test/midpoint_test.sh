#! /bin/bash
#
matlab -nodisplay -nosplash -nodesktop -batch \
  "run('midpoint_test.m');exit;" > midpoint_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
