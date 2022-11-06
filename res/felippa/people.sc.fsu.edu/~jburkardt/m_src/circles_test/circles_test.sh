#! /bin/bash
#
matlab -nodisplay -nosplash -nodesktop -batch \
  "run('circles_test.m');exit;" > circles_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
