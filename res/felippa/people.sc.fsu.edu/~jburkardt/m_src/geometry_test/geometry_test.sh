#! /bin/bash
#
matlab -nodisplay -nosplash -nodesktop -batch \
  "run('geometry_test.m');exit;" > geometry_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
