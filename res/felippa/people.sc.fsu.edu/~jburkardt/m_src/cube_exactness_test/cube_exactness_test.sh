#! /bin/bash
#
matlab -nodisplay -nosplash -nodesktop -batch \
  "run('cube_exactness_test.m');exit;" > cube_exactness_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."