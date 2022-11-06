#! /bin/bash
#
matlab -nodisplay -nosplash -nodesktop -batch \
  "run('convhull_test.m');exit;" > convhull_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
