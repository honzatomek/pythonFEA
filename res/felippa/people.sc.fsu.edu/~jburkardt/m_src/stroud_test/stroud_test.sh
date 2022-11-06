#! /bin/bash
#
matlab -nodisplay -nosplash -nodesktop -batch \
  "run('stroud_test.m');exit;" > stroud_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
