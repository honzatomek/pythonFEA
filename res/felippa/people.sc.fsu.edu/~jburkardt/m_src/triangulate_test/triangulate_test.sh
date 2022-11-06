#! /bin/bash
#
matlab -nodisplay -nosplash -nodesktop -batch \
  "run('triangulate_test.m');exit;" > triangulate_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
