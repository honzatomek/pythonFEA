#! /bin/bash
#
matlab -nodisplay -nosplash -nodesktop -batch \
  "run('delaunay_test.m');exit;" > delaunay_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
