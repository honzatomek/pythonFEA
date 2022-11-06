#! /bin/bash
#
matlab -nodisplay -nosplash -nodesktop -batch \
  "run('voronoi_test.m');exit;" > voronoi_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
