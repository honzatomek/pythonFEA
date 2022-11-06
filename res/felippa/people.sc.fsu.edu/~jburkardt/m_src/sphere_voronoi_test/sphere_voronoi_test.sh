#! /bin/bash
#
matlab -nodisplay -nosplash -nodesktop -batch \
  "run('sphere_voronoi_test.m');exit;" > sphere_voronoi_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
