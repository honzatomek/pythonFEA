#! /bin/bash
#
matlab -nodisplay -nosplash -nodesktop -batch \
  "run('polygon_grid_test.m');exit;" > polygon_grid_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
