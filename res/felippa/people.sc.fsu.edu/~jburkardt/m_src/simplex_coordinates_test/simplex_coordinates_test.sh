#! /bin/bash
#
matlab -nodisplay -nosplash -nodesktop -batch \
  "run('simplex_coordinates_test.m');exit;" > simplex_coordinates_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
