#! /bin/bash
#
matlab -nodisplay -nosplash -nodesktop -batch \
  "run('triangulation_svg_test.m');exit;" > triangulation_svg_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
