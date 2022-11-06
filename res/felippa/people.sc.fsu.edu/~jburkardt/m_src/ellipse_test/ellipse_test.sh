#! /bin/bash
#
matlab -nodisplay -nosplash -nodesktop -batch \
  "run('ellipse_test.m');exit;" > ellipse_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
