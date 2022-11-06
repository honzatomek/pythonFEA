#! /bin/bash
#
matlab -nodisplay -nosplash -nodesktop -batch \
  "run('quadrilateral_test.m');exit;" > quadrilateral_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
