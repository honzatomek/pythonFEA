#! /bin/bash
#
matlab -nodisplay -nosplash -nodesktop -batch \
  "run('toms886_test.m');exit;" > toms886_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
