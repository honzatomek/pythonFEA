#! /bin/bash
#
matlab -nodisplay -nosplash -nodesktop -batch \
  "run('trapezoidal_explicit_test.m');exit;" > trapezoidal_explicit_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
