#! /bin/bash
#
matlab -nodisplay -nosplash -nodesktop -batch \
  "run('ball_integrals_test.m');exit;" > ball_integrals_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
