#! /bin/bash
#
matlab -nodisplay -nosplash -nodesktop -batch \
  "run('wedge_monte_carlo_test.m');exit;" > wedge_monte_carlo_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."