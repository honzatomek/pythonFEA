#! /bin/bash
#
matlab -nodisplay -nosplash -nodesktop -batch \
  "run('triangle01_monte_carlo_test.m');exit;" > triangle01_monte_carlo_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
