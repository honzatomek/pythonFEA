#! /bin/bash
#
matlab -nodisplay -nosplash -nodesktop -batch \
  "run('circle_monte_carlo_test.m');exit;" > circle_monte_carlo_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
