#! /bin/bash
#
matlab -nodisplay -nosplash -nodesktop -batch \
  "run('hypercube_monte_carlo_test.m');exit;" > hypercube_monte_carlo_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."