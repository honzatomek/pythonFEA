#! /bin/bash
#
matlab -nodisplay -nosplash -nodesktop -batch \
  "run('hypercube_integrals_test.m');exit;" > hypercube_integrals_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
