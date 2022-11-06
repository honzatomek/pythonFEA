#! /bin/bash
#
matlab -nodisplay -nosplash -nodesktop -batch \
  "run('jacobi_polynomial_test.m');exit;" > jacobi_polynomial_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
