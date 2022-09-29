#! /bin/bash
#
Rscript trapezoidal_test.R > trapezoidal_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."

