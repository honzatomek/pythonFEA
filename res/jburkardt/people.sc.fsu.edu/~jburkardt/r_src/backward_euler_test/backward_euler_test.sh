#! /bin/bash
#
Rscript backward_euler_test.R > backward_euler_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."

