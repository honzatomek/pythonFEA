#! /bin/bash
#
Rscript midpoint_test.R > midpoint_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."

