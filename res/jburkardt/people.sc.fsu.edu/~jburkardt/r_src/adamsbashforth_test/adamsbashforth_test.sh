#! /bin/bash
#
Rscript adamsbashforth_test.R > adamsbashforth_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."

