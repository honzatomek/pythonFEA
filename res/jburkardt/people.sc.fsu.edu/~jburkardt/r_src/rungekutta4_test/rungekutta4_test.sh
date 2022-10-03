#! /bin/bash
#
Rscript rungekutta4_test.R > rungekutta4_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."

