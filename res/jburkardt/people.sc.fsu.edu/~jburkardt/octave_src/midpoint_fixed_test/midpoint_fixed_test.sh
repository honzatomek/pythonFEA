#! /bin/bash
#
octave midpoint_fixed_test.m > midpoint_fixed_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
