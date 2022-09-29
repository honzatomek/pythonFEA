#! /bin/bash
#
octave midpoint_explicit_test.m > midpoint_explicit_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
