#! /bin/bash
#
octave trapezoidal_test.m > trapezoidal_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."