#! /bin/bash
#
octave rk34_test.m > rk34_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
