#! /bin/bash
#
octave rk4_test.m > rk4_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
