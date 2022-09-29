#! /bin/bash
#
octave backward_euler_test.m > backward_euler_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
