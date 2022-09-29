#! /bin/bash
#
octave rkf45_test.m > rkf45_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
