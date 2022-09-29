#! /bin/bash
#
octave flame_ode_test.m > flame_ode_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
