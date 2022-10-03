#! /bin/bash
#
octave predator_prey_ode_test.m > predator_prey_ode_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
