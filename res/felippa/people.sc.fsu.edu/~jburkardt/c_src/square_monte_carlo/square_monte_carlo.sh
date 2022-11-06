#! /bin/bash
#
cp square_monte_carlo.h /$HOME/include
#
gcc -c -Wall -I /$HOME/include square_monte_carlo.c
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv square_monte_carlo.o ~/libc/square_monte_carlo.o
#
echo "Normal end of execution."
