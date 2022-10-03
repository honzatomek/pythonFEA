#! /bin/bash
#
gcc -c -Wall -I/$HOME/include three_body_ode.c
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gcc -o three_body_ode three_body_ode.o /$HOME/libc/rkf45.o -lm
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm three_body_ode.o
#
mv three_body_ode $HOME/binc
#
echo "Normal end of execution."
