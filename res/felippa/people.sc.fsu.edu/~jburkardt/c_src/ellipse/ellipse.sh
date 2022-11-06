#! /bin/bash
#
cp ellipse.h /$HOME/include
#
gcc -c -Wall ellipse.c
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv ellipse.o ~/libc/ellipse.o
#
echo "Normal end of execution."
