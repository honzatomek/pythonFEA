#! /bin/bash
#
cp triangle.h /$HOME/include
#
gcc -c -Wall -I/$HOME/include triangle.c
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv triangle.o ~/libc/triangle.o
#
echo "Normal end of execution."
