#! /bin/bash
#
cp hypersphere.h /$HOME/include
#
gcc -c -Wall -I/$HOME/include hypersphere.c
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv hypersphere.o ~/libc/hypersphere.o
#
echo "Normal end of execution."
