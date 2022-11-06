#! /bin/bash
#
cp tetrahedron.h /$HOME/include
#
gcc -c -Wall -I/$HOME/include tetrahedron.c
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv tetrahedron.o ~/libc/tetrahedron.o
#
echo "Normal end of execution."
