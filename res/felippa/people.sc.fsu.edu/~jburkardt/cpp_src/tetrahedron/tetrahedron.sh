#! /bin/bash
#
cp tetrahedron.hpp /$HOME/include
#
g++ -c -Wall -I /$HOME/include tetrahedron.cpp
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv tetrahedron.o ~/libcpp/tetrahedron.o
#
echo "Normal end of execution."
