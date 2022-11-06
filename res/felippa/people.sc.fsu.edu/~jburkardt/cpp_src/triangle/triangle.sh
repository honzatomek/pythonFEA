#! /bin/bash
#
cp triangle.hpp /$HOME/include
#
g++ -c -Wall -I /$HOME/include triangle.cpp
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv triangle.o ~/libcpp/triangle.o
#
echo "Normal end of execution."
