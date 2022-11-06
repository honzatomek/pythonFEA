#! /bin/bash
#
cp polygon.hpp /$HOME/include
#
g++ -c -Wall -I/$HOME/include polygon.cpp
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv polygon.o ~/libcpp/polygon.o
#
echo "Normal end of execution."
