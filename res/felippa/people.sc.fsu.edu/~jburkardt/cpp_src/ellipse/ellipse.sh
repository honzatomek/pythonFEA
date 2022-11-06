#! /bin/bash
#
cp ellipse.hpp /$HOME/include
#
g++ -c -Wall -I /$HOME/include ellipse.cpp
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv ellipse.o ~/libcpp/ellipse.o
#
echo "Normal end of execution."
