#! /bin/bash
#
cp hypersphere.hpp /$HOME/include
#
g++ -c -Wall -I/$HOME/include hypersphere.cpp
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv hypersphere.o ~/libcpp/hypersphere.o
#
echo "Normal end of execution."
