#! /bin/bash
#
gfortran -c -Wall polygon_grid.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv polygon_grid.o ~/lib/polygon_grid.o
#
echo "Normal end of execution."
