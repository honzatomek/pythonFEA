#! /bin/bash
#
/usr/local/bin/FreeFem++ -v 0 euler.edp > euler.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
