#! /bin/bash
#
/usr/local/bin/FreeFem++ -v 0 midpoint.edp > midpoint.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
ps2png midpoint_10.ps midpoint_10.png
ps2png midpoint_20.ps midpoint_20.png
ps2png midpoint_40.ps midpoint_40.png
rm *.ps
#
echo "Normal end of execution."
