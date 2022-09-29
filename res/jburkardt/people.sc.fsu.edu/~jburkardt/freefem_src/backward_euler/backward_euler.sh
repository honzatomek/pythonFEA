#! /bin/bash
#
/usr/local/bin/FreeFem++ -v 0 backward_euler.edp > backward_euler.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
ps2png backward_euler_10.ps backward_euler_10.png
ps2png backward_euler_20.ps backward_euler_20.png
ps2png backward_euler_40.ps backward_euler_40.png
rm *.ps
#
echo "Normal end of execution."
