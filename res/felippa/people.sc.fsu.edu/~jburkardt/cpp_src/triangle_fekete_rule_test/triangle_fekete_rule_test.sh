#! /bin/bash
#
g++ -c -Wall -I/$HOME/include triangle_fekete_rule_test.cpp
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
g++ triangle_fekete_rule_test.o /$HOME/libcpp/triangle_fekete_rule.o -lm
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
#
rm triangle_fekete_rule_test.o
#
mv a.out triangle_fekete_rule_test
./triangle_fekete_rule_test > triangle_fekete_rule_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm triangle_fekete_rule_test
#
ps2png fekete_rule_1.eps fekete_rule_1.png
ps2png fekete_rule_2.eps fekete_rule_2.png
ps2png fekete_rule_3.eps fekete_rule_3.png
ps2png fekete_rule_4.eps fekete_rule_4.png
ps2png fekete_rule_5.eps fekete_rule_5.png
ps2png fekete_rule_6.eps fekete_rule_6.png
ps2png fekete_rule_7.eps fekete_rule_7.png
rm *.eps
#
echo "Normal end of execution."
