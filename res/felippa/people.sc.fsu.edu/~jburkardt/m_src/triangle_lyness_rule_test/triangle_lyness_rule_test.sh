#! /bin/bash
#
matlab -nodisplay -nosplash -nodesktop -batch \
  "run('triangle_lyness_rule_test.m');exit;" > triangle_lyness_rule_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
