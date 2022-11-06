#! /bin/bash
#
matlab -nodisplay -nosplash -nodesktop -batch \
  "run('legendre_rule_fast_test.m');exit;" > legendre_rule_fast_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
