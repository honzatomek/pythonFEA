#! /bin/bash
#
matlab -nodisplay -nosplash -nodesktop -batch \
  "run('laguerre_rule_test.m');exit;" > laguerre_rule_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
