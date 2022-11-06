#! /bin/bash
#
matlab -nodisplay -nosplash -nodesktop -batch \
  "run('disk01_rule_test.m');exit;" > disk01_rule_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
