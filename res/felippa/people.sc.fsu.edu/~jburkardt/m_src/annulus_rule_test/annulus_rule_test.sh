#! /bin/bash
#
matlab -nodisplay -nosplash -nodesktop -batch \
  "run('annulus_rule_test.m');exit;" > annulus_rule_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
