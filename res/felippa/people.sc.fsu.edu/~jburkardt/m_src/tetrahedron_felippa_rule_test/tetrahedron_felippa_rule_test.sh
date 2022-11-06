#! /bin/bash
#
matlab -nodisplay -nosplash -nodesktop -batch \
  "run('tetrahedron_felippa_rule_test.m');exit;" > tetrahedron_felippa_rule_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
