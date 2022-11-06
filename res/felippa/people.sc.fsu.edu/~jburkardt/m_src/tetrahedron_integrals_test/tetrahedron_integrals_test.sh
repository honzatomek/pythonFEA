#! /bin/bash
#
matlab -nodisplay -nosplash -nodesktop -batch \
  "run('tetrahedron_integrals_test.m');exit;" > tetrahedron_integrals_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
