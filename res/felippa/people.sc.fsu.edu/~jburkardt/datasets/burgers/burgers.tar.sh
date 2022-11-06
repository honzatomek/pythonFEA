#!/bin/sh
#
#  Purpose:
#
#    Create a GZIP'ed TAR file of the datasets/burgers files.
#
#  Modified:
#
#    22 September 2006
#
#  Author:
#
#    John Burkardt
#
#  Move to the directory just above the "burgers" directory.
#
cd $HOME/public_html/datasets
#
#  Delete any TAR or GZ file in the directory.
#
echo "Remove TAR and GZ files."
rm burgers/*.tar
rm burgers/*.gz
#
#  Create a TAR file of the "burgers" directory.
#
echo "Create TAR file."
tar cvf burgers.tar burgers/*
#
#  Compress the file.
#
echo "Compress the TAR file."
gzip burgers.tar
#
#  Move the compressed file into the "burgers" directory.
#
echo "Move the compressed file into the directory."
mv burgers.tar.gz burgers
#
#  Say goodnight.
#
echo "The burgers gzip file has been created."
