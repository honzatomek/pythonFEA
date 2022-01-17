#!/bin/bash

# # Set up the its-pointless repository according to https://github.com/its-pointless/gcc_termux
# # Eventually remove previous NumPy and SciPy installations
# apt remove --purge numpy scipy
# # Install and setup build tools
# apt install python build-essential clang gcc-11
# setupclang-gfort-11
# # install blas and set environment variables
# apt install openblas
export BLAS=/data/data/com.termux/files/usr/lib/libblas.so
export LAPACK=/data/data/com.termux/files/usr/lib/liblapack.so
export CC=clang
export CPP=clang++
# Install wheel, NumPy and SciPy via pip
pip install wheel
pip install numpy
pip install scipy
