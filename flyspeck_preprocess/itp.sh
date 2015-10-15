#!/bin/bash

inp=$1
out=$2

mkdir -p $out

parallel -j 8 --nice 10 --noswap --resume --joblog log --timeout 600 ./dr.sh ::: $inp/*.smt2 ::: $out
