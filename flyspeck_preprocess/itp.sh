#!/bin/bash

inp=$1
out=$2

mkdir -p $out

parallel --resume --joblog log --timeout 600 ./dr.sh ::: $inp/*.smt2 ::: $out
