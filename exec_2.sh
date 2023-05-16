#!/bin/sh
rm /tmp/pipe* -f
rm -f exit.txt
dune exec ./src/ex2_2.exe $1 $2 $3
