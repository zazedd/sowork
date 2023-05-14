#!/bin/sh
rm /tmp/pipe* -f
dune exec ./src/ex2.exe $1 $2 $3 $4
