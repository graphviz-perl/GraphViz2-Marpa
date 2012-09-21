#!/bin/bash
#
# name: rend.sh.
#
# Called by: parse.1.sh.
#
# Parameters:
# 1: The name of a parsed input file.
# 2: The name of a rendered output file.
# 3 & 4: Use for debugging. Eg -maxlevel debug

echo Contents of $1:
cat $1
echo ----------------------------
rm -rf $2
echo Output of renderer:
perl -Ilib scripts/rend.pl -p $1 -o $2 $3 $4
echo ----------------------------
echo Contents of $1:
cat $1
echo ----------------------------
echo Contents of $2:
cat $2
