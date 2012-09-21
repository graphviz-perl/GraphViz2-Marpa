#!/bin/bash
#
# Name: g2m.sh.
#
# Parameters:
# 1: The abbreviated name of sample input and output data files.
#    E.g. 19 simultaneously means data/19.dot, data/19.lex and data/19.parse.
# 2 & 3: Use to run the default renderer. E.g.: -o data/19.rend.
#    Or, use for debugging. E.g.: -maxlevel debug.

perl -Ilib scripts/g2m.pl -i data/$1.dot -l data/$1.lex -p data/$1.parse $2 $3
