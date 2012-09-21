#!/bin/bash
#
# Name: lex.1.sh.
#
# Calls: lex.sh
#
# Parameters:
# 1: The abbreviated name of sample input and output data files.
#    E.g. 19 simultaneously means data/19.dot and data/19.lex.
# 2 & 3: Use for debugging. Eg -maxlevel debug.

scripts/lex.sh csv data/$1.dot data/$1.lex $2 $3
