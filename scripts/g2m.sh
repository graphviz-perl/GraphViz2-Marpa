#!/bin/bash
#
# Name: g2m.sh.
#
# Parameters:
# 1: The abbreviated name of sample input and output data files.
#	E.g. 19 simultaneously means data/19.gv, data/19.lex and data/19.parse.
# 2 & 3: Use to run the default renderer. E.g.: -o data/19.rend.
#	Or, use to report the forest. E.g.: -report_forest 1
#	Or, use for debugging. E.g.: -maxlevel debug.

perl -Ilib scripts/g2m.pl -input_file data/$1.gv -lexed_file data/$1.lex -parsed_file data/$1.parse $2 $3 $4 $5
