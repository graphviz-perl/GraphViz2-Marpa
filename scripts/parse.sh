#!/bin/bash
#
# name: parse.sh.
#
# Called by: parse.1.sh.
#
# Parameters:
# 1: The name of a dot input file.
# 2: The name of a lexed output file.
# 3: The name of a parsed output file.
# 4 & 5: Use for debugging. Eg -maxlevel debug or -report_forest 1

echo Contents of $2:
cat $2
echo ----------------------------
rm -rf $3
echo Output of parser:
perl -Ilib scripts/parse.pl -lexed_file $2 -report_items 1 -parsed_file $3 $4 $5
echo ----------------------------
echo Contents of $1:
cat $1
echo ----------------------------
echo Contents of $3:
cat $3
