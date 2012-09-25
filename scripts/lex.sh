#!/bin/bash
#
# Name: lex.sh.
#
# Called by: lex.1.sh.
#
# Parameters:
# 1: The type of STT file, e.g. csv.
#    This parameter is not currently used. See the commented-out line below.
# 2: The name of a dot input file.
# 3: The name of a lexed output file.
# 4 & 5: Use for debugging. Eg -maxlevel debug.

echo Contents of $2:
cat $2
echo ----------------------------
rm -rf $3
echo Output of lexer:
#perl -Ilib scripts/lex.pl -i $2 -lexed_file $3 -stt data/default.stt.$1 -type $1 $4 $5
perl -Ilib scripts/lex.pl -i $2 -lexed_file $3 $4 $5
echo ----------------------------
echo Contents of $3:
cat $3
echo ----------------------------

echo Contents of $2:
cat $2
echo ----------------------------
