#!/bin/bash
#
# Parameters:
# 1: The name of input and files.
#	16 means the input is data/16.gv, and the output is
#	$DR/Perl-modules/html/graphviz2.marpa/16.svg.
# 2+: Any parameters accepted by scripts/g2m.sh, i.e. scripts/g2m.pl.
# $DR is my web server's doc root (in Debian's RAM disk).

echo Contents of data/$1.gv:
cat data/$1.gv
echo ----------------------------

scripts/gv2svg.sh $1

scripts/g2m.sh data/$1.gv $2 $3 $4 $5 $6 $7 $8 $9

dot -Tsvg data/$1.gv > $DR/Perl-modules/html/graphviz2.marpa/$1.new.svg

echo Diff: $DR/Perl-modules/html/graphviz2.marpa/$1.svg $DR/Perl-modules/html/graphviz2.marpa/$1.new.svg

diff $DR/Perl-modules/html/graphviz2.marpa/$1.svg $DR/Perl-modules/html/graphviz2.marpa/$1.new.svg
