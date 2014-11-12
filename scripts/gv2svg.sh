#!/bin/bash
#
# Parameters:
# 1: The name of input and files.
#	16 means the input is data/16.gv, and the output is
#	$DR/Perl-modules/html/graphviz2.marpa/16.svg.
# $DR is my web server's doc root (in Debian's RAM disk).

echo In: data/$1.gv. Out: $DR/Perl-modules/html/graphviz2.marpa/$1.svg

dot -Tsvg data/$1.gv > $DR/Perl-modules/html/graphviz2.marpa/$1.svg
