#!/bin/bash

scripts/generate.svg.sh $1

perl -Ilib scripts/generate.demo.pl -author_tests $1

# $DR is my web server's doc root (in Debian's RAM disk).
# $PM is a directory path.

PM=Perl-modules/html/graphviz2.marpa

if [ "$1" == "1" ]
then
	PM="$PM/author_tests"
	DIR=xt/author/html
else
	DIR=html
fi

cp $DIR/* $DR/$PM             > /dev/null
cp $DIR/* ~/savage.net.au/$PM > /dev/null

echo
echo Check the version number in the demo index
