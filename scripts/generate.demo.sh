#!/bin/bash

PM=Perl-modules/html/graphviz2.marpa

for author_tests in 0 1
do
	scripts/generate.svg.sh $author_tests

	perl -Ilib scripts/generate.demo.pl -author_tests $author_tests

	# $DR is my web server's doc root (in Debian's RAM disk).
	# $PM is a directory path.

	if [ "$author_tests" == "1" ]
	then
		PM="$PM/author_tests"
		DIR=xt/author/html
	else
		DIR=html
	fi

	cp $DIR/* $DR/$PM             > /dev/null
	cp $DIR/* ~/savage.net.au/$PM > /dev/null

done

echo Check the version number in the demo index
