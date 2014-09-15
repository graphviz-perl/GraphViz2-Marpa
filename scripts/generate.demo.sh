#!/bin/bash

for i in data/*.gv ;
do
	X=`basename $i .gv`

	dot -Tsvg $i > html/$X.svg

	if [ "$?" -eq "0" ]
	then
		echo OK. $i.
	else
		echo Fail. $i.
	fi
done

perl -Ilib scripts/generate.demo.pl

# $DR is my web server's doc root.

PM=Perl-modules/html/graphviz2.marpa

cp html/* $DR/$PM
cp html/* ~/savage.net.au/$PM

echo Check the version number in the demo index
