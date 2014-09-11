#!/bin/bash

for i in data/08.gv data/09.gv data/1*.gv data/2*.gv data/3*.gv data/4*.gv data/5*.gv ;
do
	X=`basename $i .gv`

	perl -Ilib scripts/g2m.pl -in $i -out html/$X.svg

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

cp html/*.html html/*.svg $DR/$PM

echo Check the version number in the demo index
