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
