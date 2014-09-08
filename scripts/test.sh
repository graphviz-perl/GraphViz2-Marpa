#!/bin/bash

for i in data/0*.gv data/1*.gv data/2*.gv data/3*.gv data/4*.gv data/5*.gv data/9*.gv ;
do

	if [ "$i" == "data/08.gv" ]
	then
		echo Expect data/08.gv to be the first to succeed.
	fi

	scripts/g2m.sh $i -max notice

	if [ "$?" -eq "0" ]
	then
		echo OK. $i.
	else
		echo Fail. $i.
	fi
done
