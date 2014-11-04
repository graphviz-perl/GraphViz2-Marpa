#!/bin/bash

#for i in data/0*.gv data/1*.gv data/2*.gv data/3*.gv data/4*.gv data/5*.gv data/6*.gv ;
for i in data/0*.gv data/1*.gv data/2*.gv data/3*.gv data/4*.gv data/5*.gv ;
do
	scripts/g2m.sh $i

	if [ "$?" -eq "0" ]
	then
		echo OK. $i.
	else
		echo Fail. $i.
	fi
done
