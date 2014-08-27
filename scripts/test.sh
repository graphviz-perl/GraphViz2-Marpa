#!/bin/bash

for i in data/0*.gv data/1*.gv ; do
	echo $i
	scripts/g2m.sh $i -max notice

	if [ "$?" -eq "0" ]
	then
		echo Parse OK
	else
		echo Parse failed
	fi

	echo '-------------'
done

#for i in data/2*.gv; do
#	echo $i
#	scripts/g2m.sh $i
#	echo '-------------'
#done
