#!/bin/bash

echo Contents of data/$1.gv:
cat data/$1.gv
echo ----------------------------

if [ "$1" == "57" ]
then
	echo Note: $1.gv takes 7 seconds
fi

scripts/g2m.sh data/$1.gv $2 $3 $4 $5
