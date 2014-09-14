#!/bin/bash

for i in data/u*.gv ;
do
	X=`basename $i .gv`

	scripts/g2m.sh $i

	if [ "$?" -eq "0" ]
	then
		echo OK. Parsed gv: $i.

		#echo Render. In: $i. Out: /tmp/$X.gv

		scripts/render.sh $i /tmp/$X.gv

		dot -Tsvg $i > /tmp/$X.old.svg

		#echo Dot in: $i. Out: /tmp/$X.old.svg

		dot -Tsvg /tmp/$X.gv > /tmp/$X.new.svg

		#echo Dot out: /tmp/$X.gv. Out: /tmp/$X.new.svg

		diff /tmp/$X.old.svg /tmp/$X.new.svg

		if [ "$?" -eq "0" ]
		then
			echo OK. Rendered and diffed svgs: $i.
		else
			echo Fail. Rendered and diffed svgs: $i.
		fi
	else
		echo Fail. Parsed $i.
	fi

	echo ------------
done
