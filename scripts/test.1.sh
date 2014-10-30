#!/bin/bash

echo Contents of data/$1.gv:
cat data/$1.gv
echo ----------------------------

scripts/g2m.sh data/$1.gv $2 $3 $4 $5
