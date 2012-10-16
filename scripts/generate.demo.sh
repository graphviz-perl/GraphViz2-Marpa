#!/bin/bash

perl scripts/dot2rend.pl
perl scripts/rend2svg.pl
perl scripts/generate.index.pl > html/index.html

cp html/*.html html/*.svg $DR/Perl-modules/html/graphviz2.marpa/
