#!/bin/bash

perl scripts/dot2rend.pl
perl scripts/rend2svg.pl
perl scripts/generate.demo.pl

cp html/*.html html/*.svg $DR/Perl-modules/html/graphviz2.marpa/

echo Check the version number in the demo index
