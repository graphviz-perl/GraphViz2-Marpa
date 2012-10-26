#!/bin/bash

perl -Ilib scripts/dot2rend.pl
perl -Ilib scripts/rend2svg.pl
perl -Ilib scripts/generate.demo.pl

cp html/*.html html/*.svg $DR/Perl-modules/html/graphviz2.marpa/

echo Check the version number in the demo index
