#!/bin/bash

perl -Ilib scripts/dot2rend.pl
perl -Ilib scripts/rend2svg.pl
perl -Ilib scripts/generate.demo.pl

# $DR is my web server's doc root.

PM=Perl-modules/html/graphviz2.marpa

cp html/*.html html/*.svg $DR/$PM

echo Check the version number in the demo index
