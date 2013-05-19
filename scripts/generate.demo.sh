#!/bin/bash

perl -Ilib scripts/dot2rend.pl
perl -Ilib scripts/rend2svg.pl
perl -Ilib scripts/generate.demo.pl
perl -Ilib scripts/code.attributes2html.pl
perl -Ilib scripts/stt2html.pl

# $DR is my web server's doc root.

PM=Perl-modules/html/graphviz2.marpa

cp html/*.html html/*.svg $DR/$PM

echo Check the version number in the demo index
