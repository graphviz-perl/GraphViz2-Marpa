#!/bin/bash

perl -Ilib scripts/dot2rend.pl
perl -Ilib scripts/rend2svg.pl
perl -Ilib scripts/generate.demo.pl
perl -Ilib scripts/code.attributes2html.pl
perl -Ilib scripts/stt2html.pl

cp html/*.html html/*.svg $DR/Perl-modules/html/graphviz2.marpa/

echo Check the version number in the demo index
