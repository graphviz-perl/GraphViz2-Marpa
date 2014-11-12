#!/bin/bash

scripts/generate.svg.sh

perl -Ilib scripts/generate.demo.pl

# $DR is my web server's doc root (in Debian's RAM disk).
# $PM is a directory path.

PM=Perl-modules/html/graphviz2.marpa

cp html/* $DR/$PM
cp html/* ~/savage.net.au/$PM

echo Check the version number in the demo index
