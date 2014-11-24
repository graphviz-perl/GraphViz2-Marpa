#!/usr/bin/env perl

use 5.018;
use strict;
use warnings;

use Path::Tiny; # path().

# -------------

for my $file (path($ENV{GV}) -> children)
{
	if (-s $file <= 10000)
	{
		`cp $file data`;
	}
}
