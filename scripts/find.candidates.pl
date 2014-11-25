#!/usr/bin/env perl

use strict;
use warnings;

use File::Basename;

use Path::Iterator::Rule;

# -------------
# Phase 1: Get the files already seen.

my($rule) = Path::Iterator::Rule -> new;

my(%seen);

for my $file ($rule -> name(qr/\.gv$/) -> all('./xt/author/data') )
{
	$seen{basename($file, '.gv')} = 1;
}

# Phase 2: Get the files not already seen.

$rule     = Path::Iterator::Rule -> new;
my($next) = $rule -> name(qr/\.gv$/) -> size('< 10k') -> iter("$ENV{HOME}/Downloads/Graphviz/graphviz-2.38.0");

my($basename);

while (defined(my $file = $next -> () ) )
{
	# Skip special cases:
	# o Fake *.gv file.
	# o Already seen.

	$basename = basename($file, '.gv');

	next if ($seen{$basename} || ($file =~ m|tclpkg/gv/META.gv|) );

	print "$file\n";
}
