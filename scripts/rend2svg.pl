#!/usr/bin/env perl

use strict;
use warnings;

use File::Spec;

use GraphViz2::Marpa::Utils;

# -----------

my($print)         = shift || 0;
my($data_dir_name) = 'data';
my($html_dir_name) = 'html';

my($rend_file);
my($svg_file);

for my $file_name (GraphViz2::Marpa::Utils -> new -> get_files($data_dir_name, 'rend') )
{
	$rend_file = File::Spec -> catfile($data_dir_name, "$file_name.rend");
	$svg_file  = File::Spec -> catfile($html_dir_name, "$file_name.svg");

	print "$rend_file. \n" if ($print);

	`dot -Tsvg $rend_file > $svg_file`;
}
