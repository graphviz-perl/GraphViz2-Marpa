#!/usr/bin/env perl

use strict;
use warnings;

use File::Spec;

use GraphViz2::Marpa::Utils;

# -----------

my($print)         = shift || 0;
my($data_dir_name) = 'data';

my($parse_file);
my($rend_file);

for my $file_name (GraphViz2::Marpa::Utils -> new -> get_files($data_dir_name, 'parse') )
{
	$parse_file = File::Spec -> catfile($data_dir_name, "$file_name.parse");
	$rend_file  = File::Spec -> catfile($data_dir_name, "$file_name.rend");

	print "$parse_file. \n" if ($print);

	`$^X -Ilib scripts/rend.pl -parsed_file $parse_file -output_file $rend_file`;
}
