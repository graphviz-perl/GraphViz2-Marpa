#!/usr/bin/env perl

use strict;
use warnings;

use File::Spec;

use GraphViz2::Marpa::Utils;

# -----------

my($data_dir_name) = 'data';
my($print)         = shift || 0;

my($dot_file);
my($lex_file);

for my $file_name (GraphViz2::Marpa::Utils -> new -> get_files($data_dir_name, 'gv') )
{
	$dot_file = File::Spec -> catfile($data_dir_name, "$file_name.gv");
	$lex_file = File::Spec -> catfile($data_dir_name, "$file_name.lex");

	print "$dot_file. \n" if ($print);

	`$^X -Ilib scripts/lex.pl -input_file $dot_file -lexed_file $lex_file`;
}
