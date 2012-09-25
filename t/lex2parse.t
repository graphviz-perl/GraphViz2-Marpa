#!/usr/bin/env perl

use strict;
use warnings;

use Algorithm::Diff 'diff';

use File::Spec;
use File::Temp;

use GraphViz2::Marpa::Utils;

use Perl6::Slurp;

use Test::More;

# -----------

# The EXLOCK option is for BSD-based systems.

my($temp_dir)      = File::Temp -> newdir('temp.XXXX', CLEANUP => 1, EXLOCK => 0, TMPDIR => 1);
my($temp_dir_name) = $temp_dir -> dirname;
my($data_dir_name) = 'data';
my($in_suffix)     = 'lex';
my($out_suffix)    = 'parse';
my($test_count)    = 0;

my(@diff, $diff_count);
my($in_file);
my($out_file, @old_content);
my(@new_content);

for my $file_name (GraphViz2::Marpa::Utils -> new -> get_files($data_dir_name, $in_suffix) )
{
	$test_count++;

	$in_file  = File::Spec -> catfile($data_dir_name, "$file_name.$in_suffix");
	$out_file = File::Spec -> catfile($temp_dir_name, "$file_name.$out_suffix");

	`$^X scripts/parse.pl -lexed_file $in_file -parsed_file $out_file`;

	$in_file     = File::Spec -> catfile($data_dir_name, "$file_name.$out_suffix");
	@old_content = slurp $in_file;
	@new_content = slurp $out_file;
	@diff        = diff(\@old_content, \@new_content);
	$diff_count  = scalar @diff;

	ok($diff_count == 0, "Compare shipped and generated file: $in_file");
}

done_testing($test_count);
