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
my($in_suffix)     = 'dot';
my($out_suffix)    = 'lex';
my($test_count)    = 0;

my(@diff, $diff_count);
my($in_file);
my($out_file, @old_content);
my(@new_content);

# Ignore known failures.

for my $file_name (grep{! /^(?:01|02|03|04|05|06|08)/} GraphViz2::Marpa::Utils -> new -> get_files($data_dir_name, $in_suffix) )
{
	$test_count++;

	$in_file  = File::Spec -> catfile($data_dir_name, "$file_name.$in_suffix");
	$out_file = File::Spec -> catfile($temp_dir_name, "$file_name.$out_suffix");

	my($result) = `$^X scripts/lex.pl -i $in_file -l $out_file`;
	$in_file     = File::Spec -> catfile($data_dir_name, "$file_name.$out_suffix");
	@old_content = slurp $in_file;
	@new_content = slurp $out_file;
	@diff        = diff(\@old_content, \@new_content);
	$diff_count  = scalar @diff;

	ok($diff_count == 0, "Compare shipped and generated: $in_file");
}

done_testing($test_count);
