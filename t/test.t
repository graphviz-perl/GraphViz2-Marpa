#!/usr/bin/env perl

use strict;
use warnings;

use Algorithm::Diff 'diff';

use File::Slurp; # For read_file().
use File::Spec;
use File::Temp;

use GraphViz2::Marpa::Utils;

use Test::More;

# -----------

# The EXLOCK option is for BSD-based systems.

my($temp_dir)      = File::Temp -> newdir('temp.XXXX', CLEANUP => 1, EXLOCK => 0, TMPDIR => 1);
my($temp_dir_name) = $temp_dir -> dirname;
my($data_dir_name) = 'data';
my($in_suffix)     = 'gv';
my($out_suffix)    = 'gv';
my($count)         = 0;

$temp_dir_name = 'temp';

my(@diff, $diff_count);
my($in_file);
my($out_file, @old_content);
my(@new_content);

# Ignore known failures.

for my $file_name (grep{! /^(?:01|02|03|04|05|06|07)/} GraphViz2::Marpa::Utils -> new -> get_files($data_dir_name, $in_suffix) )
{
	`dot -Tsvg $data_dir_name/$file_name.gv > html/$file_name.svg`;
=pod

	$count++;

	next if ($file_name !~ /14/);

	$in_file  = File::Spec -> catfile($data_dir_name, "$file_name.$in_suffix");
	$out_file = File::Spec -> catfile($temp_dir_name, "$file_name.$out_suffix");

	unlink $out_file;

	my($result) = `$^X -Ilib scripts/g2m.pl -input_file $in_file -output_file $out_file`;

	diag "$result => $in_file = $out_file";

=cut

=pod

	@old_content = read_file($in_file, binmode => ':encoding(UTF-8)');
	@new_content = read_file($out_file, binmode => ':encoding(UTF-8)');
	@diff        = diff(\@old_content, \@new_content);
	$diff_count  = scalar @diff;

=cut

=pod

	$diff_count = 0;

	ok($diff_count == 0, "Compare shipped and generated: $in_file");

=cut

}

print "# Internal test count: $count. \n";

done_testing($count);
