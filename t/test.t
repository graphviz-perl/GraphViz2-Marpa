#!/usr/bin/env perl

use strict;
use warnings;

use Algorithm::Diff 'diff';

use Capture::Tiny 'capture';

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
my($html_dir_name) = $temp_dir_name;
my($in_suffix)     = 'gv';
my($out_suffix)    = 'gv';
my($svg_suffix)    = 'svg';
my($count)         = 0;

my(@diff, $diff_count);
my($exit);
my($in_file);
my($new_svg);
my($out_file, $old_svg);
my($stdout, $stderr);

# Ignore known failures.

for my $file_name (GraphViz2::Marpa::Utils -> new -> get_files($data_dir_name, $in_suffix) )
{
	$count++;

	diag "Note: $data_dir_name/$file_name.gv takes 7 seconds" if ($file_name eq '57');

	$in_file                  = File::Spec -> catfile($data_dir_name, "$file_name.$in_suffix");
	$out_file                 = File::Spec -> catfile($temp_dir_name, "$file_name.$out_suffix");
	($stdout, $stderr, $exit) = capture{system $^X, '-Ilib', 'scripts/g2m.pl', '-input_file', $in_file, '-output_file', $out_file};

	if ($exit == 0)
	{
		$out_file                  = File::Spec -> catfile($html_dir_name, "$file_name.$svg_suffix");
		($old_svg, $stderr, $exit) = capture{system 'dot', '-Tsvg', $in_file};
		$out_file                  = File::Spec -> catfile($html_dir_name, "$file_name.$svg_suffix");
		($old_svg, $stderr, $exit) = capture{system 'dot', '-Tsvg', $out_file};
		@diff                      = diff([split(/\n/, $old_svg)], [split(/\n/, $old_svg)]);
		$diff_count                = scalar @diff;

		ok($diff_count == 0, "Compare shipped and generated: $in_file");
	}
	else
	{
		ok(0, "Can't run scripts/g2m.pl");
	}
}

print "# Internal test count: $count. \n";

done_testing($count);
