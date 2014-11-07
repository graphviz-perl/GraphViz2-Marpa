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
my($message);
my($new_svg);
my($out_file, $old_svg);
my($stdout, $stderr);

# Allow for known failures.
# The key '01' means input file data/01.gv, etc.

my(%will_fail) =
(
	'01'    => 1,
	'02'    => 1,
	'03'    => 1,
	'04'    => 1,
	'05'    => 1,
	'06'    => 1,
	'07'    => 1,
	'42.02' => 1,
	'42.04' => 1,
	'42.05' => 1,
	'42.06' => 1,
	'42.08' => 1,
	'42.09' => 1,
	'42.10' => 1,
	'42.11' => 1,
	'42.12' => 1,
);

for my $file_name (GraphViz2::Marpa::Utils -> new -> get_files($data_dir_name, $in_suffix) )
{
	$count++;

	diag "Note: $data_dir_name/$file_name.gv takes 7 seconds" if ($file_name eq '57');

	$in_file                  = File::Spec -> catfile($data_dir_name, "$file_name.$in_suffix");
	$out_file                 = File::Spec -> catfile($temp_dir_name, "$file_name.$out_suffix");
	($stdout, $stderr, $exit) = capture{system $^X, '-Ilib', 'scripts/g2m.pl', '-input_file', $in_file, '-output_file', $out_file};

	$out_file                  = File::Spec -> catfile($html_dir_name, "$file_name.$svg_suffix");
	($old_svg, $stderr, $exit) = capture{system 'dot', '-Tsvg', $in_file};
	$out_file                  = File::Spec -> catfile($html_dir_name, "$file_name.$svg_suffix");
	($old_svg, $stderr, $exit) = capture{system 'dot', '-Tsvg', $out_file};
	@diff                      = diff([split(/\n/, $old_svg)], [split(/\n/, $old_svg)]);
	$diff_count                = scalar @diff;
	$message                   = ($will_fail{$file_name})
									? "Known and expected failure : $in_file"
									: "Tests shipped and generated: $in_file";

	ok($diff_count == 0, $message);
}

print "# Internal test count: $count. \n";

done_testing($count);
