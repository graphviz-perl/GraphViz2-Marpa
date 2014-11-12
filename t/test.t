#!/usr/bin/env perl

use strict;
use warnings;

use GraphViz2::Marpa::Utils;

use Test::More;

# -----------

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

my($count)         = 0;
my($data_dir_name) = 'data';
my($in_suffix)     = 'gv';
my($utils)         = GraphViz2::Marpa::Utils -> new;

my($diff, $diff_count);
my($message);

for my $file_name ($utils -> get_files($data_dir_name, $in_suffix) )
{
	$count++;

	$diff       = $utils -> generate_test_files($file_name);
	$diff_count = 0;
	$message    = $will_fail{$file_name}
					? "Known and expected failure : $file_name"
					: "Tests shipped and generated: $file_name";

	$diff -> Base(1); # Return line numbers, not indices.

	while ($diff -> Next() )
	{
		next if ($diff -> Same);

		$diff_count++;

		my($sep) = '';

		if(! $diff -> Items(2) )
		{
			printf "# %d,%dd%d\n", $diff -> Get(qw(Min1 Max1 Max2) );
		}
		elsif (! $diff -> Items(1) )
		{
			printf "# %da%d,%d\n", $diff -> Get(qw(Max1 Min2 Max2) );
		}
		else
		{
			$sep = "---\n";

			printf "# %d,%dc%d,%d\n", $diff -> Get(qw(Min1 Max1 Min2 Max2) );
		}

        print "# < $_" for $diff -> Items(1);
        print "# $sep";
        print "# > $_" for $diff -> Items(2);
    }
#	diag "diff_count: $diff_count. message: $message";

	ok($diff_count == 0, $message);
}

print "# Internal test count: $count. \n";

done_testing($count);
