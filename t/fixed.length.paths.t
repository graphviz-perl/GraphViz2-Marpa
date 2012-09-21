#!/usr/bin/env perl

use strict;
use warnings;

use Capture::Tiny 'capture';

use GraphViz2::Marpa::Parser::TreeUtils;

use Test::More;

# -------------

sub run
{
	GraphViz2::Marpa::Parser::TreeUtils -> new
	(
		lexed_file   => 'data/90.Petersen.lex',
		report_paths => 1,
		start_node   => 5,
		path_length  => 3,
	) -> fixed_length_paths;
}

# -------------

my($stdout, $stderr) = capture \&run;
my($expected)        = <<'EOS';
Paths of length 3 starting from node 5:
5 - 8 - 6 - 9
5 - 8 - 6 - 1
5 - 8 - 3 - 4
5 - 8 - 3 - 2
5 - 7 - 9 - 6
5 - 7 - 9 - 4
5 - 7 - 2 - 3
5 - 7 - 2 - 1
5 - 0 - 4 - 9
5 - 0 - 4 - 3
5 - 0 - 1 - 6
5 - 0 - 1 - 2
Solution count: 12
EOS

ok($stdout eq $expected);

my($test_count) = 1;

done_testing($test_count);

__END__
