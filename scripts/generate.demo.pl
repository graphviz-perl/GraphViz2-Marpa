#!/usr/bin/env perl

use feature qw/say unicode_strings/;
use open qw(:std :utf8);
use strict;
use warnings;
use warnings qw(FATAL utf8);

use Getopt::Long;

use GraphViz2::Marpa::Demo;

use Pod::Usage;

# -----------------------------------------------

my($option_parser) = Getopt::Long::Parser -> new();

my(%option);

if ($option_parser -> getoptions
(
	\%option,
	'help',
) )
{
	pod2usage(1) if ($option{'help'});

	exit GraphViz2::Marpa::Demo -> new(%option) -> generate_demo_index;
}
else
{
	pod2usage(2);
}

__END__

=pod

=head1 NAME

generate.demo.pl - Generate GraphViz2::Marpa's html/index.html.

=head1 SYNOPSIS

generate.demo.pl [options]

	Options:
	-help

Exit value: 0 for success, 1 for failure. Die upon error.

=head1 OPTIONS

=over 4

=item o -help

Print help and exit.

=back

=cut
