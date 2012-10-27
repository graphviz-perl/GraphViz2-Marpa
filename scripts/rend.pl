#!/usr/bin/env perl

use strict;
use warnings;

use Getopt::Long;

use GraphViz2::Marpa::Renderer::GraphViz2;

use Pod::Usage;

# -----------------------------------------------

my($option_parser) = Getopt::Long::Parser -> new();

my(%option);

if ($option_parser -> getoptions
(
	\%option,
	'help',
	'maxlevel=s',
	'minlevel=s',
	'output_file=s',
	'parsed_file=s',
) )
{
	pod2usage(1) if ($option{'help'});

	exit GraphViz2::Marpa::Renderer::GraphViz2 -> new(%option) -> run;
}
else
{
	pod2usage(2);
}

__END__

=pod

=head1 NAME

rend.pl - Run GraphViz2::Marpa::Renderer::GraphViz2

=head1 SYNOPSIS

rend.pl [options]

	Options:
	-help
	-maxlevel logOption1
	-minlevel logOption2
	-output_file aRenderedOutputFileName
	-parsed_file aParsedOutputFileName

Exit value: 0 for success, 1 for failure. Die upon error.

=head1 OPTIONS

=over 4

=item -help

Print help and exit.

=item -maxlevel logOption1

This option affects Log::Handler.

See the Log::handler docs.

Default: 'notice'.

=item -minlevel logOption2

This option affects Log::Handler.

See the Log::handler docs.

Default: 'error'.

No lower levels are used.

=item -output_file aRenderedOutputFileName

Specify the name of a file to be used for output by the renderer.

See the distro for data/*.rend (output from the default renderer).

Default: ''.

The default means the file is not written.

=item -parsed_file aParsedOutputFileName

Specify the name of a CSV file of parsed tokens to read.

See the distro for data/*.parse.

Default: ''.

=back

=cut
