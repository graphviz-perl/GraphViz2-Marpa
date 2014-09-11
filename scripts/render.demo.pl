#!/usr/bin/env perl

use feature qw/say unicode_strings/;
use open qw(:std :utf8);
use strict;
use warnings;
use warnings qw(FATAL utf8);

use Getopt::Long;

use GraphViz2::Marpa;

use Pod::Usage;

# -----------------------------------------------

my($option_parser) = Getopt::Long::Parser -> new();

my(%option);

if ($option_parser -> getoptions
(
	\%option,
	'description=s',
	'help',
	'input_file=s',
	'maxlevel=s',
	'minlevel=s',
	'output_file=s',
) )
{
	pod2usage(1) if ($option{'help'});

	exit GraphViz2::Marpa -> new(%option) -> run;
}
else
{
	pod2usage(2);
}

__END__

=pod

=head1 NAME

gem.pl - Run GraphViz2::Marpa and the default rendering engine.

=head1 SYNOPSIS

gem.pl [options]

	Options:
	-description graphDescription
	-help
	-input_file aDotInputFileName
	-maxlevel logOption1
	-minlevel logOption2
	-output_file aRenderedOutputFileName

Exit value: 0 for success, 1 for failure. Die upon error.

=head1 OPTIONS

=over 4

=item -description graphDescription

Read the DOT-style graph definition from the command line.

You are strongly encouraged to surround this string with '...' to protect it from your shell.

See also the -input_file option to read the description from a file.

The -description option takes precedence over the -input_file option.

Default: ''.

=item -help

Print help and exit.

=item -input_file aDotInputFileName

Read the DOT-style graph definition from a file.

See also the -description option to read the graph definition from the command line.

The -description option takes precedence over the -input_file option.

Default: ''.

See the distro for data/*.gv.

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

Specify the name of a file for the renderer to write.

Default: ''.

The default means the file is not written.

=back

=cut
