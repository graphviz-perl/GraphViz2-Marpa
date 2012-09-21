#!/usr/bin/env perl

use strict;
use warnings;

use Getopt::Long;

use GraphViz2::Marpa::Parser;

use Pod::Usage;

# -----------------------------------------------

my($option_parser) = Getopt::Long::Parser -> new();

my(%option);

if ($option_parser -> getoptions
(
 \%option,
 'help',
 'lexed_file=s',
 'maxlevel=s',
 'minlevel=s',
 'output_file=s',
 'parsed_file=s',
 'report_items=i',
) )
{
	pod2usage(1) if ($option{'help'});

	exit GraphViz2::Marpa::Parser -> new(%option) -> run;
}
else
{
	pod2usage(2);
}

__END__

=pod

=head1 NAME

parse.pl - Run GraphViz2::Marpa::Parser.

=head1 SYNOPSIS

parse.pl [options]

	Options:
	-help
	-lexed_file aLexedInputFileName
	-maxlevel logOption1
	-minlevel logOption2
	-output_file aRenderedOutputFileName
	-parsed_file aParsedOutputFileName
	-report_items $Boolean

Exit value: 0 for success, 1 for failure. Die upon error.

=head1 OPTIONS

=over 4

=item -help

Print help and exit.

=item -lexed_file aLexedInputFileName

Specify the name of a CSV file of lexed tokens to read. This file was (probably) output by the lexer.

See the distro for data/*.lex.

Default: ''.

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

Specify the name of a CSV file of parsed tokens to write.

See the distro for data/*.parse.

Default: ''.

The default means the file is not written.

=item -report_items $Boolean

Log the items recognized by the parser.

Default: 0.

=back

=cut
