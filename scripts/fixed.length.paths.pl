#!/usr/bin/env perl

use strict;
use warnings;

use Getopt::Long;

use GraphViz2::Marpa::Parser::TreeUtils;

use Pod::Usage;

# -----------------------------------------------

my($option_parser) = Getopt::Long::Parser -> new();

my(%option);

if ($option_parser -> getoptions
(
	\%option,
	'allow_loops=i',
	'help',
	'image_size=s',
	'lexed_file=s',
	'maxlevel=s',
	'minlevel=s',
	'output_file=s',
	'parsed_file=s',
	'path_length=i',
	'report_items=i',
	'report_paths=i',
	'start_node=s',
	'tree_file=s',
) )
{
	pod2usage(1) if ($option{'help'});

	exit GraphViz2::Marpa::Parser::TreeUtils -> new(%option) -> find_fixed_length_paths;
}
else
{
	pod2usage(2);
}

__END__

=pod

=head1 NAME

find.trees.pl - Run GraphViz2::Marpa::Parser::Tree.

=head1 SYNOPSIS

find.trees.pl [options]

	Options:
	-allow_loops $integer
	-help
	-image_size xInchesByyInches
	-lexed_file aLexedInputFileName
	-maxlevel logOption1
	-minlevel logOption2
	-output_file aRenderedOutputFileName
	-parsed_file aParsedOutputFileName
	-path_length $integer
	-report_items $Boolean
	-report_paths $Boolean
	-start_node aNodeName
	-tree_file anSVGFileName

Exit value: 0 for success, 1 for failure. Die upon error.

=head1 OPTIONS

=over 4

=item o -allow_loops $integer

Specify whether or not loops are allowed in the paths found.

Values for $integer:

=over 4

=item o 0 - Do not allow any loops

This is the default.

=item o 1 - Allow any node to be included once or twice.

=back

Default: 0.

=item o -help

Print help and exit.

=item o -image_size xInchesByyInches

Specify the size of the output image, in inches.

Default: 6x7.

=item o -lexed_file aLexedInputFileName

Specify the name of a CSV file of lexed tokens to read. This file was (probably) output by the lexer.

See the distro for data/*.lex.

Default: ''.

=item o -maxlevel logOption1

This option affects Log::Handler.

See the Log::handler docs.

Default: 'notice'.

=item o -minlevel logOption2

This option affects Log::Handler.

See the Log::handler docs.

Default: 'error'.

No lower levels are used.

=item o -output_file aRenderedOutputFileName

Specify the name of a file to be used for output by the renderer.

See the distro for data/*.rend (output from the default renderer).

Default: ''.

The default means the file is not written.

=item o -parsed_file aParsedOutputFileName

Specify the name of a CSV file of parsed tokens to write.

See the distro for data/*.parse.

Default: ''.

The default means the file is not written.

=item o -path_length $integer

The path length which all detected trees must have.

Defailt: 0.

=item o -report_items $Boolean

Log the items recognized by the parser.

Default: 0.

=item o -report_paths $Boolean

Log the paths detected.

Default: 0.

=item o -start_node aNodeName

The name of the node which all trees must start from.

Default: ''.

=item o -tree_file AnSVGFileName

Specify the name of an SVG file to write displaying the trees found.

Default: ''.

The default means the file is not written.

=back

=cut
