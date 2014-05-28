package GraphViz2::Marpa;

use strict;
use warnings;

use GraphViz2::Marpa::Lexer;
use GraphViz2::Marpa::Parser;

use Hash::FieldHash ':all';

use Log::Handler;

fieldhash my %description   => 'description';
fieldhash my %input_file    => 'input_file';
fieldhash my %lexed_file    => 'lexed_file';
fieldhash my %lexer         => 'lexer';
fieldhash my %logger        => 'logger';
fieldhash my %maxlevel      => 'maxlevel';
fieldhash my %minlevel      => 'minlevel';
fieldhash my %output_file   => 'output_file';
fieldhash my %parsed_file   => 'parsed_file';
fieldhash my %parser        => 'parser';
fieldhash my %renderer      => 'renderer';
fieldhash my %report_forest => 'report_forest';
fieldhash my %report_items  => 'report_items';
fieldhash my %report_stt    => 'report_stt';
fieldhash my %stt_file      => 'stt_file';
fieldhash my %timeout       => 'timeout';
fieldhash my %type          => 'type';

our $VERSION = '1.13';

# --------------------------------------------------

sub _init
{
	my($self, $arg)      = @_;
	$$arg{description}   ||= ''; # Caller can set.
	$$arg{input_file}    ||= ''; # Caller can set.
	$$arg{lexed_file}    ||= ''; # Caller can set.
	$$arg{lexer}         = '';
	$$arg{logger}        = defined($$arg{logger}) ? $$arg{logger} : undef; # Caller can set.
	$$arg{maxlevel}      ||= 'notice'; # Caller can set.
	$$arg{minlevel}      ||= 'error';  # Caller can set.
	$$arg{output_file}   ||= '';       # Caller can set.
	$$arg{parsed_file}   ||= '';       # Caller can set.
	$$arg{parser}        = '';
	$$arg{renderer}      ||= '';       # Caller can set.
	$$arg{report_forest} ||= 0;        # Caller can set.
	$$arg{report_items}  ||= 0;        # Caller can set.
	$$arg{report_stt}    ||= 0;        # Caller can set.
	$$arg{stt_file}      ||= '';       # Caller can set.
	$$arg{timeout}       ||= 10;       # Caller can set.
	$$arg{type}          ||= '';       # Caller can set.
	$self                = from_hash($self, $arg);

	if (! defined $self -> logger)
	{
		$self -> logger(Log::Handler -> new);
		$self -> logger -> add
		(
		 screen =>
		 {
			 maxlevel       => $self -> maxlevel,
			 message_layout => '%m',
			 minlevel       => $self -> minlevel,
		 }
		);
	}

	return $self;

} # End of _init.

# --------------------------------------------------

sub log
{
	my($self, $level, $s) = @_;
	$level ||= 'debug';
	$s     ||= '';

	$self -> logger -> $level($s);

} # End of log.

# --------------------------------------------------

sub new
{
	my($class, %arg) = @_;
	my($self)        = bless {}, $class;
	$self            = $self -> _init(\%arg);

	return $self;

}	# End of new.

# --------------------------------------------------

sub run
{
	my($self)  = @_;

	$self -> lexer
	(GraphViz2::Marpa::Lexer -> new
		(
		 description  => $self -> description,
		 input_file   => $self -> input_file,
		 lexed_file   => $self -> lexed_file,
		 logger       => $self -> logger,
		 maxlevel     => $self -> maxlevel,
		 minlevel     => $self -> minlevel,
		 report_items => $self -> report_items,
		 report_stt   => $self -> report_stt,
		 stt_file     => $self -> stt_file,
		 timeout      => $self -> timeout,
		 type         => $self -> type,
		)
	);

	# Return 0 for success and 1 for failure.

	my($result) = $self -> lexer -> run;

	if ($result == 0)
	{
		$self -> parser
		(GraphViz2::Marpa::Parser -> new
			(
			 lexed_file    => $self -> lexed_file,
			 logger        => $self -> logger,
			 maxlevel      => $self -> maxlevel,
			 minlevel      => $self -> minlevel,
			 output_file   => $self -> output_file,
			 parsed_file   => $self -> parsed_file,
			 renderer      => $self -> renderer,
			 report_items  => $self -> report_items,
			 report_forest => $self -> report_forest,
			 tokens        => $self -> lexer -> items,
			)
		);

		$result = $self -> parser -> run;
	}
	else
	{
		$self -> log(warn => 'The lexer failed. The parser will not be run') if ($self -> logger);
	}

	# Return 0 for success and 1 for failure.

	return $result;

} # End of run.

# --------------------------------------------------

1;

=pod

=head1 NAME

GraphViz2::Marpa - A Perl lexer and parser for Graphviz dot files

=head1 Synopsis

=over 4

=item o Display help

	perl scripts/lex.pl   -h
	perl scripts/parse.pl -h
	perl scripts/g2m.pl   -h

=item o Run the lexer

	perl scripts/lex.pl -input_file x.gv -lexed_file x.lex

	x.gv is a Graphviz dot file. x.lex will be a CSV file of lexed tokens.

=item o Run the parser without running the lexer or the default renderer

	perl scripts/parse.pl -lexed_file x.lex -parsed_file x.parse

	x.parse will be a CSV file of parsed tokens.

=item o Run the parser and the default renderer

	perl scripts/parse.pl -lexed_file x.lex -parsed_file x.parse -output_file x.rend

	x.rend will be a Graphviz dot file.

=item o Run the lexer, parser and default renderer

	perl scripts/g2m.pl -input_file x.gv -lexed_file x.lex -parsed_file x.parse -output_file x.rend

=back

See also L</Scripts>.

=head1 Description

L<GraphViz2::Marpa> provides an L<Set::FA::Element>-based lexer and a L<Marpa::XS>-based parser for L<Graphviz|http://www.graphviz.org/> (dot) graph definitions.

Both the lexer and the parser must be run to parse the dot file. See L</Scripts> for sample code.

Demo lexer/parser output: L<http://savage.net.au/Perl-modules/html/graphviz2.marpa/index.html>.

State Transition Table: L<http://savage.net.au/Perl-modules/html/graphviz2.marpa/stt.html>.

Command line options and object attributes: L<http://savage.net.au/Perl-modules/html/graphviz2.marpa/code.attributes.html>.

My article on this set of modules: L<http://www.perl.com/pub/2012/10/an-overview-of-lexing-and-parsing.html>.

The Marpa grammar as an image: L<http://savage.net.au/Ron/html/graphviz2.marpa/Marpa.Grammar.svg>. This image was created
with L<Graphviz|http://www.graphviz.org/> via L<GraphViz2>.

=head1 Modules

=over 4

=item o L<GraphViz2::Marpa>

The current module, which documents the set of modules.

It uses L<GraphViz2::Marpa::Lexer> and L<GraphViz2::Marpa::Parser>. The latter can, optionally, use the default renderer L<GraphViz2::Marpa::Renderer::GraphViz2>.

See scripts/g2m.pl.

=item o L<GraphViz2::Marpa::Lexer>

The lexer. The real work is done by L<GraphViz2::Marpa::Lexer::DFA>.

Processes a L<Graphviz|http://www.graphviz.org/> (dot) graph definition and builds a data structure representing the lexed graph. It can output that data, via RAM or a CSV file,
which can then be read by the parser L<GraphViz2::Marpa::Parser>.

See scripts/lex.pl and scripts/g2m.pl.

=item o L<GraphViz2::Marpa::Lexer::DFA>

Called by L<GraphViz2::Marpa::Lexer> to run the DFA (Discrete Finite Automaton).

Wraps L<Set::FA::Element>, which is what actually lexes the input L<Graphviz|http://www.graphviz.org/> (dot) graph definition.

=item o L<GraphViz2::Marpa::Parser>

The parser. Accepts a L<Graphviz|http://www.graphviz.org/> (dot) graph definition in the lexed format and builds a similar data structure representing the parsed graph.
It can output that data, via RAM or a CSV file, which can then be read by the default renderer, L<GraphViz2::Marpa::Renderer::GraphViz2>.

See scripts/parse.pl and scripts/g2m.pl.

=item o L<GraphViz2::Marpa::Renderer::GraphViz2>

The default renderer. Optionally called by the parser.

=item o L<GraphViz2::Marpa::Utils>

Auxiliary code.

=back

=head1 Sample Data

=over 4

=item o Input files: data/*.gv

These are L<Graphviz|http://www.graphviz.org/> (dot) graph definition files.

Note 1: Some data/*.gv files contain I<serious> deliberate mistakes (from the point of view of L<Graphviz|http://www.graphviz.org/>), but they helped with writing the code.

Specifically, they are data/(01, 02, 03, 04, 05, 06, 08).gv. Natually, they do not produce output files data/*.lex, data/*.parse, data/*.rend or html/*.svg.

Note 2: Some data/*.gv files contain I<slight> deliberate mistakes, which do not stop production of output files. They do, however, cause various warning messages to be printed
when certain scripts are run.

=item o Output files: data/*.lex, data/*.parse, data/*.rend and html/*.svg

The data/*.lex and data/*.parse are CSV files.

The data/*.rend are L<Graphviz|http://www.graphviz.org/> (dot) graph definition files output by the default renderer.

The round trip shows that the lex/parse process does not lose information along the way, but comments are discarded. See L<GraphViz2::Marpa::Lexer/How does the lexer handle comments?>.

The html/*.svg files are output by 'dot'.

=item o Data for the State Transition Table

See data/stt.csv (CSV file) and html/stt.html.

Also, data/stt.csv has been incorporated into the source code of L<GraphViz2::Marpa::Lexer>.

The CSV file was converted to HTML with scripts/stt2html.pl.

=item o Documentation for the command line options and object attributes

See data/code.attributes.csv (CSV file) and data/code.attributes.html.

The CSV file was converted to HTML with scripts/code.attributes2html.pl.

=back

=head1 Scripts

These are in the scripts/ directory.

=over 4

=item o code.attributes2html.pl

Generate both data/code.attributes.csv and data/code.attributes.html.

=item o dot2lex.pl

Convert all data/*.gv files to data/*.lex using lex.pl.

=item o dot2rend.pl

Convert all data/*.gv files to data/*.lex and data/*.parse and data/*.rend using lex.pl and parse.pl.

=item o find.config.pl

Print the path to the config file, as determined by L<File::ShareDir>'s dist_file().

=item o g2m.pl

Run the lexer, and then run the parser on the output of the lexer. Try running with -h.

=item o g2m.sh

Simplifies running g2m.pl.

=item o generate.demo.sh

Runs dot2rend.pl, rend2svg.pl and generate.index.pl.

Then runs code.attributes2html.pl and stt2html.pl.

Then it copies html/*.html and html/*.svg to my web server's doc root, $DR/Perl-modules/html/graphviz2.marpa/.

=item o generate.index.pl

Generates html/index.html from data/*.gv and html/*.svg.

=item o lex.1.sh

Simplifies running lex.sh.

=item o lex.parse.sh

Simplifies running g2m.sh, and ensures the *.svg version of the tested data file is up-to-date.

=item o lex.pl

Run the lexer. Try running with -h.

=item o lex.sh

Simplifies running lex.pl.

=item o lex2parse.pl

Convert all data/*.lex to data/*.parse using parse.pl.

=item o lex2rend.pl

Convert all data/*.lex to data/*.parse and data/*.rend using parse.pl.

=item o parse.1.sh

Simplifies running parse.sh.

=item o parse.pl

Run the parser. Try running with -h.

=item o parse.sh

Simplifies running parse.pl.

=item o parse2rend.pl

Convert all data/*.parse to data/*.rend using rend.pl.

=item o pod2html.sh

Converts all *.pm files to *.html, and copies them in my web server's dir structure.

=item o rend.1.sh

Simplifies running read.sh.

=item o rend.pl Try running with -h.

Run the default renderer.

=item o rend.sh

Simplifies running rend.pl.

=item o rend2svg.pl

Convert all data/*.rend to html/*.svg using dot.

=item o stt2html.pl

Convert data/stt.csv to html/stt.html.

=back

=head1 Distributions

This module is available as a Unix-style distro (*.tgz).

See L<http://savage.net.au/Perl-modules/html/installing-a-module.html>
for help on unpacking and installing distros.

=head1 Installation

Install L<GraphViz2::Marpa> as you would for any C<Perl> module:

Run:

	cpanm GraphViz2::Marpa

or run:

	sudo cpan GraphViz2::Marpa

or unpack the distro, and then either:

	perl Build.PL
	./Build
	./Build test
	sudo ./Build install

or:

	perl Makefile.PL
	make (or dmake or nmake)
	make test
	make install

=head1 Constructor and Initialization

C<new()> is called as C<< my($g2m) = GraphViz2::Marpa -> new(k1 => v1, k2 => v2, ...) >>.

It returns a new object of type C<GraphViz2::Marpa>.

Key-value pairs accepted in the parameter list (see corresponding methods for details
[e.g. L<description([$graph])>]):

=over 4

=item o description => $graphDescription

Read the L<Graphviz|http://www.graphviz.org/> (dot) graph definition from the command line.

You are strongly encouraged to surround this string with '...' to protect it from your shell.

See also the 'input_file' option to read the description from a file.

The 'description' option takes precedence over the 'input_file' option.

Default: ''.

=item o input_file => $aDotInputFileName

Read the L<Graphviz|http://www.graphviz.org/> (dot) graph definition from a file.

See also the 'description' option to read the graph definition from the command line.

The 'description' option takes precedence over the 'input_file' option.

Default: ''.

See the distro for data/*.gv.

=item o lexed_file => $aLexedOutputFileName

Specify the name of a CSV file of lexed tokens for the lexer to write. This file can be input to the parser.

Default: ''.

The default means the file is not written.

See the distro for data/*.lex.

=item o logger => $aLoggerObject

Specify a logger compatible with L<Log::Handler>, for the lexer and parser to use.

Default: A logger of type L<Log::Handler> which writes to the screen.

To disable logging, just set 'logger' to the empty string (not undef).

=item o maxlevel => $logOption1

This option affects L<Log::Handler>.

See the L<Log::Handler::Levels> docs.

Default: 'notice'.

=item o minlevel => $logOption2

This option affects L<Log::Handler>.

See the L<Log::Handler::Levels> docs.

Default: 'error'.

No lower levels are used.

=item o output_file => aRenderedOutputFileName

Specify the name of a file for the renderer to write.

Default: ''.

The default means the renderer is not called.

=item o parsed_file => aParsedOutputFileName

Specify the name of a CSV file of parsed tokens for the parser to write. This file can be input to the renderer.

Default: ''.

The default means the file is not written.

=item o renderer => $aRendererObject

Specify a renderer for the parser to use.

Default: undef.

=item o report_forest => $Boolean

Log the forest of paths recognised by the parser.

Default: 0.

=item o report_items => $Boolean

Log the items recognised by the lexer.

Default: 0.

=item o report_stt => $Boolean

Log the State Transition Table.

Calls L<Set::FA::Element/report()>. Set min and max log levels to 'info' for this.

Default: 0.

=item o stt_file => $sttFileName

Specify which file contains the State Transition Table.

Default: ''.

The default value means the STT is read from the source code of L<GraphViz2::Marpa::Lexer>.

Candidate files are '' and 'data/stt.csv'.

The type of this file must be specified by the 'type' option.

If the file name matches /csv$/, the value of the 'type' option is set to 'csv'.

=item o timeout => $seconds

Run the DFA for at most this many seconds.

Default: 10.

=item o type => $type

Specify the type of the stt_file: '' for internal STT and 'csv' for CSV.

Default: ''.

The default value means the STT is read from the source code of L<GraphViz2::Marpa::Lexer>.

This option must be used with the 'stt_file' option.

Warning: The 'ods' option is disabled, because I can find no way in LibreOffice to make it operate in ASCII. What happens is that when you type "
(i.e. the double-quote character on the keyboard), LibreOffice inserts a different double-quote character, which, when exported as CSV in Unicode
format, produces these 3 bytes: 0xe2, 0x80, 0x9c. This means that if you edit the STT, you absolutely must export to a CSV file in ASCII format.
It also means that dot identifiers in (normal) double-quotes will never match the double-quotes in the *.ods file.

=back

=head1 Methods

=head2 description([$graph])

The [] indicate an optional parameter.

Get or set the L<Graphviz|http://www.graphviz.org/> (dot) graph definition string.

The value supplied by the 'description' option takes precedence over the value read from the 'input_file'.

See also L</input_file()>.

'description' is a parameter to L</new()>. See L</Constructor and Initialization> for details.

=head2 input_file([$graph_file_name])

Here, the [] indicate an optional parameter.

Get or set the name of the file to read the L<Graphviz|http://www.graphviz.org/> (dot) graph definition from.

The value supplied by the 'description' option takes precedence over the value read from the 'input_file'.

See also the L</description()> method.

'input_file' is a parameter to L</new()>. See L</Constructor and Initialization> for details.

=head2 lexed_file([$lex_file_name])

Here, the [] indicate an optional parameter.

Get or set the name of the CSV file of lexed tokens for the lexer to write. This file can be input to the parser.

'lexed_file' is a parameter to L</new()>. See L</Constructor and Initialization> for details.

=head2 logger([$logger_object])

Here, the [] indicate an optional parameter.

Get or set the logger object.

To disable logging, just set 'logger' to the empty string (not undef), in the call to L</new()>.

This logger is passed to other modules.

'logger' is a parameter to L</new()>. See L</Constructor and Initialization> for details.

=head2 maxlevel([$string])

Here, the [] indicate an optional parameter.

Get or set the value used by the logger object.

This option is only used if L<GraphViz2::Marpa:::Lexer> or L<GraphViz2::Marpa::Parser>
use or create an object of type L<Log::Handler>. See L<Log::Handler::Levels>.

'maxlevel' is a parameter to L</new()>. See L</Constructor and Initialization> for details.

=head2 minlevel([$string])

Here, the [] indicate an optional parameter.

Get or set the value used by the logger object.

This option is only used if L<GraphViz2::Marpa:::Lexer> or L<GraphViz2::Marpa::Parser>
use or create an object of type L<Log::Handler>. See L<Log::Handler::Levels>.

'minlevel' is a parameter to L</new()>. See L</Constructor and Initialization> for details.

=head2 new()

See L</Constructor and Initialization> for details on the parameters accepted by L</new()>.

=head2 output_file([$file_name])

Here, the [] indicate an optional parameter.

Get or set the name of the file for the renderer to write.

'output_file' is a parameter to L</new()>. See L</Constructor and Initialization> for details.

=head2 parsed_file([$file_name])

Here, the [] indicate an optional parameter.

Get or set the name of the file of parsed tokens for the parser to write. This file can be input to the renderer.

'parsed_file' is a parameter to L</new()>. See L</Constructor and Initialization> for details.

=head2 renderer([$renderer_object])

Here, the [] indicate an optional parameter.

Get or set the renderer object.

This renderer is passed to L<GraphViz2::Marpa::Parser>.

'renderer' is a parameter to L</new()>. See L</Constructor and Initialization> for details.

=head2 report_forest([$Boolean])

The [] indicate an optional parameter.

Get or set the value which determines whether or not to log the forest of paths recognised by the parser.

'report_forest' is a parameter to L</new()>. See L</Constructor and Initialization> for details.

=head2 report_items([$Boolean])

The [] indicate an optional parameter.

Get or set the value which determines whether or not to log the items recognised by the lexer and parser.

'report_items' is a parameter to L</new()>. See L</Constructor and Initialization> for details.

=head2 report_stt([$Boolean])

The [] indicate an optional parameter.

Get or set the value which determines whether or not to log the parsed state transition table (STT).

'report_stt' is a parameter to L</new()>. See L</Constructor and Initialization> for details.

=head2 run()

This is the only method the caller needs to call. All parameters are supplied to L</new()> (or other methods).

Returns 0 for success and 1 for failure.

=head2 stt_file([$stt_file_name])

The [] indicate an optional parameter.

Get or set the name of the file containing the State Transition Table.

This option is used in conjunction with the 'type' option to L</new()>.

If the file name matches /csv$/, the value of the 'type' option is set to 'csv'.

'stt_file' is a parameter to L</new()>. See L</Constructor and Initialization> for details.

=head2 timeout($seconds)

The [] indicate an optional parameter.

Get or set the timeout for how long to run the DFA and the Parser.

'timeout' is a parameter to L</new()>. See L</Constructor and Initialization> for details.

=head2 type([$type])

The [] indicate an optional parameter.

Get or set the value which determines what type of 'stt_file' is read.

'type' is a parameter to L</new()>. See L</Constructor and Initialization> for details.

=head1 FAQ

The lexer and the parser each has an FAQ: L<Lexer|GraphViz2::Marpa::Lexer/FAQ>, and L<Parser|GraphViz2::Marpa::Parser/FAQ>.

=head2 What is the homepage of Marpa?

L<http://jeffreykegler.github.com/Marpa-web-site/>.

=head2 Why do I get error messages like the following?

	Error: <stdin>:1: syntax error near line 1
	context: digraph >>>  Graph <<<  {

Graphviz reserves some words as keywords, meaning they can't be used as an ID, e.g. for the name of the graph.
So, don't do this:

	strict graph graph{...}
	strict graph Graph{...}
	strict graph strict{...}
	etc...

Likewise for non-strict graphs, and digraphs. You can however add double-quotes around such reserved words:

	strict graph "graph"{...}

Even better, use a more meaningful name for your graph...

The keywords are: node, edge, graph, digraph, subgraph and strict. Compass points are not keywords.

See L<keywords|http://www.graphviz.org/content/dot-language> in the discussion of the syntax of DOT
for details.

=head2 Does this package support Unicode in the input dot file?

No. Sorry. Not yet.

=head2 How can I switch from Marpa::XS to Marpa::PP?

Install Marpa::PP manually. It is not mentioned in Build.PL or Makefile.PL.

Patch GraphViz2::Marpa::Parser (line 15) from Marpa::XS to Marpa:PP.

Then, run the tests which ship with this module. I've tried this, and the tests all worked. You don't need to install the code to test it. Just use:

	shell> cd GraphViz2-Marpa-1.00/
	shell> prove -Ilib -v t

=head2 If I input x.gv and output x.rend, should these 2 files be identical?

Yes - at least in the sense that running dot with them as input will produce the same output files. This is using the default renderer, of course.

Since comments in *.gv files are discarded, they can never be in the output files (*.lex, *.parse and *.rend).

So, if x.gv is formatted as I do, then x.rend will be formatted identically.

=head2 Why does the report_items option output 2 copies of the tokens?

Because the 1st copy is printed by the lexer and the 2nd by the parser.

=head2 How are custom graph attributes handled?

They are not handled at all. Sorry. The lexer only supports attributes defined by Graphviz itself.

=head2 How are the demo files generated?

I run:

	shell> scripts/generate.demo.sh

Which runs these:

	shell> perl scripts/dot2rend.pl
	shell> perl scripts/rend2svg.pl
	shell> perl scripts/generate.index.pl

And copies the demo files to my dev machine's doc root:

	shell> cp html/*.html html/*.svg $DR/Perl-modules/html/graphviz2.marpa/

=head1 Machine-Readable Change Log

The file CHANGES was converted into Changelog.ini by L<Module::Metadata::Changes>.

=head1 Version Numbers

Version numbers < 1.00 represent development versions. From 1.00 up, they are production versions.

=head1 Thanks

Many thanks are due to the people who worked on L<Graphviz|http://www.graphviz.org/>.

Jeffrey Kegler wrote L<Marpa::XS>, and has a blog on it at L<http://blogs.perl.org/users/jeffrey_kegler/>.

=head1 Support

Email the author, or log a bug on RT:

L<https://rt.cpan.org/Public/Dist/Display.html?Name=GraphViz2::Marpa>.

=head1 Author

L<GraphViz2::Marpa> was written by Ron Savage I<E<lt>ron@savage.net.auE<gt>> in 2012.

Home page: L<http://savage.net.au/index.html>.

=head1 Copyright

Australian copyright (c) 2012, Ron Savage.

	All Programs of mine are 'OSI Certified Open Source Software';
	you can redistribute them and/or modify them under the terms of
	The Artistic License, a copy of which is available at:
	http://www.opensource.org/licenses/index.html

=cut
