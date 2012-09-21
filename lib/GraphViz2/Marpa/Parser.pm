package GraphViz2::Marpa::Parser;

use strict;
use warnings;

use GraphViz2::Marpa::Renderer::GraphViz2;
use GraphViz2::Marpa::Utils;

use Hash::FieldHash ':all';

use IO::File;

use Log::Handler;

use Marpa::XS;

use Set::Array;

use Text::CSV_XS;

use Try::Tiny;

fieldhash my %item_count   => 'item_count';
fieldhash my %items        => 'items';
fieldhash my %lexed_file   => 'lexed_file';
fieldhash my %logger       => 'logger';
fieldhash my %maxlevel     => 'maxlevel';
fieldhash my %minlevel     => 'minlevel';
fieldhash my %output_file  => 'output_file';
fieldhash my %parsed_file  => 'parsed_file';
fieldhash my %renderer     => 'renderer';
fieldhash my %report_items => 'report_items';
fieldhash my %tokens       => 'tokens';
fieldhash my %utils        => 'utils';

# $myself is a copy of $self for use by functions called by Marpa.

our $myself;
our $VERSION = '1.04';

# --------------------------------------------------
# This is a function, not a method.

sub attribute_id
{
	my($stash, $t1, undef, $t2)  = @_;

	$myself -> new_item('attribute_id', $t1);

	return $t1;

} # End of attribute_id.

# --------------------------------------------------
# This is a function, not a method.

sub attribute_value
{
	my($stash, $t1, undef, $t2)  = @_;

	$myself -> new_item('attribute_value', $t1);

	return $t1;

} # End of attribute_value.

# --------------------------------------------------
# This is a function, not a method.

sub colon_id
{
	my($stash, $t1, undef, $t2)  = @_;

	$myself -> new_item('colon', $t1);

	return $t1;

} # End of colon_id.

# --------------------------------------------------
# This is a function, not a method.

sub compass_id
{
	my($stash, $t1, undef, $t2)  = @_;

	$myself -> new_item('compass_point', $t1);

	return $t1;

} # End of compass_id.

# --------------------------------------------------
# This is a function, not a method.

sub digraph
{
	my($stash, $t1, undef, $t2)  = @_;

	$myself -> new_item('digraph', $t1);

	return $t1;

} # End of digraph.

# --------------------------------------------------
# This is a function, not a method.

sub edge_id
{
	my($stash, $t1, undef, $t2)  = @_;

	$myself -> new_item('edge_id', $t1);

	return $t1;

} # End of edge_id.

# --------------------------------------------------
# This is a function, not a method.

sub end_attributes
{
	my($stash, $t1, undef, $t2)  = @_;

	$myself -> new_item('end_attribute', $t1);

	return $t1;

} # End of end_attributes.

# --------------------------------------------------
# This is a function, not a method.

sub end_graph
{
	my($stash, $t1, undef, $t2)  = @_;

	$myself -> new_item('end_graph', $t1);

	return $t1;

} # End of end_graph.

# --------------------------------------------------
# This is a function, not a method.

sub end_subgraph
{
	my($stash, $t1, undef, $t2)  = @_;

	$myself -> new_item('end_subgraph', $t1);

	return $t1;

} # End of end_subgraph.

# --------------------------------------------------

sub generate_parsed_file
{
	my($self, $file_name) = @_;

	open(OUT, '>', $file_name) || die "Can't open(> $file_name): $!";

	print OUT qq|"type","value"\n|;

	for my $item (@{$self -> items})
	{
		print OUT $self -> utils -> justify($$item{type}), qq|, "$$item{value}"|, "\n";
	}

	close OUT;

} # End of generate_parsed_file.

# --------------------------------------------------

sub grammar
{
	my($self)    = @_;
	my($grammar) = Marpa::Grammar -> new
		({
		actions       => __PACKAGE__,
		lhs_terminals => 0,
		start         => 'graph_grammar',
		symbols       =>
		{
			attribute_id    => {terminal => 1},
			attribute_value => {terminal => 1},
			close_brace     => {terminal => 1},
			close_bracket   => {terminal => 1},
			colon           => {terminal => 1},
			compass_point   => {terminal => 1},
			digraph         => {terminal => 1},
			edge_id         => {terminal => 1},
			end_subgraph    => {terminal => 1},
			equals          => {terminal => 1},
			graph_id        => {null_value => '', terminal => 1},
			id              => {terminal => 1},
			node_id         => {terminal => 1},
			open_brace      => {terminal => 1},
			open_bracket    => {terminal => 1},
			port_id         => {terminal => 1},
			start_subgraph  => {terminal => 1},
			strict          => {terminal => 1},
		},
		rules =>
			[
			 {   # Root-level stuff.
				 lhs => 'graph_grammar',
				 rhs => [qw/prolog_and_graph/],
			 },
			 {
				 lhs => 'prolog_and_graph',
				 rhs => [qw/prolog_definition graph_sequence_definition/],
			 },
			 {   # Prolog stuff.
				 lhs => 'prolog_definition',
				 rhs => [qw/strict_definition digraph_definition graph_id_definition/],
			 },
			 {
				 lhs    => 'strict_definition',
				 rhs    => [qw/strict/],
				 action => 'strict',
			 },
			 {
				 lhs    => 'digraph_definition',
				 rhs    => [qw/digraph/],
				 action => 'digraph',
			 },
			 {
				 lhs    => 'graph_id_definition',
				 rhs    => [qw/graph_id/],
				 action => 'graph_id',
			 },
			 {   # Graph stuff.
				 lhs => 'graph_sequence_definition',
				 rhs => [qw/start_graph graph_sequence_list end_graph/],
			 },
			 {
				 lhs    => 'start_graph',
				 rhs    => [qw/open_brace/],
				 action => 'start_graph',
			 },
			 {
				 lhs => 'graph_sequence_list',
				 rhs => [qw/graph_sequence_item/],
				 min => 0,
			 },
			 {
				 lhs => 'graph_sequence_item', # 1 of 7.
				 rhs => [qw/node_sequence_definition/],
			 },
			 {
				 lhs => 'graph_sequence_item', # 2 of 7.
				 rhs => [qw/node_sequence_definition edge_sequence_definition/],
			 },
			 {
				 lhs => 'graph_sequence_item', # 3 of 7.
				 rhs => [qw/node_sequence_definition edge_sequence_definition graph_sequence_definition/],
			 },
			 {
				 lhs => 'graph_sequence_item', # 4 of 7.
				 rhs => [qw/node_sequence_definition graph_sequence_definition/],
			 },
			 {
				 lhs => 'graph_sequence_item', # 5 of 7.
				 rhs => [qw/attribute_sequence_definition/],
			 },
			 {
				 lhs => 'graph_sequence_item', # 6 of 7.
				 rhs => [qw/id_sequence_definition/],
			 },
			 {
				 lhs => 'graph_sequence_item', # 7 of 7.
				 rhs => [qw/subgraph_sequence_definition/],
			 },
			 {
				 lhs    => 'end_graph',
				 rhs    => [qw/close_brace/],
				 action => 'end_graph',
			 },
			 {   # Node stuff.
				 lhs => 'node_sequence_definition',
				 rhs => [qw/node_sequence_item/],
			 },
			 {
				 lhs => 'node_sequence_item', # 1 of 4.
				 rhs => [qw/node_statement/],
			 },
			 {
				 lhs => 'node_sequence_item', # 2 of 4.
				 rhs => [qw/node_statement attribute_definition/],
			 },
			 {
				 lhs => 'node_sequence_item', # 3 of 4.
				 rhs => [qw/node_statement attribute_definition graph_sequence_item/],
			 },
			 {
				 lhs => 'node_sequence_item', # 3 of 4.
				 rhs => [qw/start_graph graph_sequence_list end_graph/],
			 },
			 {
				 lhs => 'node_statement', # 1 of 4.
				 rhs => [qw/node_item/],
			 },
			 {
				 lhs => 'node_statement', # 2 of 4.
				 rhs => [qw/node_item colon_item port_item/],
			 },
			 {
				 lhs => 'node_statement', # 3 of 4.
				 rhs => [qw/node_item colon_item port_item colon_item compass_item/],
			 },
			 {
				 lhs => 'node_statement', # 4 of 4.
				 rhs => [qw/node_item colon_item compass_item/],
			 },
			 {
				 lhs    => 'node_item',
				 rhs    => [qw/node_id/],
				 action => 'node_id',
			 },
			 {
				 lhs    => 'colon_item',
				 rhs    => [qw/colon/],
				 action => 'colon_id',
			 },
			 {
				 lhs    => 'port_item',
				 rhs    => [qw/port_id/],
				 action => 'port_id',
			 },
			 {
				 lhs    => 'compass_item',
				 rhs    => [qw/compass_point/],
				 action => 'compass_id',
			 },
			 {   # Id stuff.
				 lhs => 'id_sequence_definition',
				 rhs => [qw/id_sequence_item/],
			 },
			 {
				 lhs => 'id_sequence_item', # 1 of 2.
				 rhs => [qw/id_statement/],
			 },
			 {
				 lhs => 'id_sequence_item', # 2 of 2.
				 rhs => [qw/id_statement attribute_definition/],
			 },
			 {
				 lhs => 'id_statement',
				 rhs => [qw/id/],
				 action => 'id',
			 },
			 {   # Subgraph stuff.
				 lhs => 'subgraph_sequence_definition',
				 rhs => [qw/start_subgraph_count subgraph_id graph_sequence_definition end_subgraph_count/],
			 },
			 {
				 lhs    => 'start_subgraph_count',
				 rhs    => [qw/start_subgraph/],
				 action => 'start_subgraph',
			 },
			 {
				 lhs => 'subgraph_id',
				 rhs => [qw/graph_id/],
				 action => 'subgraph_id',
			 },
			 {
				 lhs => 'end_subgraph_count',
				 rhs => [qw/end_subgraph/],
				 action => 'end_subgraph',
			 },
			 {   # Edge stuff.
				 lhs => 'edge_sequence_definition',
				 rhs => [qw/edge_sequence_item/],
			 },
			 {
				 lhs => 'edge_sequence_item', # 1 of 3.
				 rhs => [qw/edge_statement/],
			 },
			 {
				 lhs => 'edge_sequence_item', # 2 of 3.
				 rhs => [qw/edge_statement graph_sequence_definition/],
			 },
			 {
				 lhs => 'edge_statement',
				 rhs => [qw/edge_name attribute_definition/],
			 },
			 {
				 lhs    => 'edge_name',
				 rhs    => [qw/edge_id/],
				 action => 'edge_id',
			 },
			 {   # Attribute stuff.
				 lhs => 'attribute_definition',
				 rhs => [qw/attribute_sequence/],
				 min => 0,
			 },
			 {
				 lhs => 'attribute_sequence',
				 rhs => [qw/start_attributes attribute_sequence_definition end_attributes/],
			 },
			 {
				 lhs    => 'start_attributes',
				 rhs    => [qw/open_bracket/],
				 action => 'start_attributes',
			 },
			 {
				 lhs => 'attribute_sequence_definition',
				 rhs => [qw/attribute_sequence_item/],
			 },
			 {
				 lhs => 'attribute_sequence_item', # 1 of 3.
				 rhs => [qw/attribute_statement/],
			 },
			 {
				 lhs => 'attribute_sequence_item', # 2 of 3.
				 rhs => [qw/attribute_statement attribute_sequence_item/],
			 },
			 {
				 lhs => 'attribute_sequence_item', # 3 of 3.
				 rhs => [qw/attribute_sequence_item graph_sequence_item/],
			 },
			 {
				 lhs => 'attribute_statement',
				 rhs => [qw/attribute_key has attribute_val/],
			 },
			 {
				 lhs    => 'attribute_key',
				 rhs    => [qw/attribute_id/],
				 min    => 1,
				 action => 'attribute_id',
			 },
			 {
				 lhs    => 'has',
				 rhs    => [qw/equals/],
				 min    => 1,
			 },
			 {
				 lhs    => 'attribute_val',
				 rhs    => [qw/attribute_value/],
				 min    => 1,
				 action => 'attribute_value',
			 },
			 {
				 lhs    => 'end_attributes',
				 rhs    => [qw/close_bracket/],
				 action => 'end_attributes',
			 },
			],
		});

	$grammar -> precompute;

	return $grammar;

} # End of grammar;

# --------------------------------------------------
# This is a function, not a method.

sub graph_id
{
	my($stash, $t1, undef, $t2)  = @_;

	$myself -> new_item('graph_id', $t1);

	return $t1;

} # End of graph_id.

# --------------------------------------------------
# This is a function, not a method.

sub id
{
	my($stash, $t1, undef, $t2)  = @_;

	$myself -> new_item('id', $t1);

	return $t1;

} # End of id.

# --------------------------------------------------

sub increment_item_count
{
	my($self) = @_;

	# Warning! Don't use:
	# return $self -> item_count($self -> item_count + 1);
	# It returns $self.

	$self -> item_count($self -> item_count + 1);

	return $self -> item_count;

} # End of increment_item_count.

# --------------------------------------------------

sub _init
{
	my($self, $arg)     = @_;
	$$arg{item_count}   = 0;
	$$arg{items}        = Set::Array -> new;
	$$arg{lexed_file}   ||= ''; # Caller can set.
	$$arg{logger}       = defined($$arg{logger}) ? $$arg{logger} : undef; # Caller can set.
	$$arg{maxlevel}     ||= 'notice'; # Caller can set.
	$$arg{minlevel}     ||= 'error';  # Caller can set.
	$$arg{output_file}  ||= '';       # Caller can set.
	$$arg{parsed_file}  ||= '';       # Caller can set.
	$$arg{renderer}     = defined($$arg{renderer}) ? $$arg{renderer} : undef; # Caller can set.
	$$arg{report_items} ||= 0;        # Caller can set.
	$$arg{tokens}       ||= [];       # Caller can set.
	$$arg{utils}        = GraphViz2::Marpa::Utils -> new;
	$self               = from_hash($self, $arg);
	$myself             = $self;

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

	if (! defined $self -> renderer)
	{
		$self -> renderer
			(
			GraphViz2::Marpa::Renderer::GraphViz2 -> new
			(
			 logger      => $self -> logger,
			 output_file => $self -> output_file,
			)
			);
	}

	return $self;

} # End of _init.

# --------------------------------------------------

sub log
{
	my($self, $level, $s) = @_;

	$self -> logger -> $level($s) if ($self -> logger);

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

sub new_item
{
	my($self, $type, $value) = @_;

	$self -> items -> push
		({
			count => $self -> increment_item_count,
			name  => '',
			type  => $type,
			value => $value,
		});

} # End of new_item.

# --------------------------------------------------
# This is a function, not a method.

sub node_id
{
	my($stash, $t1, undef, $t2)  = @_;

	$myself -> new_item('node_id', $t1);

	return $t1;

} # End of node_id.

# --------------------------------------------------
# This is a function, not a method.

sub port_id
{
	my($stash, $t1, undef, $t2)  = @_;

	$myself -> new_item('port_id', $t1);

	return $t1;

} # End of port_id.

# --------------------------------------------------

sub report
{
	my($self)   = @_;
	my($format) = '%4s  %-16s  %s';

	$self -> log(notice => sprintf($format, 'Item', 'Type', 'Value') );

	for my $item ($self -> items -> print)
	{
		$self -> log(notice => sprintf($format, $$item{count}, $$item{type}, $$item{value}) );
	}

} # End of report.

# --------------------------------------------------

sub run
{
	my($self)       = @_;
	my($recognizer) = Marpa::Recognizer -> new({grammar => $self -> grammar});

	if ($#{$self -> tokens} < 0)
	{
		for my $record (@{$self -> utils -> read_csv_file($self -> lexed_file)})
		{
			$recognizer -> read($$record{type}, $$record{value});
		}
	}
	else
	{
		for my $item (@{$self -> tokens})
		{
			$recognizer -> read($$item{type}, $$item{value});
		}
	}

	my($result) = $recognizer -> value;
	$result     = $result ? ${$result} : 'Parse failed';
	$result     = $result ? $result    : 0;

	die $result if ($result);

	# Log tokens.

	$self -> report if ($self -> report_items);

	# Write tokens to a CSV file.

	my($file_name) = $self -> parsed_file;

	if ($file_name)
	{
		$self -> generate_parsed_file($file_name);
	}

	# Pass the tokens to the renderer.

	if ($self -> renderer)
	{
		$self -> renderer -> tokens([$self -> items -> print]);
		$self -> renderer -> run;
	}

	# Return 0 for success and 1 for failure.

	return 0;

} # End of run.

# --------------------------------------------------
# This is a function, not a method.

sub start_attributes
{
	my($stash, $t1, undef, $t2)  = @_;

	$myself -> new_item('start_attribute', $t1);

	return $t1;

} # End of start_attributes.

# --------------------------------------------------
# This is a function, not a method.

sub start_graph
{
	my($stash, $t1, undef, $t2)  = @_;

	$myself -> new_item('start_graph', $t1);

	return $t1;

} # End of start_graph.

# --------------------------------------------------
# This is a function, not a method.

sub start_subgraph
{
	my($stash, $t1, undef, $t2)  = @_;

	$myself -> new_item('start_subgraph', $t1);

	return $t1;

} # End of start_subgraph.

# --------------------------------------------------
# This is a function, not a method.

sub strict
{
	my($stash, $t1, undef, $t2)  = @_;

	$myself -> new_item('strict', $t1);

	return $t1;

} # End of strict.

# --------------------------------------------------
# This is a function, not a method.

sub subgraph_id
{
	my($stash, $t1, undef, $t2)  = @_;

	$myself -> new_item('graph_id', $t1);

	return $t1;

} # End of subgraph_id.

# --------------------------------------------------

1;

=pod

=head1 NAME

L<GraphViz2::Marpa::Parser> - A Perl parser for Graphviz dot files. Input comes from L<GraphViz2::Marpa::Lexer>.

=head1 Synopsis

=over 4

=item o Display help

	perl scripts/lex.pl   -h
	perl scripts/parse.pl -h
	perl scripts/g2m.pl   -h

=item o Run the lexer

	perl scripts/lex.pl -i x.dot -l x.lex

	x.dot is a Graphviz dot file. x.lex will be a CSV file of lexed tokens.

=item o Run the parser without running the lexer or the default renderer

	perl scripts/parse.pl -l x.lex -p x.parse

	x.parse will be a CSV file of parsed tokens.

=item o Run the parser and the default renderer

	perl scripts/parse.pl -l x.lex -p x.parse -o x.rend

	x.rend will be a Graphviz dot file.

=item o Run the lexer, parser and default renderer

	perl scripts/g2m.pl -i x.dot -l x.lex -p x.parse -o x.rend

=back

=head1 Description

L<GraphViz2::Marpa::Lexer> provides a L<Marpa::XS>-based parser for L<http://www.graphviz.org/> dot files.

The input is expected to be, via RAM or a CSV file, from L<GraphViz2::Marpa::Lexer>.

Demo lexer/parser output: L<http://savage.net.au/Perl-modules/html/graphviz2.marpa/index.html>.

State Transition Table: L<http://savage.net.au/Perl-modules/html/graphviz2.marpa/default.stt.html>.

Command line options and object attributes: L<http://savage.net.au/Perl-modules/html/graphviz2.marpa/code.attributes.html>.

My article on this set of modules: L<http://savage.net.au/Ron/html/graphviz2.marpa/Lexing.and.Parsing.with.Marpa.html>.

The Marpa grammar as an image: L<http://savage.net.au/Ron/html/graphviz2.marpa/Marpa.Grammar.svg>. This image was created
with L<Graphviz|http://www.graphviz.org/> via L<GraphViz2>.

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

C<new()> is called as C<< my($parser) = GraphViz2::Marpa::Parser -> new(k1 => v1, k2 => v2, ...) >>.

It returns a new object of type C<GraphViz2::Marpa::Parser>.

Key-value pairs accepted in the parameter list (see corresponding methods for details
[e.g. maxlevel()]):

=over 4

=item o lexed_file => $aLexedOutputFileName

Specify the name of a CSV file of lexed tokens to read. This file can be output from the lexer.

Default: ''.

The default means the file is not read.

The value supplied by the 'tokens' option takes preference over the 'lexed_file' option.

See the distro for data/*.lex.

=item o logger => $aLoggerObject

Specify a logger compatible with L<Log::Handler>, for the parser and renderer to use.

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

Specify the name of a file to be passed to the renderer.

Default: ''.

The default means the renderer is not called.

=item o parsed_file => aParsedOutputFileName

Specify the name of a CSV file of parsed tokens to write. This file can be input to the default renderer.

Default: ''.

The default means the file is not written.

=item o renderer => $aRendererObject

Specify a renderer for the parser to use.

Default: A object of type L<GraphViz2::Marpa::Renderer::GraphViz2>.

=item o report_items => $Boolean

Log the items recognised by the lexer.

Default: 0.

=item o tokens => anArrayrefOfLexedTokens

Specify an arrayref of tokens output by the lexer.

The value supplied by the 'tokens' option takes preference over the 'lexed_file' option.

=back

=head1 Methods

=head2 items()

Returns an arrayref of parsed tokens. Each element of this arrayref is a hashref. See L</How is the parsed graph stored in RAM?> for details.

These parsed tokens do I<not> bear a one-to-one relationship to the lexed tokens returned by the lexer's L<items()/GraphViz2::Marpa::Lexer> method.
However, they are (necessarily) very similar.

If you provide an output file by using the 'parsed_file' option to L</new()>, or the L</parsed_file()> method, the file will have 2 columns, type and value.

E.g.: If the arrayref looks like:

	...
	{count => 10, name => '', type => 'start_attribute', value => '['},
	{count => 11, name => '', type => 'attribute_id'   , value => 'color'},
	{count => 13, name => '', type => 'attribute_value', value => 'red'},
	{count => 14, name => '', type => 'end_attribute'  , value => ']'},
	...

then the output file will look like:

	"type","value"
	...
	start_attribute , "["
	attribute_id    , "color"
	attribute_value , "red"
	end_attribute   , "]"
	...

Usage:

	my($parser) = GraphViz2::Marpa::Parser -> new(...);

	# $parser -> items actually returns an object of type Set::Array.

	if ($parser -> run == 0)
	{
		my(@items) = @{$parser -> items};
	}

=head2 lexed_file([$lex_file_name])

'lexed_file' is a parameter to L</new()>. See L</Constructor and Initialization> for details.

Here, the [] indicate an optional parameter.

Get or set the name of the CSV file of lexed tokens to read. This file can be output by the lexer.

The value supplied by the 'tokens' option takes preference over the 'lexed_file' option.

=head2 logger([$logger_object])

Here, the [] indicate an optional parameter.

Get or set the logger object.

To disable logging, just set 'logger' to the empty string, in the call to L</new()>.

This logger is passed to the default renderer.

=head2 maxlevel([$string])

'maxlevel' is a parameter to L</new()>. See L</Constructor and Initialization> for details.

Here, the [] indicate an optional parameter.

Get or set the value used by the logger object.

This option is only used if L<GraphViz2::Marpa:::Lexer> or L<GraphViz2::Marpa::Parser>
use or create an object of type L<Log::Handler>. See L<Log::Handler::Levels>.

=head2 minlevel([$string])

'minlevel' is a parameter to L</new()>. See L</Constructor and Initialization> for details.

Here, the [] indicate an optional parameter.

Get or set the value used by the logger object.

This option is only used if L<GraphViz2::Marpa:::Lexer> or L<GraphViz2::Marpa::Parser>
use or create an object of type L<Log::Handler>. See L<Log::Handler::Levels>.

=head2 new()

See L</Constructor and Initialization> for details on the parameters accepted by L</new()>.

=head2 output_file([$file_name])

'output_file' is a parameter to L</new()>. See L</Constructor and Initialization> for details.

Here, the [] indicate an optional parameter.

Get or set the name of the file to be passed to the renderer.

=head2 parsed_file([$file_name])

'parsed_file' is a parameter to L</new()>. See L</Constructor and Initialization> for details.

Here, the [] indicate an optional parameter.

Get or set the name of the file of parsed tokens for the parser to write. This file can be input to the renderer.

=head2 renderer([$renderer_object])

'renderer' is a parameter to L</new()>. See L</Constructor and Initialization> for details.

Here, the [] indicate an optional parameter.

Get or set the renderer object.

This renderer renders the tokens output by the parser.

=head2 report_items([$Boolean])

'report_items' is a parameter to L</new()>. See L</Constructor and Initialization> for details.

The [] indicate an optional parameter.

Get or set the value which determines whether or not to log the items recognised by the parser.

=head2 run()

This is the only method the caller needs to call. All parameters are supplied to L</new()> (or other methods).

Returns 0 for success and 1 for failure.

=head2 tokens([$arrayrefOfLexedTokens])

'tokens' is a parameter to L</new()>. See L</Constructor and Initialization> for details.

Here, the [] indicate an optional parameter.

Get or set the arrayref of lexed tokens to process.

The value supplied by the 'tokens' option takes preference over the 'lexed_file' option.

=head2 utils([$aUtilsObject])

Here, the [] indicate an optional parameter.

Get or set the utils object.

Default: A object of type L<GraphViz2::Marpa::Utils>.

=head1 FAQ

=head2 Why doesn't the lexer/parser handle my HTML-style labels?

Traps for young players:

=over 4

=item o The <br /> component must include the '/'. <br align='center'> is not accepted by Graphviz

=item o The <br />'s attributes must use single quotes because output files use CSV with double quotes

=back

See data/38.* for good examples.

=head2 How can I switch from Marpa::XS to Marpa::PP?

Install Marpa::PP manually. It is not mentioned in Build.PL or Makefile.PL.

Patch GraphViz2::Marpa::Parser (line 15) from Marpa::XS to Marpa:PP.

Then, run the tests which ship with this module. I've tried this, and the tests all worked. You don't need to install the code to test it. Just use:

	shell> cd GraphViz2-Marpa-1.00/
	shell> prove -Ilib -v t

=head2 Where are the scripts documented?

In L<GraphViz2::Marpa/Scripts>.

=head2 How is the parsed graph stored in RAM?

Items are stored in an arrayref. This arrayref is available via the L</items()> method.

These items have the same format as the arrayref of items returned by the items() method in
L<GraphViz2::Marpa::Lexer>, and the same as in L<GraphViz2::Marpa::Lexer::DFA>.

However, the precise values in the 'type' field of the following hashref vary between the lexer and the parser.

Each element in the array is a hashref:

	{
		count => $integer, # 1 .. N.
		name  => '',       # Unused.
		type  => $string,  # The type of the token.
		value => $value,   # The value from the input stream.
	}

$type => $value pairs used by the parser are listed here in alphabetical order by $type:

=over 4

=item o attribute_id => $id

=item o attribute_value => $value

=item o colon => ':'

This separates nodes from ports and ports from compass points.

=item o compass_point => $id

=item o digraph => $yes_no

'yes' => digraph and 'no' => graph.

=item o edge_id => $id

$id is either '->' for a digraph or '--' for a graph.

=item o end_attribute => ']'

This indicates the end of a set of attributes.

=item o end_graph => $graph_count

This indicates the end of a graph or subgraph or any stand-alone {}, and - for subgraphs - preceeds the subgraph's 'end_subgraph'.

$graph_count increments by 1 each time 'graph_id' is detected in the input string, and decrements each time a matching 'end_graph' is detected.

=item o end_subgraph => $subgraph_count

This indicates the end of a subgraph, and follows the subgraph's 'end_graph'.

$subgraph_count increments by 1 each time 'start_subgraph' is detected in the input string, and decrements each time a matching 'end_subgraph' is detected.

=item o graph_id => $id

This indicates both the graph's $id and each subgraph's $id.

For graphs and subgraphs, the $id may be '' (the empty string).

=item o node_id => $id

=item o port_id => $id

=item o start_attribute => '['

This indicates the start of a set of attributes.

=item o start_graph => $graph_count

This indicates the start of a graph, or any stand-alone {}.

$graph_count increments by 1 each time 'graph_id' is detected in the input string, and decrements each time a matching 'end_graph' is detected.

=item o start_subgraph => $subgraph_count

This indicates the start of a subgraph, and preceeds the subgraph's 'graph_id'.

$subgraph_count increments by 1 each time 'start_subgraph' is detected in the input string, and decrements each time a matching 'end_subgraph' is detected

=item o strict => $yes_no

'yes' => strict and 'no' => not strict.

=back

Consult data/*.lex and the corresponding data/*.parse for many examples.

=head2 How does the parser handle comments?

Comments are not expected in the input stream.

=head2 How does the parser interact with Marpa?

See L<http://savage.net.au/Perl-modules/html/graphviz2.marpa/Lexing.and.Parsing.with.Marpa.html>.

=head1 Machine-Readable Change Log

The file CHANGES was converted into Changelog.ini by L<Module::Metadata::Changes>.

=head1 Version Numbers

Version numbers < 1.00 represent development versions. From 1.00 up, they are production versions.

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
