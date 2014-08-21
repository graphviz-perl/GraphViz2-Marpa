package GraphViz2::M;

use strict;
use utf8;
use warnings;
use warnings  qw(FATAL utf8);    # Fatalize encoding glitches.
use open      qw(:std :utf8);    # Undeclared streams in UTF-8.
use charnames qw(:full :short);  # Unneeded in v5.16.

use File::Slurp; # For read_file().

use Log::Handler;

use Marpa::R2;

use Moo;

use Tree::DAG_Node;

use Types::Standard qw/Any Str/;

use Try::Tiny;

has description =>
(
	default  => sub{return ''},
	is       => 'rw',
	isa      => Str,
	required => 0,
);

has grammar =>
(
	default  => sub {return ''},
	is       => 'rw',
	isa      => Any,
	required => 0,
);

has graph_text =>
(
	default  => sub{return ''},
	is       => 'rw',
	isa      => Str,
	required => 0,
);

has input_file =>
(
	default  => sub{return ''},
	is       => 'rw',
	isa      => Str,
	required => 0,
);

has items =>
(
	default  => sub{return ''},
	is       => 'rw',
	isa      => Any,
	required => 0,
);

has logger =>
(
	default  => sub{return undef},
	is       => 'rw',
	isa      => Any,
	required => 0,
);

has maxlevel =>
(
	default  => sub{return 'info'},
	is       => 'rw',
	isa      => Str,
	required => 0,
);

has minlevel =>
(
	default  => sub{return 'error'},
	is       => 'rw',
	isa      => Str,
	required => 0,
);

has output_file =>
(
	default  => sub{return ''},
	is       => 'rw',
	isa      => Str,
	required => 0,
);

has recce =>
(
	default  => sub{return ''},
	is       => 'rw',
	isa      => Any,
	required => 0,
);

has renderer =>
(
	default  => sub{return ''},
	is       => 'rw',
	isa      => Any,
	required => 0,
);

our $VERSION = '2.03';

# --------------------------------------------------

sub BUILD
{
	my($self) = @_;

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

	$self->grammar
	(
		Marpa::R2::Scanless::G -> new
		({
source					=> \(<<'END_OF_GRAMMAR'),

:default				::= action => [values]

lexeme default			= latm => 1

:start 					::= graph_definition

# The prolog to the graph.

graph_definition		::= prolog_tokens graph_statement_tokens

prolog_tokens			::= strict_token graph_type global_id_value

strict_token			::=
strict_token			::= strict_literal

graph_type				::= digraph_literal
							| graph_literal

global_id_value			::=
global_id_value			::= global_id_token

global_id_token			::= global_id
							| ('"') global_id ('"')
							| ('<') global_id ('>')

# The graph proper.

graph_statement_tokens	::= open_brace statement_list close_brace

statement_list			::= statement*

# Note: Subgraphs are handled by the statement type 'graph_statement_tokens'.
# Hence subgraph sub_1 {...} and subgraph {...} and sub_1 {...} and {...} are all output as:
# o The optional literal 'subgraph', which is classified as a node_id.
# o The optional subgraph id, which is also classified as a node_id.
# o The literal '{'.
# o The body of the subgraph.
# o The literal '}'.

statement				::= node_statement
							| edge_statement
							| subgraph_statement
							| graph_statement_tokens

# Node stuff.
# Note: generic_id_token is a copy of global_id_token because I wish to trigger a different event.

node_statement			::= generic_id_token attribute_tokens

generic_id_token		::= generic_id
							| ('"') generic_id ('"')
							| ('<') generic_id ('>')

# Attribute stuff.
# These have no body between the '[]' because they are parsed manually in order to
# preserve whitespace (which is discarded by this grammar). See attribute_list().

attribute_tokens		::=
attribute_tokens		::= open_bracket close_bracket

# Edge stuff.

edge_statement			::= edge_node_token edge_literal edge_node_token attribute_tokens

edge_node_token			::= generic_id
							| node_port_id
							| node_port_compass_id

node_port_id			::= node_port
							| ('"') node_port ('"')
							| ('<') node_port ('>')

node_port_compass_id	::= node_port_compass
							| ('"') node_port_compass ('"')
							| ('<') node_port_compass ('>')

# Subgraph stuff.

subgraph_statement		::= graph_statement_tokens

# Lexeme-level stuff, in alphabetical order.

:lexeme					~ close_brace		pause => before		event => close_brace

close_brace				~ '}'

:lexeme					~ close_bracket		pause => before		event => close_bracket

close_bracket			~ ']'

colon					~ ':'

:lexeme					~ digraph_literal	pause => before		event => digraph_literal

digraph_literal			~ 'digraph'

:lexeme					~ edge_literal		pause => before		event => edge_literal

edge_literal			~ '->'
edge_literal			~ '--'

:lexeme					~ global_id			pause => before		event => global_id

global_id_prefix		~ [a-zA-Z\200-\377_]
global_id_suffix		~ [a-zA-Z\200-\377_0-9]*
global_id				~ <global_id_prefix>global_id_suffix

:lexeme					~ generic_id		pause => before		event => generic_id

generic_id				~ <global_id_prefix>global_id_suffix

:lexeme					~ graph_literal		pause => before		event => graph_literal

graph_literal			~ 'graph'

:lexeme					~ open_brace		pause => before		event => open_brace

open_brace				~ '{'

:lexeme					~ open_bracket		pause => before		event => open_bracket

open_bracket			~ '['

:lexeme					~ node_port			pause => before		event => node_port

node_port_prefix		~ <global_id_prefix>global_id_suffix
node_port				~ node_port_prefix<colon>node_port_prefix

:lexeme					~ node_port_compass	pause => before		event => node_port_compass

node_port_compass		~ node_port_prefix<colon>node_port_prefix<colon>node_port_prefix

:lexeme					~ strict_literal	pause => before		event => strict_literal

strict_literal			~ 'strict'

# White space.

:discard				~ whitespace

whitespace				~ [\s]*

END_OF_GRAMMAR
		})
	);

	$self -> recce
	(
		Marpa::R2::Scanless::R -> new
		({
			grammar => $self -> grammar,
		})
	);

	$self -> items(Tree::DAG_Node -> new({name => 'Root'}));

	for my $type (qw/Prolog Graphs/)
	{
		my($node) = Tree::DAG_Node -> new({name => $type});

		$self -> items -> add_daughter($node);
	}

	$GraphViz2::Marpa::Actions::caller = $self;

} # End of BUILD.

# ------------------------------------------------

sub attribute_field
{
	my($self, $input)  = @_;
	my(@char)          = split(//, $input);
	my($char_count)    = 0;
	my($count_quotes)  = 0;
	my($field)         = '';
	my($html)          = 'no';
	my($previous_char) = '';

	my($char);
	my($result);

	for my $i (0 .. $#char)
	{
		$char_count++;

		$char = $char[$i];
		$html = 'yes' if ( ($html eq 'no') && ($char eq '<') && ($field eq '') );

		$self -> log(debug => "Char: $char. HTML: $html. count_quotes: $count_quotes.");

		if ($char eq '"')
		{
			# Gobble up and escaped quotes.

			if ($previous_char eq '\\')
			{
				$field         .= $char;
				$previous_char = $char;

				next;
			}

			$count_quotes++;

			$field .= $char;

			# If HTML, gobble up any quotes.

			if ($html eq 'yes')
			{
				$previous_char = $char;

				next;
			}

			# First quote is start of field.

			if ($count_quotes == 1)
			{
				$previous_char = $char;

				next;
			}

			# Second quote is end of field.

			$count_quotes = 0;
			$result       = $field;
			$field        = '';

			$self -> log(debug => "Result 1: $result.");

			# Skip the quote, and skip any training '\s=\s'.

			$i++;

			$self -> log(debug => "Input:    " . join('', @char[$i .. $#char]) . '.');

			while ( ($i < $#char) && ($char[$i] =~ /[=\s]/) )
			{
				$i++;
				$char_count++;
			}

			$self -> log(debug => "Input:    " . join('', @char[$i .. $#char]) . '.');

			last;
		}
		elsif ( ($char =~ /\s/) && ($count_quotes == 0) )
		{
			# Discard spaces outside quotes and outside HTML.

			$field         .= $char if ($html eq 'yes');
			$previous_char = $char;

			next;
		}
		elsif ( ($char eq '=') && ($count_quotes == 0) )
		{
			# Discard '=' outside quotes.

			$result = $field;
			$field  = '';

			$self -> log(debug => "Result 2: $result.");

			last;
		}
		else
		{
			$field .= $char;
		}

		$previous_char = $char;
	}

	$result = $field if ($field ne '');
	$result =~ s/^"(.+)"$/$1/;

	$self -> log(debug => "Result: $result.");
	$self -> log(debug => "Input:  $input.");

	$input  = substr($input, $char_count);

	$self -> log(debug => "Input:  $input.");

	return ($result, $input);

} # End of attribute_field.

# -----------------------------------------------
# $target is qr/], at the end of a list of attributes.
# The special case is <<...>>, as used in attributes.

sub find_terminator
{
	my($self, $stringref, $target, $start) = @_;
	my(@char)   = split(//, substr($$stringref, $start) );
	my($offset) = 0;
	my($quote)  = '';
	my($angle)  = 0; # Set to 1 if inside <<...>>.

	my($char);

	for my $i (0 .. $#char)
	{
		$char   = $char[$i];
		$offset = $i;

		if ($quote)
		{
			# Ignore an escaped quote.
			# The first 2 backslashes are just to fix syntax highlighting in UltraEdit.

			next if ( ($char =~ /[\]\"\'>]/) && ($i > 0) && ($char[$i - 1] eq '\\') );

			# Get out of quotes if matching one found.

			if ($char eq $quote)
			{
				if ($quote eq '>')
				{
					$quote = '' if (! $angle || ($char[$i - 1] eq '>') );

					next;
				}

				$quote = '';

				next;
			}
		}
		else
		{
			# Look for quotes.
			# 1: Skip escaped chars.

			next if ( ($i > 0) && ($char[$i - 1] eq '\\') );

			# 2: " and '.
			# The backslashes are just to fix syntax highlighting in UltraEdit.

			if ($char =~ /[\"\']/)
			{
				$quote = $char;

				next;
			}

			# 3: <.
			# In the case of attributes ($target eq '}') but not nodes names,
			# quotes can be <...> or <<...>>.

			if ( ($target =~ ']') && ($char =~ '<') )
			{
				$quote = '>';
				$angle = 1 if ( ($i < $#char) && ($char[$i + 1] eq '<') );

				next;
			}

			last if ($char =~ $target);
		}
	}

	return $start + $offset;

} # End of find_terminator.

# --------------------------------------------------

sub log
{
	my($self, $level, $s) = @_;

	$self -> logger -> log($level => $s) if ($self -> logger);

} # End of log.

# --------------------------------------------------

sub process
{
	my($self)   = @_;
	my($string) = $self -> graph_text;
	my($length) = length $string;
	my($level)  = 0;

	$self -> log(info => "Input: $string");

	# We use read()/lexeme_read()/resume() because we pause at each lexeme.

	my($attribute_list);
	my(@event, $event_name);
	my($generic_id, $global_id);
	my($lexeme_name, $lexeme, $literal);
	my($node_port);
	my($span, $start);
	my($type);

	for
	(
		my $pos = $self -> recce -> read(\$string);
		$pos < $length;
		$pos = $self -> recce -> resume($pos)
	)
	{
		$self -> log(debug => "read() => pos: $pos");

		@event          = @{$self -> recce -> events};
		$event_name     = ${$event[0]}[0];
		($start, $span) = $self -> recce -> pause_span;
		$lexeme_name    = $self -> recce -> pause_lexeme;
		$lexeme         = $self -> recce -> literal($start, $span);

		$self -> log(debug => "pause_span($lexeme_name) => start: $start. span: $span. " .
			"lexeme: $lexeme. event: $event_name");

		if ($event_name eq 'close_brace')
		{
			$pos     = $self -> recce -> lexeme_read($lexeme_name);
			$literal = substr($string, $start, $pos - $start);

			$self -> log(debug => "close_brace => '$literal'");
			$self -> process_brace($level, $literal);

			$level--;
		}
		elsif ($event_name eq 'close_bracket')
		{
			$pos     = $self -> recce -> lexeme_read($lexeme_name);
			$literal = substr($string, $start, $pos - $start);

			$self -> log(debug => "close_bracket => '$literal'");
			$self -> process_bracket($literal);
		}
		elsif ($event_name eq 'digraph_literal')
		{
			$pos     = $self -> recce -> lexeme_read($lexeme_name);
			$literal = substr($string, $start, $pos - $start);

			$self -> log(debug => "digraph_literal => '$literal'");
			$self -> process_token('Prolog', 'literal', $literal);
		}
		elsif ($event_name eq 'edge_literal')
		{
			$pos     = $self -> recce -> lexeme_read($lexeme_name);
			$literal = substr($string, $start, $pos - $start);

			$self -> log(debug => "edge_literal => '$literal'");
			$self -> process_token('Graphs', 'edge', $literal);
		}
		elsif ($event_name eq 'generic_id')
		{
			$pos        = $self -> recce -> lexeme_read($lexeme_name);
			$generic_id = substr($string, $start, $pos - $start);
			$type       = 'node_id';

			if ($generic_id =~ /^(?:node|edge|graph)$/)
			{
				$type = 'class';
			}

			$self -> log(debug => "generic_id => '$generic_id'. type => $type");
			$self -> process_token('Graphs', $type, $generic_id);
		}
		elsif ($event_name eq 'global_id')
		{
			$pos       = $self -> recce -> lexeme_read($lexeme_name);
			$global_id = substr($string, $start, $pos - $start);

			$self -> log(debug => "global_id => '$global_id'");
			$self -> process_token('Prolog', 'global_id', $global_id);
		}
		elsif ($event_name eq 'graph_literal')
		{
			$pos     = $self -> recce -> lexeme_read($lexeme_name);
			$literal = substr($string, $start, $pos - $start);

			$self -> log(debug => "graph_literal => '$literal'");
			$self -> process_token('Prolog', 'literal', $literal);
		}
		elsif ($event_name eq 'node_port')
		{
			$pos       = $self -> recce -> lexeme_read($lexeme_name);
			$node_port = substr($string, $start, $pos - $start);

			$self -> log(debug => "node_port => '$literal'");
			$self -> process_token('Graphs', 'node_port', $node_port);
		}
		elsif ($event_name eq 'node_port_compass')
		{
			$pos       = $self -> recce -> lexeme_read($lexeme_name);
			$node_port = substr($string, $start, $pos - $start);

			$self -> log(debug => "node_port_compass => '$literal'");
			$self -> process_token('Graphs', 'node_port_compass', $node_port);
		}
		elsif ($event_name eq 'open_brace')
		{
			$level++;

			$pos     = $self -> recce -> lexeme_read($lexeme_name);
			$literal = substr($string, $start, $pos - $start);

			$self -> log(debug => "open_brace => '$literal'");
			$self -> process_brace($level, $literal);
		}
		elsif ($event_name eq 'open_bracket')
		{
			# Read the open_bracket lexeme, but don't do lexeme_read()
			# at the bottom of the for loop, because we're just about
			# to fiddle $pos to skip the attributes.

			$pos     = $self -> recce -> lexeme_read($lexeme_name);
			$literal = substr($string, $start, $pos - $start);

			$self -> log(debug => "open_bracket => '$literal'");
			$self -> process_bracket($literal);

			$pos = $self -> find_terminator(\$string, qr/]/, $start);

			$attribute_list = substr($string, $start + 1, $pos - $start - 1);

			$self -> log(debug => "index() => attribute list: $attribute_list");
			$self -> process_attribute($level, $attribute_list);
		}
		elsif ($event_name eq 'strict_literal')
		{
			$pos     = $self -> recce -> lexeme_read($lexeme_name);
			$literal = substr($string, $start, $pos - $start);

			$self -> log(debug => "strict_literal => '$literal'");
			$self -> process_token('Prolog', 'literal', $literal);
		}
		else
		{
			die "Unexpected lexeme '$lexeme_name' with a pause\n";
		}
    }

	# Return a defined value for success and undef for failure.

	return $self -> recce -> value;

} # End of process.

# --------------------------------------------------

sub process_attribute
{
	my($self, $level, $input) = @_;

	my($key);
	my($value);

	($key, $input)   = $self -> attribute_field($input);
	($value, $input) = $self -> attribute_field($input);
	my($node)        = Tree::DAG_Node -> new({name => 'attribute', attributes => {type => $key, value => $value} });
	my(@daughters)   = $self -> items -> daughters;
	my($index)       = 1; # Graphs not Prolog.
	@daughters       = $daughters[$index] -> daughters;

	$daughters[$#daughters] -> add_daughter($node);

} # End of process_attribute.

# --------------------------------------------------

sub process_brace
{
	my($self, $level, $name) = @_;
	my($node)      = Tree::DAG_Node -> new({name => $name, attributes => {value => $name} });
	my(@daughters) = $self -> items -> daughters;
	my($index)     = 1; # Graphs not Prolog.

	$daughters[$index] -> add_daughter($node);

} # End of process_brace.

# --------------------------------------------------

sub process_bracket
{
	my($self, $name) = @_;
	my($node)      = Tree::DAG_Node -> new({name => $name, attributes => {value => $name} });
	my(@daughters) = $self -> items -> daughters;
	my($index)     = 1; # Graphs not Prolog.
	@daughters     = $daughters[$index] -> daughters;

	$daughters[$#daughters] -> add_daughter($node);

} # End of process_bracket.

# --------------------------------------------------

sub process_token
{
	my($self, $context, $name, $value) = @_;
	my($node)      = Tree::DAG_Node -> new({name => $name, attributes => {value => $value} });
	my(@daughters) = $self -> items -> daughters;
	my($index)     = $context eq 'Prolog' ? 0 : 1;

	$daughters[$index] -> add_daughter($node);

} # End of process_token.

# --------------------------------------------------

sub run
{
	my($self) = @_;

	if ($self -> description)
	{
		# Assume graph is a single line without comments.

		$self -> graph_text($self -> description);
	}
	elsif ($self -> input_file)
	{
		my($ara_ref) = read_file
						(
							$self -> input_file,
							array_ref  => 1,
							binmode    => ':encoding(UTF-8)',
							chomp      => 1,
						);

		# Discard comments and combine lines into a single string.

		$self -> graph_text(join(' ', grep{! /^#/} @$ara_ref) );
	}
	else
	{
		die "Error: You must provide a graph using one of -input_file or -description\n";
	}

	# Return 0 for success and 1 for failure.

	my($result) = 0;

	try
	{
		if (defined $self -> process)
		{
			$self -> log(info => join("\n", @{$self -> items -> tree2string}) );
		}
		else
		{
			$result = 1;
		}
	}
	catch
	{
		$result = 1;

		$self -> log(error => "Exception: $_");
	};


	$self -> log(error => 'Parse failed') if ($result == 1);

	# Return 0 for success and 1 for failure.

	$self -> log(info => "Parse result: $result (0 is success)");

	return $result;

} # End of run.

# --------------------------------------------------

1;

=pod

=head1 NAME

Graph::Easy::Marpa - A Marpa-based parser for Graph::Easy::Marpa-style Graphviz files

=head1 Synopsis

=head2 Sample Code

	#!/usr/bin/env perl

	use strict;
	use warnings;

	use Graph::Easy::Marpa;

	use Getopt::Long;

	use Pod::Usage;

	# -----------------------------------------------

	my($option_parser) = Getopt::Long::Parser -> new();

	my(%option);

	if ($option_parser -> getoptions
	(
		\%option,
		'description=s',
		'dot_input_file=s',
		'format=s',
		'help',
		'input_file=s',
		'logger=s',
		'maxlevel=s',
		'minlevel=s',
		'output_file=s',
		'rankdir=s',
		'report_tokens=i',
		'token_file=s',
	) )
	{
		pod2usage(1) if ($option{'help'});

		# Return 0 for success and 1 for failure.

		exit Graph::Easy::Marpa -> new(%option) -> run;
	}
	else
	{
		pod2usage(2);
	}

This is shipped as C<scripts/parse.pl>, although the shipped version has built-in help.

Run 'perl -Ilib scripts/parse.pl -h' for sample demos.

=head2 Sample output

Unpack the distro and copy html/*.html and html/*.svg to your web server's doc root directory.

Then, point your browser at 127.0.0.1/index.html.

Or, hit L<http://savage.net.au/Perl-modules/html/graph.easy.marpa/index.html>.

=head2 Modules

=over 4

=item o Graph::Easy::Marpa

The current module, which documents the set of modules.

It uses L<Graph::Easy::Marpa::Parser> and L<Graph::Easy::Marpa::Renderer::GraphViz2>, and 'dot', to
render a C<Graph::Easy::Marpa>-syntax file into a (by default) *.svg file.

See scripts/parse.pl and scripts/parse.sh.

=item o Graph::Easy::Marpa::Parser

See L<Graph::Easy::Marpa::Parser>.

Accepts a graph definition in the Graph::Easy::Marpa language and builds a data structure representing the graph.

See scripts/parse.pl and scripts/parse.sh.

=item o Graph::Easy::Marpa::Renderer::GraphViz2

This is the default renderer, and can output a dot file, suitable for inputting to the C<dot> program.

Also, it can use L<GraphViz2> to call C<dot> and write dot's output to yet another file.

=item o Graph::Easy::Marpa::Actions

This is a file of methods called by L<Marpa::R2> as callbacks, during the parse.

End-users have no need to call any of its methods.

=item o Graph::Easy::Marpa::Config

This manages the config file, which contains a HTML template used by C<scripts/generate.index.pl>.

End-users have no need to call any of its methods.

=item o Graph::Easy::Marpa::Filer

Methods to help with reading sets of files.

End-users have no need to call any of its methods.

=item o Graph::Easy::Marpa::Utils

Methods to help with testing and generating the demo page.

See L<http://savage.net.au/Perl-modules/html/graph.easy.marpa/index.html>.

End-users have no need to call any of its methods.

=back

=head1 Description

L<Graph::Easy::Marpa> provides a L<Marpa>-based parser for C<Graph::Easy::Marpa>-style graph definitions.

Such graph definitions are wrappers around Graphviz's L<DOT|http://www.graphviz.org/content/dot-language> language.
Therefore this module is a pre-processor for DOT files.

The default renderer mentioned above, L<Graph::Easy::Marpa::Renderer::GraphViz2>, can be used to convert the graph
into a image.

See L</Data Files and Scripts> for details.

=head1 Distributions

This module is available as a Unix-style distro (*.tgz).

See L<http://savage.net.au/Perl-modules/html/installing-a-module.html>
for help on unpacking and installing distros.

=head1 Installation

Install L<Graph::Easy::Marpa> as you would for any C<Perl> module:

Run:

	cpanm Graph::Easy::Marpa

or run:

	sudo cpan Graph::Easy::Marpa

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

C<new()> is called as C<< my($parser) = Graph::Easy::Marpa -> new(k1 => v1, k2 => v2, ...) >>.

It returns a new object of type C<Graph::Easy::Marpa>.

Key-value pairs accepted in the parameter list (see corresponding methods for details
[e.g. maxlevel()]):

=over 4

=item o description => $graph_description_string

Specify a string for the graph definition.

You are strongly encouraged to surround this string with '...' to protect it from your shell.

See also the I<input_file> key to read the graph from a file.

The I<description> key takes precedence over the I<input_file> key.

The value for I<description> is passed to L<Graph::Easy::Marpa::Parser>.

Default: ''.

=item o dot_input_file => $file_name

Specify the name of a file that the rendering engine can write to, which will contain the input
to dot (or whatever). This is good for debugging.

If '', the file will not be created.

The value for I<dot_input_file> is passed to L<Graph::Easy::Marpa::Renderer::GraphViz2>.

Default: ''.

=item o format => $format_name

This is the format of the output file, to be created by the renderer.

The value for I<format> is passed to L<Graph::Easy::Marpa::Renderer::GraphViz2>.

Default: 'svg'.

=item o input_file => $graph_file_name

Read the graph definition from this file.

See also the I<description> key to read the graph from the command line.

The whole file is slurped in as a single graph.

The first few lines of the file can start with /^\s*#/, and will be discarded as comments.

The I<description> key takes precedence over the I<input_file> key.

The value for I<input_file> is passed to L<Graph::Easy::Marpa::Parser>.

Default: ''.

=item o logger => $logger_object

Specify a logger object.

The default value triggers creation of an object of type L<Log::Handler> which outputs to the screen.

To disable logging, just set I<logger> to the empty string.

The value for I<logger> is passed to L<Graph::Easy::Marpa::Parser> and to L<Graph::Easy::Marpa::Renderer::GraphViz2>.

Default: undef.

=item o maxlevel => $level

This option is only used if an object of type L<Log::Handler> is created. See I<logger> above.

See also L<Log::Handler::Levels>.

The value for I<maxlevel> is passed to L<Graph::Easy::Marpa::Parser> and to L<Graph::Easy::Marpa::Renderer::GraphViz2>.

Default: 'info'. A typical value is 'debug'.

=item o minlevel => $level

This option is only used if an object of type L<Log::Handler> is created. See I<logger> above.

See also L<Log::Handler::Levels>.

The value for I<minlevel> is passed to L<Graph::Easy::Marpa::Parser> and to L<Graph::Easy::Marpa::Renderer::GraphViz2>.

Default: 'error'.

No lower levels are used.

=item o output_file => $output_file_name

If an output file name is supplied, and a rendering object is also supplied, then this call is made:

	$self -> renderer -> run(format => $self -> format, items => [$self -> items -> print], output_file => $file_name);

This is how the plotted graph is actually created.

The value for I<output_file> is passed to L<Graph::Easy::Marpa::Renderer::GraphViz2>.

Default: ''.

=item o rankdir => $direction

$direction must be one of: LR or RL or TB or BT.

Specify the rankdir of the graph as a whole.

The value for I<rankdir> is passed to L<Graph::Easy::Marpa::Renderer::GraphViz2>.

Default: 'TB'.

=item o renderer => $renderer_object

This is the object whose run() method will be called to render the result of parsing
the input graph.

The format of the parameters passed to the renderer are documented in L<Graph::Easy::Marpa::Renderer::GraphViz2/run(%arg)>,
which is the default value for this object.

Default: ''.

=item o report_tokens => $Boolean

Reports, via the log, the tokens recognized by the parser.

The value for I<report_tokens> is passed to L<Graph::Easy::Marpa::Parser>.

Default: 0.

=item o token_file => $token_file_name

This is the name of the file to write containing the tokens (items) output from L<Graph::Easy::Marpa::Parser>.

The value for I<token_file> is passed to L<Graph::Easy::Marpa::Parser>.

Default: 0.

=back

=head1 Data Files and Scripts

=head2 Overview of the Data Flow

The parser works like this:

=over 4

=item o You use the parser Graph::Easy::Marpa::Parser directly ...

Call C<< Graph::Easy::Marpa::Parser -> new(%options) >>.

=item o ... or, you use Graph::Easy::Marpa, which calls the parser and then the renderer

Call C<< Graph::Easy::Marpa -> new(%options) >>.

Of course, the renderer is only called if the parser exits without error.

=back

Details:

=over 4

=item o Input a graph definition

This comes from the I<description> parameter to new(), or is read from a file with the I<input_file> parameter.

See new(input_file => $graph_file_name) or new(description => $graph_string) above for details.

A definition looks like '[node.1]{a:b;c:d}->{e:f;}->{g:h}[node.2]{i:j}->[node.3]{k:l}'.

Here, node names are: node.1, node.2 and node.3.

Edge names are: '->' for directed graphs, or '--' for undirected graphs.

Nodes and edges can have attributes, very much like CSS. The attributes in this sample are meaningless,
and are just to demonstrate the syntax.

And yes, unlike the original L<Graph::Easy> syntax, you can use a series of edges between 2 nodes,
with different attributes, as above.

See L<http://www.graphviz.org/content/attrs> for a long list of the attributes available for Graphviz.

=item o Parse the graph

After the parser runs successfully, the parser object holds a L<Set::Array> object of tokens representing the graph.

See L<Graph::Easy::Marpa::Parser/How is the parsed graph stored in RAM?> for details.

=item o Output the parsed tokens

See new(token_file => $csv_file_name) above for details.

=item o Call the renderer

=back

=head2 Data and Script Interaction

Sample input files for the parser are in data/*.ge. Sample output files are in data/*.tokens.

=over 4

=item o scripts/parse.pl and scripts/parser.sh

These use L<Graph::Easy::Marpa::Parser>.

They run the parser on one *.ge input file, and produce an arrayref of items.

Run scripts/parse.pl -h for samples of how to drive it.

Try:

	cat data/node.05.ge
	perl -Ilib scripts/parse.pl -i data/node.05.ge -t data/node.05.tokens -re 1

You can use scripts/parse.sh to simplify this process:

	scripts/parse.sh data/node.05.ge data/node.05.tokens -re 1
	scripts/parse.sh data/subgraph.12.ge data/subgraph.12.tokens -re 1

=back

=head1 Methods

=head2 description([$graph_description_string])

Here, the [] indicate an optional parameter.

Get or set the string for the graph definition.

See also the input_file() method to read the graph from a file, below.

The value supplied to the description() method takes precedence over the value read from the input file.

=head2 dot_input_file([$file_name])

Here, the [] indicate an optional parameter.

Get or set the name of the file into which the rendering engine will write to input to dot (or whatever).

=head2 format([$format])

Here, the [] indicate an optional parameter.

Get or set the format of the output file, to be created by the renderer.

=head2 input_file([$graph_file_name])

Here, the [] indicate an optional parameter.

Get or set the name of the file to read the graph definition from.

See also the description() method.

The whole file is slurped in as a single graph.

The first few lines of the file can start with /^\s*#/, and will be discarded as comments.

The value supplied to the description() method takes precedence over the value read from the input file.

=head2 log($level, $s)

Calls $self -> logger -> log($level => $s) if ($self -> logger).

Up until V 1.11, this used to call $self -> logger -> $level($s), but the change was made to allow
simpler loggers, meaning they did not have to implement all the methods covered by $level().
See CHANGES for details. For more on log levels, see L<Log::Handler::Levels>.

=head2 logger([$logger_object])

Here, the [] indicate an optional parameter.

Get or set the logger object.

To disable logging, just set logger to the empty string.

This logger is passed to L<Graph::Easy::Marpa::Parser> and L<Graph::Easy::Marpa::Renderer::GraphViz2>.

=head2 maxlevel([$string])

Here, the [] indicate an optional parameter.

Get or set the value used by the logger object.

This option is only used if an object of type L<Log::Handler> is created. See L<Log::Handler::Levels>.

=head2 minlevel([$string])

Here, the [] indicate an optional parameter.

Get or set the value used by the logger object.

This option is only used if an object of type L<Log::Handler> is created. See L<Log::Handler::Levels>.

=head2 output_file([$output_file_name])

Here, the [] indicate an optional parameter.

Get or set the name of the file to which the renderer will write to resultant graph.

This is how the plotted graph is actually created.

If no renderer is supplied, or no output file is supplied, nothing is written.

=head2 rankdir([$direction])

Here, the [] indicate an optional parameter.

Get or set the rankdir of the graph as a whole.

The default is 'TB' (top to bottom).

=head2 renderer([$rendering_object])

Here, the [] indicate an optional parameter.

Get or set the rendering object.

This is the object whose run() method will be called to render the result of parsing the input file.

The format of the parameters passed to the renderer are documented in L<Graph::Easy::Marpa::Renderer::GraphViz2/run(%arg)>,
which is the default value for this object.

=head2 report_tokens([$Boolean])

Here, the [] indicate an optional parameter.

Get or set the flag to report, via the log, the items recognized in the tokens file.

Calls L<Graph::Easy::Marpa::Parser/report()> to do the reporting.

=head2 tokens_file([$token_file_name])

Here, the [] indicate an optional parameter.

Get or set the name of the file to write containing the tokens (items) output from L<Graph::Easy::Marpa::Parser>.

=head1 FAQ

=head2 Has anything changed moving from V 1.* to V 2.*?

Yes:

=over 4

=item o Input file naming

The test data files are shipped as data/*.ge.

Of course, you can use any input file name you wish.

=item o Output file naming

The output files of parsed tokens are shipped as data/*.tokens.

Of course, you can use any output file name you wish.

=item o Output files

The output files, data/*.dot, are now shipped.

=back

=head2 What is the homepage of Marpa?

L<http://jeffreykegler.github.io/Ocean-of-Awareness-blog/>.

=head2 How do I reconcile Marpa's approach with classic lexing and parsing?

I've included in
L<this article|http://savage.net.au/Ron/html/Conditional.preservation.of.whitespace.html#Constructing_a_Mental_Picture_of_Lexing_and_Parsing>
a section which is aimed at helping us think about this issue.

=head2 What is the purpose of this set of modules?

It's a complete re-write of L<Graph::Easy>, designed to make on-going support for the C<Graph::Easy::Marpa> language
much, much easier.

=head2 What are Graph::Easy::Marpa graphs?

In short, it means a text string containing a definition of a graph, using a cleverly designed language,
that can be used to describe the sort of graph you wish to plot. Then, L<Graph::Easy::Marpa> does the plotting
by calling L<Graph::Easy::Marpa::Renderer::GraphViz2>.

See L<Graph::Easy::Marpa::Parser/What is the Graph::Easy::Marpa language?>.

=head2 What do Graph::Easy::Marpa graph definitions look like?

	[node_1]{color: red; style: circle} -> {class: fancy;} [node_2]{color: green;}

=head2 How are graphs stored in RAM by the parser?

See L<Graph::Easy::Marpa::Parser/FAQ>.

=head2 How are attributes assigned to nodes and edges?

Since the scan of the input stream is linear, any attribute detected belongs to the nearest preceeding
node(s) or edge.

=head2 How are attributes assigned to groups?

The only attributes which can be passed to a subgraph (group) are those that 'dot' accepts under the 'graph'
part of a subgraph definition.

This means the attribute 'rank' cannot be passed, yet.

=head2 Is there sample data I can examine?

See data/*.ge and the corresponding data/*.tokens and html/*.svg.

Note: Some files contain deliberate mistakes. See above for scripts/parse.pl and scripts/parse.sh.

=head2 What about the fact the Graph::Easy can read various other definition formats?

I have no plans to support such formats. Nevertheless, having written these modules, it should be fairly
easy to derive classes which perform that sort of work.

=head2 How to I re-generate the web page of demos?

See scripts/generate.index.pl.

=head2 What are the defaults for GraphViz2, the default rendering engine?

	 GraphViz2 -> new
	 (
	  edge    => $class{edge}   || {color => 'grey'},
	  global  => $class{global} || {directed => 1},
	  graph   => $class{graph}  || {rankdir => $self -> rankdir},
	  logger  => $self -> logger,
	  node    => $class{node} || {shape => 'oval'},
	  verbose => 0,
	 )

where $class($name) is taken from the class declarations at the start of the input stream.

=head1 Machine-Readable Change Log

The file Changes was converted into Changelog.ini by L<Module::Metadata::Changes>.

=head1 Version Numbers

Version numbers < 1.00 represent development versions. From 1.00 up, they are production versions.

=head1 Thanks

Many thanks are due to the people who worked on L<Graph::Easy>.

Jeffrey Kegler wrote L<Marpa>, and has been helping me via private emails.

=head1 Support

Email the author, or log a bug on RT:

L<https://rt.cpan.org/Public/Dist/Display.html?Name=Graph::Easy::Marpa>.

=head1 Author

L<Graph::Easy::Marpa> was written by Ron Savage I<E<lt>ron@savage.net.auE<gt>> in 2011.

Home page: L<http://savage.net.au/index.html>.

=head1 Copyright

Australian copyright (c) 2011, Ron Savage.

	All Programs of mine are 'OSI Certified Open Source Software';
	you can redistribute them and/or modify them under the terms of
	The Artistic License, a copy of which is available at:
	http://www.opensource.org/licenses/index.html

=cut
