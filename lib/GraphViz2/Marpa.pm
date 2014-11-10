package GraphViz2::Marpa;

use strict;
use utf8;
use warnings;
use warnings  qw(FATAL utf8);    # Fatalize encoding glitches.
use open      qw(:std :utf8);    # Undeclared streams in UTF-8.

use File::Slurp; # For read_file().

use GraphViz2::Marpa::Renderer::Graphviz;

use Log::Handler;

use Marpa::R2;

use Moo;

use Tree::DAG_Node;

use Types::Standard qw/Any ArrayRef Int HashRef Str/;

use Try::Tiny;

has bnf =>
(
	default  => sub{return ''},
	is       => 'rw',
	isa      => Any,
	required => 0,
);

has brace_count =>
(
	default  => sub{return 0},
	is       => 'rw',
	isa      => Int,
	required => 0,
);

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

has known_events =>
(
	default  => sub{return {} },
	is       => 'rw',
	isa      => HashRef,
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
	default  => sub{return 'notice'},
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

has stack =>
(
	default  => sub{return []},
	is       => 'rw',
	isa      => ArrayRef,
	required => 0,
);

has trace_terminals =>
(
	default  => sub{return 0},
	is       => 'rw',
	isa      => Int,
	required => 0,
);

has tree =>
(
	default  => sub{return ''},
	is       => 'rw',
	isa      => Any,
	required => 0,
);

has uid =>
(
	default  => sub{return 0},
	is       => 'rw',
	isa      => Int,
	required => 0,
);

our $VERSION = '2.00';

# ------------------------------------------------

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

	# Policy: Event names are always the same as the name of the corresponding lexeme.

	$self -> bnf
	(
<<'END_OF_GRAMMAR'

:default				::= action => [values]

lexeme default			=  latm => 1		# Longest Acceptable Token Match.

:start					::= graph_definition

graph_definition		::= prolog_tokens graph_statement

prolog_tokens			::= strict_token graph_type global_id_type

strict_token			::=
strict_token			::= strict_literal

graph_type				::= digraph_literal
							| graph_literal

global_id_type			::=
global_id_type			::= node_name

# Graph stuff.

graph_statement			::= open_brace statement_list close_brace

statement_list			::= statement_token*

statement_token			::= statement statement_terminator

# Statement stuff.

statement				::= node_statement
							| edge_statement
							| subgraph_statement

statement_terminator	::= semicolon_literal
statement_terminator	::=

# Node stuff

node_statement			::= node_name
							| node_name attribute_statement
							| assignment_statement

# Edge stuff

edge_statement			::= edge_lhs edge_rhs
							| edge_lhs edge_rhs attribute_statement

edge_lhs				::= node_name
							| subgraph_statement

edge_rhs				::= edge_name edge_lhs
							| edge_name edge_lhs edge_rhs

edge_name				::= directed_edge
							| undirected_edge

# Attribute and Assignment stuff.

attribute_statement		::= open_bracket assignment_statements close_bracket

assignment_statements	::= assignment_statement*

assignment_statement	::= attribute_name ('=') attribute_value

# Subgraph stuff.
# Subgraphs are handled by the statement type 'graph_statement_tokens'.
# Hence subgraph sub_1 {...} and subgraph {...} and sub_1 {...} and {...} are all output as:
# o The optional literal 'subgraph', which is classified as a literal.
# o The optional subgraph id, which is classified as a node_id.
# o The literal '{'.
# o The body of the subgraph.
# o The literal '}'.

subgraph_statement		::= subgraph_prefix subgraph_id_token graph_statement

subgraph_prefix			::=
subgraph_prefix			::= subgraph_literal

subgraph_id_token		::=
subgraph_id_token		::= node_name

# Lexemes in alphabetical order.
# Quoted string handling copied from Marpa::R2's metag.bnf.

:lexeme					~ attribute_name		pause => before		event => attribute_name
attribute_name			~ string

:lexeme					~ attribute_value		pause => before		event => attribute_value
attribute_value			~ string

:lexeme					~ close_brace			pause => before		event => close_brace
close_brace				~ '}'

:lexeme					~ close_bracket			pause => before		event => close_bracket
close_bracket			~ ']'
close_bracket			~ '];'

:lexeme					~ digraph_literal		pause => before		event => digraph_literal
digraph_literal			~ 'digraph':i

:lexeme					~ directed_edge			pause => before		event => directed_edge
directed_edge			~ '->'

double_quoted_char_set	~ double_quoted_char+
double_quoted_char		~ escaped_char
							|  [^\"\x{0A}\x{0B}\x{0C}\x{0D}\x{0085}\x{2028}\x{2029}]

escaped_char			~ '\' [[:print:]]

# Use ' here just for the UltraEdit syntax hiliter.

:lexeme					~ graph_literal			pause => before		event => graph_literal
graph_literal			~ 'graph':i

html_quoted_char_set	~ html_quoted_char+
html_quoted_char		~ escaped_char
							| [^\x{0A}\x{0B}\x{0C}\x{0D}\x{0085}\x{2028}\x{2029}]

:lexeme					~ node_name				pause => before		event => node_name
node_name				~ string

:lexeme					~ open_brace			pause => before		event => open_brace
open_brace				~ '{'

:lexeme					~ open_bracket			pause => before		event => open_bracket
open_bracket			~ '['

semicolon_literal		~ ';'

#single_quoted_char_set	~ single_quoted_char+
#single_quoted_char		~ escaped_char
#							[^\'\x{0A}\x{0B}\x{0C}\x{0D}\x{0085}\x{2028}\x{2029}]

:lexeme					~ strict_literal		pause => before		event => strict_literal
strict_literal			~ 'strict':i

# Single-quoted strings will work, but they are not supported in the
# DOT language. See http://www.graphviz.org/content/dot-language.

string					~ [\"]	double_quoted_char_set	[\"]
string					~ [<]	html_quoted_char_set	[>]
#string					~ [\']	single_quoted_char_set	[\']
string					~ unquoted_char_set

:lexeme					~ subgraph_literal		pause => before		event => subgraph_literal
subgraph_literal		~ 'subgraph':i

:lexeme					~ undirected_edge		pause => before		event => undirected_edge
undirected_edge			~ '--'

# The '=' is necessary for cases like: 'name=value' in node_1 [name=value].

unquoted_char_set		~ unquoted_char+
unquoted_char			~ escaped_char
							| [^\s\[\]=]

# Boilerplate.

:discard				~ separators
separators				~ [;,]

:discard				~ whitespace
whitespace				~ [\s]+

:discard				~ <C style comment>
:discard				~ <Cplusplus style comment>
:discard				~ <hash style comment>

# C and C++ comment handling copied from MarpaX::Languages::C::AST.

<C style comment>					~ '/*' <comment interior> '*/'

<comment interior>					~ <optional non stars> <optional star prefixed segments> <optional pre final stars>

<optional non stars>				~ [^*]*
<optional star prefixed segments>	~ <star prefixed segment>*
<star prefixed segment>				~ <stars> [^/*] <optional star free text>
<stars>								~ [*]+
<optional star free text>			~ [^*]*
<optional pre final stars>			~ [*]*

<Cplusplus style comment>			~ '//' <Cplusplus comment interior>
<Cplusplus comment interior>		~ [^\n]*

# Hash comment handling copied from Marpa::R2's metag.bnf.

<hash style comment>				~ <terminated hash comment>
										| <unterminated final hash comment>

<terminated hash comment>			~ '#' <hash comment body> <vertical space char>

<unterminated final hash comment>	~ '#' <hash comment body>

<hash comment body>					~ <hash comment char>*

<vertical space char>				~ [\x{0A}\x{0B}\x{0C}\x{0D}\x{2028}\x{2029}]

<hash comment char>					~ [^\x{0A}\x{0B}\x{0C}\x{0D}\x{2028}\x{2029}]

END_OF_GRAMMAR
	);

	$self -> grammar
	(
		Marpa::R2::Scanless::G -> new
		({
			source => \$self -> bnf
		})
	);

	$self -> recce
	(
		Marpa::R2::Scanless::R -> new
		({
			grammar         => $self -> grammar,
			ranking_method  => 'high_rule_only',
			trace_terminals => $self -> trace_terminals,
		})
	);

	my(%event);

	for my $line (split(/\n/, $self -> bnf) )
	{
		$event{$1} = 1 if ($line =~ /event\s+=>\s+(\w+)/);
	}

	$self -> known_events(\%event);

	# Since $self -> tree has not been initialized yet,
	# we can't call our _add_daughter() until after this statement.

	$self -> tree(Tree::DAG_Node -> new({name => 'root', attributes => {uid => 0} }));
	$self -> stack([$self -> tree -> root]);

	for my $name (qw/prolog graph/)
	{
		$self -> _add_daughter($name, {});
	}

	# The 'prolog' daughter is the parent of all items in the prolog,
	# so it gets pushed onto the stack.
	# Later, when 'digraph' or 'graph' is encountered, the 'graph' daughter replaces it.

	my(@daughters) = $self -> tree -> daughters;
	my($index)     = 0; # 0 => prolog, 1 => graph.
	my($stack)     = $self -> stack;

	push @$stack, $daughters[$index];

	$self -> stack($stack);

} # End of BUILD.

# ------------------------------------------------

sub _add_daughter
{
	my($self, $name, $attributes)  = @_;
	$$attributes{uid} = $self -> uid($self -> uid + 1);
	my($node)         = Tree::DAG_Node -> new({name => $name, attributes => $attributes});
	my($stack)        = $self -> stack;

	$$stack[$#$stack] -> add_daughter($node);

} # End of _add_daughter.

# ------------------------------------------------

sub clean_after
{
	my($self, $s) = @_;

	# The grammar allows things like '"xyz",', so clean them up.
	# Note: You can't use (?:[\"\']) here!

	if ($s =~ /^([\"\'])(.*)\1,$/)
	{
		$s = $2;
	}
	elsif ($s =~ /^(.*),$/)
	{
		$s = $1;
	}

	$s =~ s/^\s+//;
	$s =~ s/\s+$//;
	$s =~ s/^([\"\'])(.*)\1$/$2/; # The backslashes are just for the UltraEdit syntax hiliter.

	return $s;

} # End of clean_after.

# ------------------------------------------------

sub clean_before
{
	my($self, $s) = @_;

	$s =~ s/\s*;\s*$//;
	$s =~ s/^\s+//;
	$s =~ s/\s+$//;
	$s =~ s/^(<)\s+/$1/;
	$s =~ s/\s+(>)$/$1/;

	return $s;

} # End of clean_before.

# ------------------------------------------------

sub _dump_stack
{
	my($self, $caller) = @_;

	$self -> log(debug => "\tStack @ $caller");

	for my $item (@{$self -> stack})
	{
		$self -> log(debug => "\tName: " . $item -> name);
	}

	$self -> log(info => join("\n", @{$self -> tree -> tree2string}) );

} # End of _dump_stack.

# ------------------------------------------------

sub hashref2string
{
	my($self, $hashref) = @_;
	$hashref ||= {};

	return '{' . join(', ', map{qq|$_ => "$$hashref{$_}"|} sort keys %$hashref) . '}';

} # End of hashref2string.

# ------------------------------------------------

sub _identify_lexeme
{
	my($self, $string, $start, $span, $lexeme) = @_;

	pos($string) = $start + $span;
	$string      =~ /\G\s*(\S)/ || return;
	my($literal) = $1;
	my($type)    = ($literal eq '=') ? 'attribute_name' : 'node_name';

	$self -> log(debug => "Disambiguated lexeme (2 of 2) |$lexeme| as '$type'");

	return $type;

} # End of _identify_lexeme.

# ------------------------------------------------

sub log
{
	my($self, $level, $s) = @_;

	$self -> logger -> log($level => $s) if ($self -> logger);

} # End of log.

# ------------------------------------------------

sub _post_process
{
	my($self) = @_;

	# Walk the tree, find the edges, and stockpile uids of interest.
	# The Tree::DAG_Node docs warn against modifying the tree during a walk,
	# so we use a hash to track uids found, and post-process them.

	my($attributes);
	my($uid, %uid);

	$self -> tree -> walk_down
	({
		callback => sub
		{
			my($node) = @_;
			my($name) = $node -> name;

			# Stash uids for later processing.

			if ($name eq 'edge_id')
			{
				$attributes  = $node -> mother -> attributes;
				$uid         = $$attributes{uid};
				$uid{$uid}   = $node;
			}

			# Keep walking.

			return 1;
		},
		_depth => 0,
	});

} # End of _post_process.

# ------------------------------------------------

sub _process
{
	my($self)          = @_;
	my($string)        = $self -> clean_before($self -> graph_text);
	my($length)        = length $string;
	my($format)        = '%-20s    %5s    %5s    %5s    %-s';
	my($last_event)    = '';
	my($prolog_token)  = qr/(?:digraph_literal|graph_literal|strict_literal)/;
	my($pos)           = 0;
	my(%class)         =
	(
		edge     => 'class',
		graph    => 'class',
		node     => 'class',
		subgraph => 'literal',
	);

	$self -> log(debug => "Length of input: $length");
	$self -> log(debug => sprintf($format, 'Event', 'Start', 'Span', 'Pos', 'Lexeme') );

	my($event_name);
	my(@fields);
	my($lexeme);
	my($node_name);
	my($span, $start);
	my($temp, $type);

	# We use read()/lexeme_read()/resume() because we pause at each lexeme.
	# Also, in read(), we use $pos and $length to avoid reading Ruby Slippers tokens (if any).

	for
	(
		$pos = $self -> recce -> read(\$string, $pos, $length);
		$pos < $length;
		$pos = $self -> recce -> resume($pos)
	)
	{
		($start, $span) = $self -> recce -> pause_span;
		$event_name     = $self -> _validate_event($string, $start, $span);
		$lexeme         = $self -> recce -> literal($start, $span);
		$pos            = $self -> recce -> lexeme_read($event_name);

		die "lexeme_read() rejected lexeme |$lexeme|\n" if (! defined $pos);

		$self -> log(debug => sprintf($format, $event_name, $start, $span, $pos, $lexeme) );

		if ($event_name eq 'attribute_name')
		{
			# Special case.

			if (substr($lexeme, 0, 1) eq '[')
			{
				$event_name = 'open_bracket';
				$temp       = '[';

				$self -> log(debug => sprintf($format, $event_name, $start, 1, $pos, $temp) );
				$self -> _process_bracket($temp, $event_name);

				substr($lexeme, 0, 1) = '';
				$fields[0]            = $self -> clean_after($lexeme);
			}
			else
			{
				$fields[0] = $self -> clean_after($lexeme);
			}
		}
		elsif ($event_name eq 'attribute_value')
		{
			# Special cases.
			# Handle ']' and '];'.

			$temp                  = '';
			substr($lexeme, -1, 1) = '' if (substr($lexeme, -1, 1) eq ';');

			if (substr($lexeme, -1, 1) eq ']')
			{
				$temp                  = ']';
				substr($lexeme, -1, 1) = '';
			}

			$lexeme = $self -> clean_after($lexeme);

			$self -> _add_daughter('attribute', {type => $fields[0], value => $lexeme});

			@fields = ();

			if ($temp)
			{
				$event_name = 'close_bracket';

				$self -> log(debug => sprintf($format, $event_name, $start, 1, $pos, $temp) );
				$self -> _process_bracket($temp, $event_name);
			}
		}
		elsif ($event_name eq 'close_brace')
		{
			$self -> _process_brace($lexeme, $event_name);
		}
		elsif ($event_name eq 'close_bracket')
		{
			# Special case.

			substr($lexeme, -1, 1) = '' if (substr($lexeme, -1, 1) eq ';');

			$self -> _process_bracket($lexeme, $event_name);
		}
		elsif ($event_name eq 'directed_edge')
		{
			$self -> _process_token('edge_id', $event_name, $lexeme);
		}
		elsif ($event_name eq 'node_name')
		{
			# Special case.

			next if ($lexeme eq ';');

			$lexeme = $self -> clean_after($lexeme);
			$type   = $class{$lexeme} ? $class{$lexeme} : 'node_id';

			$self -> _add_daughter($type, {value => $lexeme});
		}
		elsif ($event_name eq 'open_brace')
		{
			$self -> _process_brace($lexeme, $event_name);
		}
		elsif ($event_name eq 'open_bracket')
		{
			$self -> _process_bracket($lexeme, $event_name);
		}
		elsif ($event_name =~ $prolog_token)
		{
			$self -> _process_prolog_token($event_name, $lexeme);
		}
		elsif ($event_name =~ 'subgraph_literal')
		{
			$self -> _process_token('literal', $event_name, $lexeme);
		}
		elsif ($event_name eq 'undirected_edge')
		{
			$self -> _process_token('edge_id', $event_name, $lexeme);
		}

		$last_event = $event_name;

		#$self -> log(debug => join("\n", @{$self -> tree -> tree2string}) );
    }

	if (my $ambiguous_status = $self -> recce -> ambiguous)
	{
		$self -> log(notice => 'Terminals expected: ' . join(', ', @{$self -> recce -> terminals_expected}) );
		$self -> log(notice => "Parse is ambiguous. Status: $ambiguous_status");
	}

	# Return a defined value for success and undef for failure.

	return $self -> recce -> value;

} # End of _process.

# ------------------------------------------------

sub _process_brace
{
	my($self, $name, $event_name) = @_;

	# When the 1st '{' is encountered, the 'Graph' daughter of the root
	# becomes the parent of all other tree nodes, replacing the prolog' daughter.

	if ($self -> brace_count == 0)
	{
		my($stack) = $self -> stack;

		pop @$stack;

		my(@daughters) = $self -> tree -> daughters;
		my($index)     = 1; # 0 => prolog, 1 => graph.

		push @$stack, $daughters[$index];

		$self -> stack($stack);
	}

	# When a '{' is encountered, the last thing pushed becomes it's parent.
	# Likewise, when a '}' is encountered, we pop the stack.

	my($stack) = $self -> stack;

	if ($name eq '{')
	{
		$self -> brace_count($self -> brace_count + 1);
		$self -> _add_daughter('literal', {type => $event_name, value => $name});

		my(@daughters) = $$stack[$#$stack] -> daughters;

		push @$stack, $daughters[$#daughters];
	}
	else
	{
		pop @$stack;

		$self -> stack($stack);
		$self -> _add_daughter('literal', {type => $event_name, value => $name});
		$self -> brace_count($self -> brace_count - 1);
	}

} # End of _process_brace.

# ------------------------------------------------

sub _process_bracket
{
	my($self, $name, $event_name) = @_;

	# When a '[' is encountered, the last thing pushed becomes it's parent.
	# Likewise, if ']' is encountered, we pop the stack.

	my($stack) = $self -> stack;

	if ($name eq '[')
	{
		my(@daughters) = $$stack[$#$stack] -> daughters;

		push @$stack, $daughters[$#daughters];

		$self -> _process_token('literal', $event_name, $name);
	}
	else
	{
		$self -> _process_token('literal', $event_name, $name);

		pop @$stack;

		$self -> stack($stack);
	}

} # End of _process_bracket.

# ------------------------------------------------

sub _process_prolog_token
{
	my($self, $event_name, $value) = @_;

	$self -> _add_daughter('literal', {type => $event_name, value => lc $value});

	# When 'digraph' or 'graph' is encountered, the 'graph' daughter of the root
	# becomes the parent of all other tree nodes, replacing the 'prolog' daughter.

	if ($event_name ne 'strict_literal')
	{
		my($stack) = $self -> stack;

		pop @$stack;

		my(@daughters) = $self -> tree -> daughters;
		my($index)     = 1; # 0 => prolog, 1 => graph.

		push @$stack, $daughters[$index];

		$self -> stack($stack);
	}

} # End of _process_prolog_token.

# ------------------------------------------------

sub _process_token
{
	my($self, $name, $event_name, $value) = @_;

	$self -> _add_daughter($name, {type => $event_name, value => $value});

} # End of _process_token.

# ------------------------------------------------

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
		# Quick removal of whole-line C++ and hash comments.

		$self -> graph_text(join(' ', grep{! m!^(?:#|//)!} read_file($self -> input_file, binmode => ':encoding(utf-8)') ) );
	}
	else
	{
		die "You must provide a graph using one of -input_file or -description. \n";
	}

	# Return 0 for success and 1 for failure.

	my($result) = 0;

	try
	{
		if (defined (my $value = $self -> _process) )
		{
			# At this stage, _post_process() does nothing.

			#$self -> _post_process;
			$self -> log(info => join("\n", @{$self -> tree -> tree2string}) );
		}
		else
		{
			$result = 1;

			$self -> log(error => 'Parse failed');
		}
	}
	catch
	{
		$result = 1;

		$self -> log(error => "Parse failed. Error: $_");
	};

	$self -> log(info => "Parse result:  $result (0 is success)");

	if ($result == 0)
	{
		# Clean up the stack by popping the root node.

		my($stack) = $self -> stack;

		pop @$stack;

		$self -> stack($stack);
		$self -> log(debug => 'Brace count:   ' . $self -> brace_count . ' (0 is success)');
		$self -> log(debug => 'Stack size:    ' . $#{$self -> stack} . ' (0 is success)');

		my($output_file) = $self -> output_file;

		if ($output_file)
		{
			$self -> renderer
			(
				GraphViz2::Marpa::Renderer::Graphviz -> new
				(
					logger      => $self -> logger,
					maxlevel    => $self -> maxlevel,
					minlevel    => $self -> minlevel,
					output_file => $self -> output_file,
					tree        => $self -> tree,
				)
			) if (! $self -> renderer);

			$self -> renderer -> run;
			$self -> log(info => "Rendered file: $output_file");
		}
	}

	# Return 0 for success and 1 for failure.

	return $result;

} # End of run.

# ------------------------------------------------

sub _validate_event
{
	my($self, $string, $start, $span) = @_;
	my(@event)         = @{$self -> recce -> events};
	my($event_count)   = scalar @event;
	my(@event_name)    = sort map{$$_[0]} @event;
	my($event_name)    = $event_name[0]; # Default.
	my($lexeme)        = substr($string, $start, $span);
	my($line, $column) = $self -> recce -> line_column($start);
	my($literal)       = substr($string, $start + $span, 20);
	$literal           =~ tr/\n/ /;
	$literal           =~ s/^\s+//;
	$literal           =~ s/\s+$//;
	my($message)       = "Location: ($line, $column). Lexeme: |$lexeme|. Next few chars: |$literal|";
	$message           = "$message. Events: $event_count. Names: ";

	$self -> log(debug => $message . join(', ', @event_name) . '.');

	my(%event_name);

	@event_name{@event_name} = (1) x @event_name;

	for (@event_name)
	{
		die "Unexpected event name '$_'" if (! ${$self -> known_events}{$_});
	}

	if ($event_count > 1)
	{
		my(%special_case) =
		(
			'}'        => 'close_brace',
			']'        => 'close_bracket',
			'];'       => 'close_bracket',
			'->'       => 'directed_edge',
			'{'        => 'open_brace',
			'['        => 'open_bracket',
			'subgraph' => 'subgraph_literal',
			'--'       => 'undirected_edge',
		);

		if ($special_case{$lexeme})
		{
			$event_name = $special_case{$lexeme};

			$self -> log(debug => "Disambiguated lexeme (1 of 2) |$lexeme| as '$event_name'");
		}
		elsif ($event_count == 2)
		{
			# We can handle ambiguous events when they are 'attribute_name' and 'node_name'.
			# 'attribute_name' is followed by '=', and 'node_name' is followed by anything else.
			# In fact, 'node_name' may be folowed by '[' to indicate the start of its attributes.

			$event_name   = undef;
			my($expected) = "$event_name[0]!$event_name[1]";

			if ($expected eq 'attribute_name!node_name')
			{
				$event_name = $self -> _identify_lexeme($string, $start, $span, $lexeme);
			}

			if (! defined $event_name)
			{
				die "Cannot identify lexeme as either 'attribute_name' or 'node_name'. \n";
			}
		}
		else
		{
			die "The code only handles 1 event at a time, or the pair ('attribute_name', 'node_name'). \n";
		}
	}

	return $event_name;

} # End of _validate_event.

# ------------------------------------------------

1;

=pod

=head1 NAME

C<GraphViz2::Marpa> - A Marpa-based parser for Graphviz C<dot> files

=head1 Synopsis

=over 4

=item o Display help

	perl scripts/g2m.pl -h

=item o Run the parser

	perl scripts/g2m.pl -input_file data/16.gv
	perl scripts/g2m.pl -input_file data/16.gv -max info

The L</FAQ> discusses the way the parsed data is stored in RAM.

=item o Run the parser and the default renderer

	perl scripts/g2m.pl -input_file data/16.gv -output_file ./16.gv

./16.gv will be the rendered Graphviz C<dot> file.

See scripts/test.utf8.sh for comparing the output of running the parser, and C<dot>, on all
data/utf8.*.gv files.

=back

See also L</Scripts>.

=head1 Description

L<GraphViz2::Marpa> provides a L<Marpa::R2>-based parser for L<Graphviz|http://www.graphviz.org/>
graph definitions.

Demo output: L<http://savage.net.au/Perl-modules/html/graphviz2.marpa/index.html>.

L<Marpa's homepage|http://savage.net.au/Marpa.html>.

Articles:

=over 4

=item o Overview

L<Announcing this module|http://savage.net.au/Ron/html/A.New.Marpa-based.Parser.for.GraphViz.html>

=item o Building the Grammar

L<Conditional preservation of whitespace|http://savage.net.au/Ron/html/Conditional.preservation.of.whitespace.html>

This module will be re-written, again, now that its BNF has been incorporated into GraphViz2::Marpa,
and patched along the way.

=back

=head1 Modules

=over 4

=item o L<GraphViz2::Marpa>

The current module, which documents the set of modules.

It can, optionally, use the default renderer L<GraphViz2::Marpa::Renderer::Graphviz>.

Accepts a L<Graphviz|http://www.graphviz.org/> graph definition and builds a corresponding
data structure representing the parsed graph. It can pass that data to the default renderer,
L<GraphViz2::Marpa::Renderer::Graphviz>, which can then render it to a text file ready to be
input to C<dot>. Such 'round-tripping', as it's called, is the best way to test a renderer.

See scripts/g2m.pl and scripts/test.utf8.sh.

=item o L<GraphViz2::Marpa::Renderer::Graphviz>

The default renderer. Optionally called by the parser.

=item o L<GraphViz2::Marpa::Config>

Auxiliary code, used to help generate the demo page.

=item o L<GraphViz2::Marpa::Utils>

Auxiliary code, used to help generate the demo page.

=back

=head1 Sample Data

=over 4

=item o Input files: data/*.gv

These are valid L<Graphviz|http://www.graphviz.org/> graph definition files.

Some data/*.gv files may contain deliberate mistakes, which may or may not stop production
of output files. They may cause various warning messages to be printed by C<dot> when
being rendered.

See L<the demo page|http://savage.net.au/Perl-modules/html/graphviz2.marpa/index.html> for details.

=item o Output files: html/*.svg

The html/*.svg are L<Graphviz|http://www.graphviz.org/> graph definition files output
by scripts/generate.demo.sh.

The round trip shows that the lex/parse process does not lose information along the way, but
comments are discarded..

=back

=head1 Scripts

These are in the scripts/ directory.

=over 4

=item o copy.config.pl

For use by the author. Output:

	Copied config/.htgraphviz2.marpa.conf to /home/ron/.config/Perl/GraphViz2-Marpa

=item o find.config.pl

For use by the author. Output:

	Using: File::HomeDir -> my_dist_config('GraphViz2-Marpa', '.htgraphviz2.marpa.conf'):
	Found: /home/ron/.config/Perl/GraphViz2-Marpa/.htgraphviz2.marpa.conf

=item o g2m.pl

Runs the parser. Try running with -h.

=item o g2m.sh

Simplifies running g2m.pl.

=item o generate.demo.pl

See generate.demo.sh.

=item o generate.demo.sh

Runs scripts/g2m.pl on all files data/*.gv, and then runs generate.index.pl.

After that it copies html/*.html and html/*.svg to my web server's doc root,
$DR/Perl-modules/html/graphviz2.marpa/ (which is a RAMdisk directory under Debian).

=item o pod2html.sh

Converts all *.pm files to *.html, and copies them in my web server's dir structure (in Debian's
RAM disk).

=item o test.sh

Tests any subset of the data.

=item o test.utf8.sh

Tests the utf8 subset of the data more thoroughly than test.sh does.

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

Read the L<Graphviz|http://www.graphviz.org/> graph definition from the command line.

You are strongly encouraged to surround this string with '...' to protect it from your shell.

See also the 'input_file' option to read the description from a file.

The 'description' option takes precedence over the 'input_file' option.

Default: ''.

=item o input_file => $aDotInputFileName

Read the L<Graphviz|http://www.graphviz.org/> graph definition from a file.

See also the 'description' option to read the graph definition from the command line.

The 'description' option takes precedence over the 'input_file' option.

Default: ''.

See the distro for data/*.gv.

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

=item o output_file => aRenderedDotInputFileName

Specify the name of a file for the renderer to write.

That is, write the DOT-style graph definition to a file.

When this file and the input file are both run thru C<dot>, they should produce identical *.svg
files.

If an output file name is specified, an object of type L<GraphViz2::Marpa::Renderer::Graphviz> is
created and called after the input file has been successfully parsed.

Default: ''.

The default means the renderer is not called.

=item o renderer => aGraphViz2::Marpa::Renderer::Graphviz-compatible object

Specify a renderer for the parser to use.

See C<output_file> just above.

Default: undef.

If an output file is specified, then an object of type L<GraphViz2::Marpa::Renderer::Graphviz>
is created and its C<run()> method is called.

=back

=head1 Methods

=head2 description([$graph])

The [] indicate an optional parameter.

Get or set the L<Graphviz|http://www.graphviz.org/> graph definition string.

The value supplied by the 'description' option takes precedence over the value read from the
'input_file'.

See also L</input_file()>.

'description' is a parameter to L</new()>. See L</Constructor and Initialization> for details.

=head2 input_file([$graph_file_name])

Here, the [] indicate an optional parameter.

Get or set the name of the file to read the L<Graphviz|http://www.graphviz.org/> graph
definition from.

The value supplied by the 'description' option takes precedence over the value read from the
'input_file'.

See also the L</description()> method.

'input_file' is a parameter to L</new()>. See L</Constructor and Initialization> for details.

=head2 log($level, $s)

If a logger is defined, this logs the message $s at level $level.

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

If an output file name is specified, an object of type L<GraphViz2::Marpa::Renderer::Graphviz> is
created and called after the input file has been successfully parsed.

'output_file' is a parameter to L</new()>. See L</Constructor and Initialization> for details.

=head2 renderer([$renderer_object])

Here, the [] indicate an optional parameter.

Get or set the renderer object.

This renderer is called if C<output_file()> is given a value.

'renderer' is a parameter to L</new()>. See L</Constructor and Initialization> for details.

=head2 run()

This is the only method the caller needs to call. All parameters are supplied to L</new()>
(or via other methods before C<run()> is called).

See scripts/g2m.pl.

Returns 0 for success and 1 for failure.

=head1 FAQ

=head2 How is the parsed data held in RAM?

It's held in a tree managed by L<Tree::DAG_Node>.

Note: In this section the word 'node' refers to nodes in this tree, not Graphviz-style nodes.

Frstly, we examine a sample graph, assuming the module's pre-reqs are installed.

Run:

	perl -Ilib scripts/g2m.pl -input_file data/10.gv -max info

This is the input:

	digraph graph_10
	{
		edge ["color" = "green",]
		node [shape=rpromoter]
	}

And this is the output:

	root. Attributes: {uid => "0"}
	   |--- prolog. Attributes: {uid => "1"}
	   |   |--- literal. Attributes: {uid => "3", value => "digraph"}
	   |--- graph. Attributes: {uid => "2"}
	       |--- node_id. Attributes: {uid => "4", value => "graph_10"}
	       |--- literal. Attributes: {uid => "5", value => "{"}
	       |   |--- class. Attributes: {uid => "6", value => "edge"}
	       |   |   |--- literal. Attributes: {uid => "7", value => "["}
	       |   |   |--- attribute. Attributes: {name => "color", uid => "8", value => "green"}
	       |   |   |--- literal. Attributes: {uid => "9", value => "]"}
	       |   |--- class. Attributes: {uid => "10", value => "node"}
	       |       |--- literal. Attributes: {uid => "11", value => "["}
	       |       |--- attribute. Attributes: {name => "shape", uid => "12", value => "rpromoter"}
	       |       |--- literal. Attributes: {uid => "13", value => "]"}
	       |--- literal. Attributes: {uid => "14", value => "}"}
	Parse result:  0 (0 is success)

If that worked, proceed.

To follow along with this discussion, run:

	perl -Ilib scripts/g2m.pl -input_file data/16.gv -max info

Input:

	/* C comment. */

	// C++ comment.

	# Hash comment.

	STRICT digraph graph_16
	{
		fontsize = 16.0
		label    = "\"Standard\"\rSyntax\lTest"
		size     = "5,6"

		node
		[
			shape = "record",
		];

		edge
		[
			color = "red"
			penwidth = 3,
		];

		node_16_1
		[
			label    = "<p11> left|<p12> middle|<p13> right"
			pencolor = blue
		]

		node_16_2
		[
			pencolor = green
			label    = "<p21> one|<p22> two"
		]

		node_16_1:p11 -> node_16_2:p22:s
		[
			arrowhead = "odiamond";
			arrowtail = "odot",
			color     = red
			dir       = both;
		];

		subgraph subgraph_16_1
		{
			node [shape = square]

			label = ""

			node_16_3 -> { node [shape = circle] node_16_4 }
			[
				arrowhead = "empty",
				arrowtail = "halfopen"
				color     = green
				dir       = "both",
			]

			node_16_5 -> node_16_6
			[
				arrowhead = "halfopen",
				arrowtail = "empty"
				color     = blue
				dir       = "both",
			]
		}
	}

Output:

	root. Attributes: {uid => "0"}
	   |--- prolog. Attributes: {uid => "1"}
	   |   |--- literal. Attributes: {uid => "3", value => "STRICT"}
	   |   |--- literal. Attributes: {uid => "4", value => "digraph"}
	   |--- graph. Attributes: {uid => "2"}
	       |--- node_id. Attributes: {uid => "5", value => "graph_16"}
	       |--- literal. Attributes: {uid => "6", value => "{"}
	       |   |--- attribute. Attributes: {name => "fontsize", uid => "7", value => "16.0"}
	       |   |--- attribute. Attributes: {name => "label", uid => "8", value => "\"Standard\"\rSyntax\lTest"}
	       |   |--- attribute. Attributes: {name => "size", uid => "9", value => "5,6"}
	       |   |--- class. Attributes: {uid => "10", value => "node"}
	       |   |   |--- literal. Attributes: {uid => "11", value => "["}
	       |   |   |--- attribute. Attributes: {name => "shape", uid => "12", value => "record"}
	       |   |   |--- literal. Attributes: {uid => "13", value => "]"}
	       |   |--- class. Attributes: {uid => "14", value => "edge"}
	       |   |   |--- literal. Attributes: {uid => "15", value => "["}
	       |   |   |--- attribute. Attributes: {name => "color", uid => "16", value => "red"}
	       |   |   |--- attribute. Attributes: {name => "penwidth", uid => "17", value => "3"}
	       |   |   |--- literal. Attributes: {uid => "18", value => "]"}
	       |   |--- node_id. Attributes: {uid => "19", value => "node_16_1"}
	       |   |   |--- literal. Attributes: {uid => "20", value => "["}
	       |   |   |--- attribute. Attributes: {name => "label", uid => "21", value => "<p11> left|<p12> middle|<p13> right"}
	       |   |   |--- attribute. Attributes: {name => "pencolor", uid => "22", value => "blue"}
	       |   |   |--- literal. Attributes: {uid => "23", value => "]"}
	       |   |--- node_id. Attributes: {uid => "24", value => "node_16_2"}
	       |   |   |--- literal. Attributes: {uid => "25", value => "["}
	       |   |   |--- attribute. Attributes: {name => "pencolor", uid => "26", value => "green"}
	       |   |   |--- attribute. Attributes: {name => "label", uid => "27", value => "<p21> one|<p22> two"}
	       |   |   |--- literal. Attributes: {uid => "28", value => "]"}
	       |   |--- node_id. Attributes: {uid => "29", value => "node_16_1:p11"}
	       |   |--- edge_id. Attributes: {uid => "30", value => "->"}
	       |   |--- node_id. Attributes: {uid => "31", value => "node_16_2:p22:s"}
	       |   |   |--- literal. Attributes: {uid => "32", value => "["}
	       |   |   |--- attribute. Attributes: {name => "arrowhead", uid => "33", value => "odiamond"}
	       |   |   |--- attribute. Attributes: {name => "arrowtail", uid => "34", value => "odot"}
	       |   |   |--- attribute. Attributes: {name => "color", uid => "35", value => "red"}
	       |   |   |--- attribute. Attributes: {name => "dir", uid => "36", value => "both"}
	       |   |   |--- literal. Attributes: {uid => "37", value => "]"}
	       |   |--- literal. Attributes: {uid => "38", value => "subgraph"}
	       |   |--- node_id. Attributes: {uid => "39", value => "subgraph_16_1"}
	       |   |--- literal. Attributes: {uid => "40", value => "{"}
	       |   |   |--- class. Attributes: {uid => "41", value => "node"}
	       |   |   |   |--- literal. Attributes: {uid => "42", value => "["}
	       |   |   |   |--- attribute. Attributes: {name => "shape", uid => "43", value => "square"}
	       |   |   |   |--- literal. Attributes: {uid => "44", value => "]"}
	       |   |   |--- attribute. Attributes: {name => "label", uid => "45", value => ""}
	       |   |   |--- node_id. Attributes: {uid => "46", value => "node_16_3"}
	       |   |   |--- edge_id. Attributes: {uid => "47", value => "->"}
	       |   |   |--- literal. Attributes: {uid => "48", value => "{"}
	       |   |   |   |--- class. Attributes: {uid => "49", value => "node"}
	       |   |   |   |   |--- literal. Attributes: {uid => "50", value => "["}
	       |   |   |   |   |--- attribute. Attributes: {name => "shape", uid => "51", value => "circle"}
	       |   |   |   |   |--- literal. Attributes: {uid => "52", value => "]"}
	       |   |   |   |--- node_id. Attributes: {uid => "53", value => "node_16_4"}
	       |   |   |--- literal. Attributes: {uid => "54", value => "}"}
	       |   |   |   |--- literal. Attributes: {uid => "55", value => "["}
	       |   |   |   |--- attribute. Attributes: {name => "arrowhead", uid => "56", value => "empty"}
	       |   |   |   |--- attribute. Attributes: {name => "arrowtail", uid => "57", value => "halfopen"}
	       |   |   |   |--- attribute. Attributes: {name => "color", uid => "58", value => "green"}
	       |   |   |   |--- attribute. Attributes: {name => "dir", uid => "59", value => "both"}
	       |   |   |   |--- literal. Attributes: {uid => "60", value => "]"}
	       |   |   |--- node_id. Attributes: {uid => "61", value => "node_16_5"}
	       |   |   |--- edge_id. Attributes: {uid => "62", value => "->"}
	       |   |   |--- node_id. Attributes: {uid => "63", value => "node_16_6"}
	       |   |       |--- literal. Attributes: {uid => "64", value => "["}
	       |   |       |--- attribute. Attributes: {name => "arrowhead", uid => "65", value => "halfopen"}
	       |   |       |--- attribute. Attributes: {name => "arrowtail", uid => "66", value => "empty"}
	       |   |       |--- attribute. Attributes: {name => "color", uid => "67", value => "blue"}
	       |   |       |--- attribute. Attributes: {name => "dir", uid => "68", value => "both"}
	       |   |       |--- literal. Attributes: {uid => "69", value => "]"}
	       |   |--- literal. Attributes: {uid => "70", value => "}"}
	       |--- literal. Attributes: {uid => "71", value => "}"}
	Parse result:  0 (0 is success)

You can see from this output that words special to Graphviz (e.g. STRICT) are accepted no matter
what case they are in. Such tokens are stored in lower-case.

A more detailed analysis follows.

The 'root' node has 2 daughters:

=over 4

=item o The 'prolog' sub-tree

The 'prolog' node is the root of a sub-tree holding everything before the graph's ID, if any.

The node is called 'prolog', and its hashref of attributes is C<< {uid => "1"} >>.

It has 1 or 2 daughters. The possibilities are:

=over 4

=item o Input: 'digraph ...'

The 1 daughter is named 'literal', and its attributes are C<< {uid => "3", value => "digraph"} >>.

=item o Input: 'graph ...'

The 1 daughter is named 'literal', and its attributes are C<< {uid => "3", value => "graph"} >>.

=item o Input: 'strict digraph ...'

The 2 daughters are named 'literal', and their attributes are, respectively,
C<< {uid => "3", value => "strict"} >> and C<< {uid => "4", value => "digraph"} >>.

=item o Input: 'strict graph ...'

The 2 daughters are named 'literal', and their attributes are, respectively,
C<< {uid => "3", value => "strict"'} >> and C<< {uid => "4", value => "graph"} >>.

=back

And yes, the graph ID, if any, is under the 'graph' node. The reason for this is that with any
subgraphs within the graph, the same structure applies: First the (sub)graph ID, then a literal
'{', then that (sub)graph's details, and finally a literal '}'.

=item o The 'graph' sub-tree

The 'graph' node is the root of a sub-tree holding everything about the graph, including the graph's
ID, if any.

The node is called 'graph', and its hashref of attributes is C<< {uid => "2"} >>.

The 'graph' node has as many daughters, with their own daughters, as is necessary to hold the
output of parsing the remainder of the input.

In particular, if the input graph has an ID, i.e. the input is of the form 'digraph my_id ...'
(or various versions thereof) then the 1st daughter will be called 'node_id', and its attributes
will be C<< {uid => "5", value => "my_id"} >>.

Futher, the 2nd daughter will be called 'literal', and its attributes will be
C<< {uid => "6", value => "{"} >>. A subsequent daughter (for a syntax-free input file, of
course), will also be called 'literal', and its attributes will be
C<< {uid => "#", value => "}"} >>.

Of course, if the input lacks the 'my_id' token, then the uids will differ slightly.

Lastly, this pattern, of optional (sub)graph id followed by a matching pair of '{', '}' nodes,
is used for all graphs and subgraphs.

In the case the input contains an explicit 'subgraph', then just before the node representing
'my_id' or '{', there will be another node representing the 'subgraph' token.

It's name will be 'literal', and its attributes will be
C<< {uid => "#", value => "subgraph"} >>.

E.g., the output from:

	shell> perl scripts/g2m.pl -input_file data/16.gv -max info | grep sub

contains:

	literal. Attributes: {uid => "38", value => "subgraph"}

followed by

	node_id. Attributes: {uid => "39", value => "subgraph_16_1"}

=back

=head2 How many different names can these nodes have?

The list of possible node names follows. In many cases, you have to examine the 'value' key of
the node's attributes to determine the exact nature of the node.

=over 4

=item o attribute

In this case, the node's attributes contain a hashref like
{name => "arrowhead", uid => "33", value => "odiamond"}, meaning the 'name' field holds the name
of the attribute, and the 'value' field holds the value of the attribute.

=item o class

This is used when any of 'edge', 'graph', or 'node' appear at the start of the (sub)graph, and
is the mother of the attributes attached to the class. The 'value' of the attribute will be 'edge',
'graph, or 'node'.

The 1st and last daughters will be literals whose attribute values are '[' and ']' respectively,
and the middle daughter(s) will be nodes of type 'attribute' (as just discussed).

=item o edge_id

The 'value' of the attribute will be either '--' or '->'.

Thus the 'tail' will be the previous daughter (node or subgraph), and the 'head' will be the next.

Samples are:

	n1 -> n2
	n1 -> {n2}
	{n1} -> n2

In a L<daisy chain|https://en.wikipedia.org/wiki/Garland#Daisy_chain> of nodes, the last node in
the chain will have daughters that are the attributes of each edge in the chain. This is how
Graphviz syntax specifies all edge attributes.

=item o literal

'literal' is the name of some nodes, with the 'value' key in the attributes having one of these
values:

=over 4

=item o {

=item o }

=item o [

This indicates the start of a set of attributes for a specific class, edge or node.

The 1st and last daughters will be literals whose attribute 'value' keys are '[' and ']'
respectively.

Between these 2 nodes will be 1 node for each attribute, as seen above with
C<< edge ["color" = "green",] >>.

=item o ]

See the previous point.

=item o digraph

=item o graph

=item o strict

=item o subgraph

=back

=item o node_id

The 'value' of the attributes is just the (graph) node's name.

Note: A node name can appear more than once in succession, either as a declaration of the node's
existence and then as the tail of an edge, or, as in this fragment of data/56.gv:

	node [shape=rpromoter colorscheme=rdbu5 color=1 style=filled fontcolor=3]; Hef1a; TRE; UAS; Hef1aLacOid;
	Hef1aLacOid [label="Hef1a-LacOid"];

This is a case where tree compression could be done, but isn't done yet.

=back

=head2 How are nodes, ports and compass points represented in the (above) tree?

Input contains this fragment of data/16.gv:

	node_16_1:p11 -> node_16_2:p22:s
	[
		arrowhead = "odiamond";
		arrowtail = "odot",
		color     = red
		dir       = both;
	];

And the output log contains:

	|   |--- node_id. Attributes: {uid => "29", value => "node_16_1:p11"}
	|   |--- edge_id. Attributes: {uid => "30", value => "->"}
	|   |--- node_id. Attributes: {uid => "31", value => "node_16_2:p22:s"}

You can see the ports and compass points have been incorporated into the 'value' attribute.

=head2 How are comments stored in the tree?

They aren't stored, they are discarded. And this in turn means rendered C<dot> files can't ever
contain them.

=head2 What is the homepage of Marpa?

L<http://savage.net.au/Marpa.html>.

That page has a long list of links.

=head2 Why do I get error messages like the following?

	Error: <stdin>:1: syntax error near line 1
	context: digraph >>>  Graph <<<  {

Graphviz reserves some words as keywords, meaning they can't be used as an ID, e.g. for the
name of the graph.

So, don't do this:

	strict graph graph{...}
	strict graph Graph{...}
	strict graph strict{...}
	etc...

Likewise for non-strict graphs, and digraphs. You can however add double-quotes around such
reserved words:

	strict graph "graph"{...}

Even better, use a more meaningful name for your graph...

The keywords are: node, edge, graph, digraph, subgraph and strict. Compass points are not keywords.

See L<keywords|http://www.graphviz.org/content/dot-language> in the discussion of the syntax of DOT
for details.

=head2 Does this package support Unicode in the input C<dot> file?

Yes.

But node names with utf8 glyphs should always be enclosed in double-quotes, even though this is
not always necessary.

See data/utf8.*.gv and scripts/test.utf8.sh. In particular, see data/utf8.01.gv.

=head2 How can I switch from Marpa::XS to Marpa::PP?

Don't use either of them. Use L<Marpa::R2>.

=head2 If I input x.old.gv and output x.new.gv, should these 2 files be identical?

Yes - at least in the sense that running C<dot> on them will produce the same output files.
This is assuming the default renderer is used.

As mentioned just above, comments in input files are discarded, so they can never be in the output
file.

=head2 How are custom graph attributes handled?

They are treated like any other attribute. That is, syntax checking is not performed at that level,
but only at the grammatical level. If the construct matches the grammar, this code accepts it.

See data/32.gv.

=head2 How are the demo files generated?

See scripts/generate.demo.sh.

=head1 See Also

L<Marpa::Demo::StringParser>. The significance of this module is that during the re-write of
GraphViz2::Marpa, the string-handling code was perfected in L<Marpa::Demo::StringParser>.

Later, that code was improved within this module, and will be back-ported into
Marpa::Demo::StringParser

=head1 Machine-Readable Change Log

The file CHANGES was converted into Changelog.ini by L<Module::Metadata::Changes>.

=head1 Version Numbers

Version numbers < 1.00 represent development versions. From 1.00 up, they are production versions.

=head1 Thanks

Many thanks are due to the people who worked on L<Graphviz|http://www.graphviz.org/>.

Jeffrey Kegler wrote L<Marpa::XS>, and has a blog on it at
L<http://blogs.perl.org/users/jeffrey_kegler/>.

=head1 Repository

L<https://github.com/ronsavage/GraphViz2-Marpa>

=head1 Support

Email the author, or log a bug on RT:

L<https://rt.cpan.org/Public/Dist/Display.html?Name=GraphViz2::Marpa>.

=head1 Author

L<GraphViz2::Marpa> was written by Ron Savage I<E<lt>ron@savage.net.auE<gt>> in 2012.

Marpa's homepage: <http://savage.net.au/Marpa.html>.

My homepage: L<http://savage.net.au/>.

=head1 Copyright

Australian copyright (c) 2012, Ron Savage.

	All Programs of mine are 'OSI Certified Open Source Software';
	you can redistribute them and/or modify them under the terms of
	The Artistic License 2.0, a copy of which is available at:
	http://opensource.org/licenses/alphabetical.

=cut
