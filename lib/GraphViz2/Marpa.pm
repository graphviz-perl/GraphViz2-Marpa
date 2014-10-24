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

use Types::Standard qw/Any ArrayRef Int Str/;

use Try::Tiny;

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

	$self -> grammar
	(
		Marpa::R2::Scanless::G -> new
		({
source					=> \(<<'END_OF_GRAMMAR'),

:default				::= action => [values]

lexeme default			= latm => 1		# Longest Acceptable Token Match.

:start 					::= graph_definition

# The prolog to the graph.

graph_definition		::= graph_body
							| comments graph_body
							| graph_body comments
							| comments graph_body comments

comments				::= comment+

comment					::= <C style comment>
							| <Cplusplus style comment>
							| <hash style comment>

graph_body				::= prolog_tokens graph_statement_tokens

prolog_tokens			::= strict_token graph_type global_id_type

strict_token			::=
strict_token			::= strict_literal

graph_type				::= digraph_literal
							| graph_literal

global_id_type			::=
global_id_type			::= generic_id_token

# The graph proper.

graph_statement_tokens	::= open_brace statement_list close_brace

statement_list			::= statement_token*

statement_token			::= statement statement_terminator
							| comments statement_token

statement_terminator	::= semicolon_literal
statement_terminator	::=

statement				::= node_statement
							| edge_statement
							| attribute_statement
							| assignment_statement
							| subgraph_statement
							| comments
# Node stuff.

node_statement			::= generic_id_token attribute_tokens

generic_id_token		::= generic_id
							| ('<') generic_id ('>')
							| number
							| '"' string '"'
							| empty_string

number					::= float
							| integer

# Edge stuff.

edge_statement			::= edge_segment attribute_tokens

edge_segment			::= edge_tail edge_literal edge_head

edge_tail				::= edge_node_token
							| subgraph_statement

edge_head				::= edge_tail
							|  edge_tail edge_literal edge_head

edge_node_token			::= generic_id_token
							| node_port_id
							| node_port_compass_id

node_port_id			::= generic_id_token<colon>generic_id_token

node_port_compass_id	::= node_port_id<colon>generic_id_token

# Attribute stuff.
# These have no body between the '[]' because they are parsed manually in order to
# preserve whitespace in double-quoted strings, which is otherwise discarded by this grammar.
# See _process_attributes(). See also the same method for the handling of attribute terminators: [;,].

attribute_tokens		::=
attribute_tokens		::= open_bracket close_bracket statement_terminator

attribute_statement		::= node_statement # Search for 'class'! It's in _process().

# Assignment stuff.
# There is nothing after 'equals_literal' because it is parsed manually in order to
# preserve whitespace in double-quoted strings, which is otherwise discarded by this grammar.
# See _attribute_field().

assignment_statement	::= generic_id equals_literal generic_id_token

# Subgraph stuff.
# Subgraphs are handled by the statement type 'graph_statement_tokens'.
# Hence subgraph sub_1 {...} and subgraph {...} and sub_1 {...} and {...} are all output as:
# o The optional literal 'subgraph', which is classified as a literal.
# o The optional subgraph id, which is classified as a node_id.
# o The literal '{'.
# o The body of the subgraph.
# o The literal '}'.

subgraph_statement		::= subgraph_prefix subgraph_id_token graph_statement_tokens

subgraph_prefix			::=
subgraph_prefix			::= subgraph_literal

subgraph_id_token		::=
subgraph_id_token		::= generic_id_token

# Lexeme-level stuff, in alphabetical order.

:lexeme					~ close_brace		pause => before		event => close_brace

close_brace				~ '}'

:lexeme					~ close_bracket		pause => before		event => close_bracket

close_bracket			~ ']'

:lexeme					~ colon				pause => before		event => colon

colon					~ ':'

digit					~ [0-9]
digit_any				~ digit*
digit_many				~ digit+

:lexeme					~ digraph_literal	pause => before		event => digraph_literal

digraph_literal			~ 'digraph':i

:lexeme					~ edge_literal		pause => before		event => edge_literal

edge_literal			~ '->'
edge_literal			~ '--'

:lexeme					~ empty_string		pause => before		event => string

empty_string			~ '""'

:lexeme					~ equals_literal	pause => before		event => equals_literal

equals_literal			~ '='

escaped_char			~ '\' string_char

# Comment with ' just for the UltraEdit syntax hiliter.

escaped_quote			~ '\"'

:lexeme					~ float				pause => before		event => float

float					~ sign_maybe digit_any '.' digit_many
							| sign_maybe digit_many '.' digit_any

:lexeme					~ generic_id		pause => before		event => generic_id

generic_id_prefix		~ [a-zA-Z\200-\377_]
generic_id_suffix		~ [a-zA-Z\200-\377_0-9]*
generic_id				~ <generic_id_prefix>generic_id_suffix

:lexeme					~ graph_literal		pause => before		event => graph_literal

graph_literal			~ 'graph':i

:lexeme					~ integer			pause => before		event => integer

integer					~ sign_maybe non_zero digit_any
							| zero

non_zero				~ [1-9]

:lexeme					~ open_brace		pause => before		event => open_brace

open_brace				~ '{'

:lexeme					~ open_bracket		pause => before		event => open_bracket

open_bracket			~ '['

semicolon_literal		~ ';'

sign_maybe				~ [+-]
sign_maybe				~

:lexeme					~ strict_literal	pause => before		event => strict_literal

strict_literal			~ 'strict':i

:lexeme					~ string			pause => before		event => string

string					~ string_char+

string_char				~ escaped_char | escaped_quote | [^"]

# Comment with " just for the UltraEdit syntax hiliter.

:lexeme					~ subgraph_literal	pause => before		event => subgraph_literal

subgraph_literal		~ 'subgraph':i

zero					~ '0'

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

<vertical space char>				~ [\x{A}\x{B}\x{C}\x{D}\x{2028}\x{2029}]

<hash comment char>					~ [^\x{A}\x{B}\x{C}\x{D}\x{2028}\x{2029}]

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
			grammar          => $self -> grammar,
			#trace_terminals => 99,
		})
	);

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

# --------------------------------------------------

sub _adjust_attributes
{
	my($self, $mother) = @_;
	my(@daughters)     = $mother -> daughters;

	my(@attributes);
	my(@name);

	for (my $i = 0; $i < ($#daughters - 1); $i++)
	{
		# Have we found a triplet of the form ['node_id', 'equals', 'float|integer|node_id']?

		if ($daughters[$i] -> name eq 'node_id')
		{
			$name[0] = $daughters[$i + 1] -> name;
			$name[1] = $daughters[$i + 2] -> name;

			if ("$name[0]$name[1]" =~ /^equals(?:float|integer|node_id)/)
			{
				$attributes[0] = $daughters[$i] -> attributes;
				$attributes[1] = $daughters[$i + 2]-> attributes;
				$attributes[2] = {type => $attributes[1]{type} || 'string', uid => $attributes[0]{uid}, value => $attributes[1]{value} };

				$daughters[$i] -> name($attributes[0]{value});
				$daughters[$i] -> attributes($attributes[2]);

				# Really, we want to do ...

				#splice(@daughters, ($i + 1), 2);

				# ... but Tree::DAG_Node warns against changing the tree during a walk_down.
			}
		}
	}

} # End of _adjust_attributes.

# --------------------------------------------------

sub _adjust_edge_attributes
{
	my($self, $mothers) = @_;

	my(@daughters);
	my($mother);
	my($offset);
	my($name, $next_daughter, $next_name);

	for my $uid (keys %$mothers)
	{
		$mother    = $$mothers{$uid};
		@daughters = $mother -> daughters;

		for my $i (0 .. $#daughters)
		{
			# Find each edge.

			$name = $daughters[$i] -> name;

			next if ($name ne 'edge');

			# This test is redundant if we know the input file is syntactally valid,
			# since then there must necessarily be a next daughter after an edge.
			# But is the file known to be valid at this point?
			# Yes, since we're in the post-processing phase, and Marpa has validated it.
			# Unless, of course, there's a defect in our BNF for DOT.

			#next if ($i < $#daughters);

			# But what exactly is the thing at the head of the edge?
			# It could be a node, or it could be a subgraph.

			$self -> _adjust_head_attributes(\@daughters, $i);
		}
	}

} # End of _adjust_edge_attributes.

# --------------------------------------------------

sub _adjust_head_attributes
{
	my($self, $daughters, $i) = @_;

	# The head of the edge will be one of:
	# o A node.
	# o A subgraph, as in ['subgraph', 'id', '{', ..., '}'].
	# o A subgraph, as in ['subgraph', '{', ..., '}'].
	# o A subgraph, as in ['{', ..., '}'].
	# In all 4 cases, we want to move the attributes, if any, off the last element in the list,
	# and re-attach them to the tree node of the edge. And why do that?
	# Because I think that's a good idea for when the graph is rendered.

	my($name) = $$daughters[$i + 1] -> name;

	if ($name =~ /node_id/)
	{
		# It's a node.

		$$daughters[$i] -> set_daughters($$daughters[$i + 1] -> clear_daughters);
	}
	elsif ($name eq 'literal')
	{
		my($attributes) = $$daughters[$i + 1] -> attributes;

		my($offset);

		if ($$attributes{value})
		{
			if ($$attributes{value} eq '{')
			{
				# Find '}'. It'll be the first '}' in this daughter list.

				for (my $j = $i + 2; $j <= $#$daughters; $j++)
				{
					$attributes = $$daughters[$j] -> attributes;

					if ($$attributes{value} && ($$attributes{value} eq '}') )
					{
						$offset = $j;
						$j      = $#$daughters;
					}
				}
			}
			elsif ( ( ($i + 2) < $#$daughters) && ($$attributes{value} eq 'subgraph') )
			{
				# Is the next item a subgraph id or a '{'?

				my($name) = $$daughters[$i + 2] -> name;

				if ($name eq 'node_id')
				{
					$offset = $i + 2;
				}
				elsif ( ($i + 3) < $#$daughters)
				{
					$attributes = $$daughters[$i + 3] -> attributes;

					if ($$attributes{value} && ($$attributes{value} eq '{') )
					{
						$offset = $i + 3;
					}
				}
			}
		}

		if ($offset)
		{
			# It's a subgraph.

			$$daughters[$i] -> set_daughters($$daughters[$offset] -> clear_daughters);
		}
	}

} # End of _adjust_head_attributes.

# ------------------------------------------------
# $separator is '='.

sub _attribute_field
{
	my($self, $type, $input, $separator) = @_;
	my(@char)          = split(//, $input);
	my($char_count)    = 0;
	my($quote_count)   = 0;
	my($field)         = '';
	my($html)          = 'no';
	my($previous_char) = '';
	my($result)        = '';

	my($char);

	for my $i (0 .. $#char)
	{
		$char_count++;

		$char = $char[$i];
		$html = 'yes' if ( ($html eq 'no') && ($char eq '<') && ($field eq '') );

		$self -> log(debug => "Char: <$char>. HTML: $html. quote_count: $quote_count. Field: $field.");

		if ($char eq '"')
		{
			# Gobble up any escaped quotes.

			if ($previous_char eq '\\')
			{
				$field         .= $char;
				$previous_char = $char;

				next;
			}

			$quote_count++;

			$field .= $char;

			# If HTML, gobble up any quotes.

			if ($html eq 'yes')
			{
				$previous_char = $char;

				next;
			}

			# First quote is start of field.

			if ($quote_count == 1)
			{
				$previous_char = $char;

				next;
			}

			# Second quote is end of field.

			$quote_count = 0;
			$result      = $field;
			$field       = '';

			$self -> log(debug => "Result 1: $result.");

			# Skip the quote, and skip any training '\s=\s'.

			$i++;

			$self -> log(debug => "Input: " . join('', @char[$i .. $#char]) . '.');

			while ( ($i < $#char) && ($char[$i] =~ /[=\s]/) )
			{
				$i++;
				$char_count++;
			}

			$self -> log(debug => "Input: " . join('', @char[$i .. $#char]) . '.');

			last;
		}
		elsif ($quote_count == 0)
		{
			if ($html eq 'no')
			{
				# Discard some chars outside quotes, and possibly finish.

				if ($char =~ /[$separator\s;,]/)
				{
					next if (length($field) == 0);

					$result = $field;
					$field  = '';

					$self -> log(debug => "Result 2: $result.");

					last;
				}
			}

			$field         .= $char;
			$previous_char = $char;
		}
		else
		{
			$field .= $char;
		}

		$previous_char = $char;
	}

	$result = $field if ($field ne '');
	$result =~ s/^"(.+)"$/$1/;

	$self -> log(debug => "Input 1: $input.");

	$input  = substr($input, $char_count);

	$self -> log(debug => "Input 2: $input.");

	$input  =~ s/^[\s;,]+//;

	$self -> log(debug => "Input 3: $input.");

	return ($result, $input);

} # End of _attribute_field.

# --------------------------------------------------

sub _compress_node_port_compass
{
	my($self, $mothers) = @_;

	# Compress node:port and node:port:compass into 1 node each.
	# These are the possibilities:
	# $i	+ 1		+ 2		+ 3		+ 4			Action
	# Node										None
	# Node	Colon	Port						Compress
	# Node	Colon	Port	Colon	Compass		Compress
	#
	# We use a stack so we can reprocess a daughter list after
	# compressing a set of daughters.

	my(@stack) = keys %$mothers;

	my(@daughters);
	my(@edge_daughters);
	my($mother);
	my(@name, $new_name, $node_attributes);

	while (my $uid = shift @stack)
	{
		$mother    = $$mothers{$uid};
		@daughters = $mother -> daughters;

		MIDDLE_LOOP:
		for my $i (0 .. $#daughters)
		{
			# We try the longest possible match first, since the alternative
			# would accidently find the first part of the longest match.

			for (my $offset = 4; $offset > 0; $offset -= 2)
			{
				if ( ($i + $offset) <= $#daughters)
				{
					if ($self -> _compress_nodes($mothers, $uid, \@daughters, $i, $offset) )
					{
						# Reprocess the mother's daughters after compressing ($offset + 1) nodes.

						unshift @stack, $uid;

						# I don't like jumps, but I need to exit these 2 loops immediately,
						# since the mother's daughter list has just been fiddled.

						last MIDDLE_LOOP;
					}
				}
			}
		}
	}

} # End of _compress_node_port_compass.

# -----------------------------------------------

sub _compress_nodes
{
	my($self, $mothers, $uid, $daughters, $i, $offset) = @_;
	my($found) = 0;

	my(@attributes);
	my(@name);

	$attributes[0] = $$daughters[$i + 1] -> attributes;
	$attributes[1] = ($offset == 4) ? $$daughters[$i + 3] -> attributes : {value => ':'};

	if ("${$attributes[0]}{value}${$attributes[1]}{value}" eq '::')
	{
		# Preserve the attributes of the last node, since they belong to the edge.
		# Here we attach them to the new node which is a combination of several nodes.
		# Later, the caller of compress_node_port_compass() will move them back to the edge.

		my(@edge_daughters) = ( ($i + $offset) <= $#$daughters) ? $$daughters[$i + $offset] -> daughters : ();
		$found              = 1;
		my($new_name)       = '';

		my($node_attributes);

		for (my $j = $i; $j <= ($i + $offset); $j++)
		{
			$node_attributes  = $$daughters[$j] -> attributes;
			$new_name        .= $$node_attributes{value};
		}

		splice(@$daughters, ($i + 1), $offset);

		# We update the attributes with the node's real name.
		# The tree node's name will still be 'node_id'.

		$node_attributes         = $$daughters[$i] -> attributes;
		$$node_attributes{value} = $new_name;

		$$daughters[$i] -> attributes($node_attributes);
		$$daughters[$i] -> set_daughters(@edge_daughters) if ($#edge_daughters >= 0);
		$$mothers{$uid} -> set_daughters(@$daughters);
	}

	return $found;

} # End of _compress_nodes;

# -----------------------------------------------
# $target is ']'.
# The special case is <<...>>, as used in attributes.
# The same code is used in MarpaX::Demo::StringParser.

sub _find_terminator
{
	my($self, $stringref, $start, $target) = @_;
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
			# The 2nd & 3rd backslashes are just for the UltraEdit syntax hiliter.

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
			# The backslashes are just for the UltraEdit syntax hiliter.

			if ($char =~ /[\"\']/)
			{
				$quote = $char;

				next;
			}

			# 3: <.
			# In the case of attributes but not nodes names, quotes can be <...> or <<...>>.

			if ($char eq '<')
			{
				$quote = '>';
				$angle = 1 if ( ($i < $#char) && ($char[$i + 1] eq '<') );

				next;
			}

			last if ($char eq $target);
		}
	}

	return $start + $offset;

} # End of _find_terminator.

# --------------------------------------------------

sub log
{
	my($self, $level, $s) = @_;

	$self -> logger -> log($level => $s) if ($self -> logger);

} # End of log.

# --------------------------------------------------

sub _post_process
{
	my($self) = @_;

	# Walk the tree, find the edges, and then stockpile their mothers.
	# Later, compress the [node]:[port]:[compass] quintuplets and the
	# [node]:[port] triplets each into a single node, either [node:port:compass]
	# or [node:port].
	# The Tree::DAG_Node docs warn against modifying the tree during a walk,
	# so we use a hash to track all the mothers found, and post-process them.

	my($attributes);
	my(%mothers);
	my($uid);

	$self -> tree -> walk_down
	({
		callback => sub
		{
			my($node) = @_;
			my($name) = $node -> name;

			# Stash mother uids, for later processing.

			if ($name eq 'edge')
			{
				$attributes    = $node -> mother -> attributes;
				$uid           = $$attributes{uid};
				$mothers{$uid} = $node -> mother if (! $mothers{$uid});
			}

			# Phase 1: Replace tree node triplets ('node_id', 'equals', 'float|integer|node_id')
			# with the 3 daughters ('$attribute_name', 'equals', 'float|integer|node_id').

			$node = $node -> mother;

			# Ignore the root's mother, and nodes whose mother is 'root'.

			if ($node && ($node -> name ne 'root') )
			{
				$self -> _adjust_attributes($node);
			}

			# Keep recursing.

			return 1;
		},
		_depth => 0,
	});

	$self -> _compress_node_port_compass(\%mothers);

	# Now look for attributes hanging off the edge's head node/subgraph,
	# and move them back to belong to the edge itself.

	$self -> _adjust_edge_attributes(\%mothers);

	# Phase 2: Replace tree node triplets ('node_id', 'equals', 'float|integer|node_id')
	# with the 1 daughter ('$attribute_name'.

	my(@daughters);
	my($i);

	for my $uid (keys %mothers)
	{
		@daughters = $mothers{$uid} -> daughters;

		for ($i = 0; $i < ($#daughters - 2); $i++)
		{
			if ($daughters[$i + 1] -> name eq 'equals')
			{
				splice(@daughters, ($i + 1), 2);

				$mothers{$uid} -> set_daughters(@daughters);
			}
		}
	}

} # End of _post_process.

# --------------------------------------------------

sub _process
{
	my($self)          = @_;
	my($string)        = $self -> graph_text;
	my($length)        = length $string;
	my($literal_token) = qr/(?:colon|edge_literal|equals_literal|strict_literal|subgraph_literal)/;
	my($prolog_token)  = qr/(?:digraph_literal|graph_literal)/;
	my($simple_token)  = qr/(?:float|integer)/;

	$self -> log(debug => "Input:\n$string");

	# We use read()/lexeme_read()/resume() because we pause at each lexeme.

	my($attribute_list, $attribute_value);
	my(@event, $event_name);
	my($generic_id);
	my($lexeme_name, $lexeme, $literal);
	my($node_name);
	my($span, $start, $s);
	my($type);

	for
	(
		my $pos = $self -> recce -> read(\$string);
		$pos < $length;
		$pos = $self -> recce -> resume($pos)
	)
	{
		$self -> log(debug => "read() => pos: $pos. ");

		@event          = @{$self -> recce -> events};
		$event_name     = ${$event[0]}[0];
		($start, $span) = $self -> recce -> pause_span;
		$lexeme_name    = $self -> recce -> pause_lexeme;
		$lexeme         = $self -> recce -> literal($start, $span);

		$self -> log(debug => "pause_span($lexeme_name) => start: $start. span: $span. " .
			"lexeme: $lexeme. event: $event_name. ");

		if ($event_name =~ $simple_token)
		{
			$pos = $self -> _process_lexeme(\$string, $start, $event_name, $event_name, $lexeme_name, $event_name);
		}
		elsif ($event_name =~ $literal_token)
		{
			$node_name = ($event_name eq 'edge_literal')
							? 'edge'
							: ($event_name eq 'equals_literal')
							? 'equals'
							: 'literal';
			$pos       = $self -> _process_lexeme(\$string, $start, $event_name, $node_name, $lexeme_name, undef);
		}
		elsif ($event_name eq 'close_brace')
		{
			$pos     = $self -> recce -> lexeme_read($lexeme_name);
			$literal = substr($string, $start, $pos - $start);

			$self -> log(debug => "close_brace => '$literal'");
			$self -> _process_brace($literal);
		}
		elsif ($event_name eq 'close_bracket')
		{
			$pos     = $self -> recce -> lexeme_read($lexeme_name);
			$literal = substr($string, $start, $pos - $start);

			$self -> log(debug => "close_bracket => '$literal'");
			$self -> _process_bracket($literal);
		}
		elsif ($event_name eq 'generic_id')
		{
			$pos        = $self -> recce -> lexeme_read($lexeme_name);
			$generic_id = substr($string, $start, $pos - $start);
			$type       = 'node_id';

			if ($generic_id =~ /^(?:edge|graph|node)$/i)
			{
				$type = 'class';
			}
			elsif ($generic_id eq 'subgraph')
			{
				$type = 'literal';
			}

			$self -> log(debug => "generic_id => '$generic_id'. type => $type");
			$self -> _process_token($type, $generic_id);
		}
		elsif ($event_name eq 'open_brace')
		{
			$pos     = $self -> recce -> lexeme_read($lexeme_name);
			$literal = substr($string, $start, $pos - $start);

			$self -> log(debug => "open_brace => '$literal'");
			$self -> _process_brace($literal);
		}
		elsif ($event_name eq 'open_bracket')
		{
			$pos     = $self -> recce -> lexeme_read($lexeme_name);
			$literal = substr($string, $start, $pos - $start);

			$self -> log(debug => "open_bracket => '$literal'");
			$self -> _process_bracket($literal);

			$pos = $self -> _find_terminator(\$string, $start, ']');

			$attribute_list = substr($string, $start + 1, $pos - $start - 1);

			$self -> log(debug => "index() => attribute list: $attribute_list");
			$self -> _process_attributes($attribute_list);
		}
		elsif ($event_name eq 'string')
		{
			$pos     = $self -> recce -> lexeme_read($lexeme_name);
			$literal = substr($string, $start, $pos - $start);

			$self -> log(debug => "string => '$literal'");
			$self -> _process_token('node_id', $literal);
		}
		# From here on are the low-frequency events.
		elsif ($event_name =~ $prolog_token)
		{
			$pos     = $self -> recce -> lexeme_read($lexeme_name);
			$literal = lc substr($string, $start, $pos - $start);

			$self -> log(debug => "$event_name => '$literal'");
			$self -> _process_digraph_graph($event_name, $literal);
		}
		else
		{
			die "Unexpected lexeme '$lexeme_name' with a pause\n";
		}
    }

	# Return a defined value for success and undef for failure.

	return $self -> recce -> value;

} # End of _process.

# --------------------------------------------------

sub _process_attributes
{
	my($self, $attribute_list) = @_;

	my($name_length, $value_length);
	my($name);
	my($value);

	while (length($attribute_list) > 0)
	{
		($name, $attribute_list)  = $self -> _attribute_field('name', $attribute_list, '=');
		$name_length              = length $name;

		$self -> log(debug => "Found attribute name  <$name>");

		($value, $attribute_list) = $self -> _attribute_field('value', $attribute_list, '=');
		$value_length             = length $value;

		$self -> log(debug => "Found attribute value <$value>");

		# Check for 'node_name [ ]'.

		if ( ($name_length > 0) && ($value_length > 0) )
		{
			# Remove trailing whitespace from HTML labels.

			$value =~ s/\s+$//;

			$self -> _add_daughter($name, {value => $value});
		}

		# Discard attribute teminators.

		$attribute_list =~ s/^\s*[;,]\s*//;
	}

} # End of _process_attributes.

# --------------------------------------------------

sub _process_brace
{
	my($self, $name) = @_;

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
		$self -> _add_daughter('literal', {value => $name});

		my(@daughters) = $$stack[$#$stack] -> daughters;

		push @$stack, $daughters[$#daughters];
	}
	else
	{
		pop @$stack;

		$self -> stack($stack);
		$self -> _add_daughter('literal', {value => $name});
		$self -> brace_count($self -> brace_count - 1);
	}

} # End of _process_brace.

# --------------------------------------------------

sub _process_bracket
{
	my($self, $name) = @_;

	# When a '[' is encountered, the last thing pushed becomes it's parent.
	# Likewise, if ']' is encountered, we pop the stack.

	my($stack) = $self -> stack;

	if ($name eq '[')
	{
		my(@daughters) = $$stack[$#$stack] -> daughters;

		push @$stack, $daughters[$#daughters];

		$self -> _process_token('literal', $name);
	}
	else
	{
		$self -> _process_token('literal', $name);

		pop @$stack;

		$self -> stack($stack);
	}

} # End of _process_bracket.

# --------------------------------------------------

sub _process_digraph_graph
{
	my($self, $name, $value) = @_;

	$self -> _add_daughter('literal', {value => $value});

	# When 'digraph' or 'graph' is encountered, the 'Graph' daughter of the root
	# becomes the parent of all other tree nodes, replacing the 'prolog' daughter.

	my($stack) = $self -> stack;

	pop @$stack;

	my(@daughters) = $self -> tree -> daughters;
	my($index)     = 1; # 0 => prolog, 1 => graph.

	push @$stack, $daughters[$index];

	$self -> stack($stack);

} # End of _process_digraph_graph.

# --------------------------------------------------

sub _process_lexeme
{
	my($self, $string, $start, $event_name, $name, $lexeme_name, $type) = @_;
	my($pos)   = $self -> recce -> lexeme_read($lexeme_name);
	my($value) = lc substr($$string, $start, $pos - $start);

	$self -> log(debug => "$event_name => '$value'");

	$value = $type ? {type => $type, value => $value} : {value => $value};

	$self -> _add_daughter($name, $value);

	return $pos;

} # End of _process_lexeme.

# --------------------------------------------------

sub _process_token
{
	my($self, $name, $value) = @_;

	$self -> _add_daughter($name, {value => $value});

} # End of _process_token.

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
		# Quick removal of whole-line C++ and hash comments.

		my @graph_text = grep{! m!^\s*(?:#|//)!}
							read_file
								(
									$self -> input_file,
									binmode => ':encoding(UTF-8)',
								);

		$self -> graph_text(join('', @graph_text) );
	}
	else
	{
		die "Error: You must provide a graph using one of -input_file or -description\n";
	}

	# Return 0 for success and 1 for failure.

	my($result) = 0;

	try
	{
		if (defined $self -> _process)
		{
			$self -> _post_process;
			$self -> log(info => join("\n", @{$self -> tree -> tree2string}) );
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

	$self -> log(info => "Parse result: $result (0 is success)");

	if ($result == 0)
	{
		# Clean up the stack by popping the root node.

		my($stack) = $self -> stack;

		pop @$stack;

		$self -> stack($stack);
		$self -> log(debug => 'Brace count:  ' . $self -> brace_count . ' (0 expected)');
		$self -> log(debug => 'Stack size:   ' . $#{$self -> stack} . ' (0 expected)');

		my($output_file) = $self -> output_file;

		if ($output_file)
		{
			$self -> log(debug => "Rendering to $output_file");
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
		}
	}

	# Return 0 for success and 1 for failure.

	return $result;

} # End of run.

# --------------------------------------------------

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

L<An article on this module|http://savage.net.au/Ron/html/A.New.Marpa-based.Parser.for.GraphViz.html>.
This article contains the ToDo list.

=head1 Modules

=over 4

=item o L<GraphViz2::Marpa>

The current module, which documents the set of modules.

It can, optionally, use the default renderer L<GraphViz2::Marpa::Renderer::Graphviz>.

Accepts a L<Graphviz|http://www.graphviz.org/> graph definition and builds a corresponding
data structure representing the parsed graph. It can pass that data to the default renderer,
L<GraphViz2::Marpa::Renderer::Graphviz>, which can then render it to a text file.

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

Some data/*.gv files may contain I<slight> deliberate mistakes, which do not stop production
of output files. They do, however, cause various warning messages to be printed by C<dot> when
certain scripts are run.

=item o Output files: html/*.svg

The html/*.svg are L<Graphviz|http://www.graphviz.org/> graph definition files output
by scripts/generate.demo.sh.

The round trip shows that the lex/parse process does not lose information along the way, but
comments are discarded..

=item o Input files: fail/*.gv

These are faulty L<Graphviz|http://www.graphviz.org/> graph definition files.

That is, they contain syntax errors (I<serious> deliberate mistakes from the point of view of
L<Graphviz|http://www.graphviz.org/>), but they helped with writing the code.

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

=item o generate.index.pl

Generates html/index.html from data/*.gv and html/*.svg.

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

When this file and the input file are both run thru C<dot>, they should produce identical *.svg files.

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
	}

And this is the output:

	root. Attributes: {uid => "0"}
	   |---prolog. Attributes: {uid => "1"}
	   |   |---literal. Attributes: {uid => "3", value => "digraph"}
	   |---graph. Attributes: {uid => "2"}
	       |---node_id. Attributes: {uid => "4", value => "graph_10"}
	       |---literal. Attributes: {uid => "5", value => "{"}
	       |   |---class. Attributes: {uid => "6", value => "edge"}
	       |       |---literal. Attributes: {uid => "7", value => "["}
	       |       |---attribute. Attributes: {type => "color", uid => "8", value => "green"}
	       |       |---literal. Attributes: {uid => "9", value => "]"}
	       |---literal. Attributes: {uid => "10", value => "}"}

To follow along with this discussion, run:

	perl -Ilib scripts/g2m.pl -input_file data/16.gv -max info

The root node has 2 daughters:

=over 4

=item o The 'prolog' sub-tree

This daughter is the root of a sub-tree holding everything before the graph's ID, if any.

The node is called 'prolog', and its hashref of attributes is C<< {uid => 1} >>.

It has 1 or 2 daughters. The possibilities are:

=over 4

=item o Input 'digraph ...'

The 1 daughter is named 'literal', and its attributes are C<< {uid => 3, value => 'digraph'} >>.

=item o Input 'graph ...'

The 1 daughter is named 'literal', and its attributes are C<< {uid => 3, value => 'graph'} >>.

=item o Input 'strict digraph ...'

The 2 daughters are named 'literal', and their attributes are, respectively,
C<< {uid => 3, value => 'strict'} >> and C<< {uid => 4, value => 'digraph'} >>.

=item o Input 'strict graph ...'

The 2 daughters are named 'literal', and their attributes are, respectively,
C<< {uid => 3, value => 'strict'} >> and C<< {uid => 4, value => 'graph'} >>.

=back

And yes, the graph ID, if any, is under the 'Graph' node.

=item o The 'graph' sub-tree

This daughter is the root of a sub-tree holding everything about the graph, including the graph's ID, if any.

The node is called 'graph', and its hashref of attributes is C<< {uid => 2} >>.

The 'Graph' node has as many daughters, with their own daughters, as is necessary to hold the
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

	literal. Attributes: {uid => "47", value => "subgraph"}

followed by

	node_id. Attributes: {uid => "48", value => "subgraph_16_1"}

=back

=head2 How many different names can these nodes have?

The list of possible node names follows. In many cases, you have to examine the 'value' key of
the node's attributes to determine the exact nature of the node.

=over 4

=item o $attribute_name

This indicates an attribute for a class (see next point), an edge, a node or a (sub)graph.

Here's part of the log from processing data/16.gv:

	|   |---fontsize. Attributes: {type => "float", uid => "7", value => "16.0"}
	|   |---label. Attributes: {type => "string", uid => "10", value => "\"Standard\"\rSyntax\lTest"}
	|   |---size. Attributes: {type => "string", uid => "13", value => "5,6"}
	...
	|   |---class. Attributes: {uid => "20", value => "edge"}
	|   |   |---literal. Attributes: {uid => "21", value => "["}
	|   |   |---color. Attributes: {uid => "22", value => "red"}
	|   |   |---penwidth. Attributes: {uid => "23", value => "3"}
	|   |   |---literal. Attributes: {uid => "24", value => "]"}
	|   |---node_id. Attributes: {uid => "25", value => "node_16_1"}
	|   |   |---literal. Attributes: {uid => "26", value => "["}
	|   |   |---label. Attributes: {uid => "27", value => "<p11> left|<p12> middle|<p13> right"}
	|   |   |---pencolor. Attributes: {uid => "28", value => "blue"}
	|   |   |---literal. Attributes: {uid => "29", value => "]"}
	...
	|   |---node_id. Attributes: {uid => "35", value => "node_16_1:p11"}
	|   |---edge. Attributes: {uid => "38", value => "->"}
	|   |   |---literal. Attributes: {uid => "44", value => "["}
	|   |   |---arrowhead. Attributes: {uid => "45", value => "odiamond"}
	|   |   |---arrowtail. Attributes: {uid => "46", value => "odot"}
	|   |   |---color. Attributes: {uid => "47", value => "red"}
	|   |   |---dir. Attributes: {uid => "48", value => "both"}
	|   |   |---literal. Attributes: {uid => "49", value => "]"}
	|   |---node_id. Attributes: {uid => "39", value => "node_16_2:p22:s"}

Futher, in some cases, the code can identify the type of the 'value' as one of 'integer', 'float' or 'string.

Lastly notice that for classes, edges and node, the attributes are surrounded by 2 nodes called 'literal',
with 'value's of '[' and ']'. However, for attributes specified as 'fontsize = 16.0', this is not the case.

=item o class

This is used when any of 'edge', 'graph', or 'node' appear at the start of the (sub)graph, and
is the mother of the attributes attached to the class. The 'value' of the attribute will be 'edge',
'graph, or 'node'.

The 1st and last daughters will be literals whose attribute values are '[' and ']' respectively.

Input contains this fragment of data/16.gv:

	node
	[
		shape = "record",
	];

And the output log contains:

	|   |---class. Attributes: {uid => "13", value => "node"}
	|   |   |---literal. Attributes: {uid => "14", value => "["}
	|   |   |---shape. Attributes: {uid => "15", value => "record"}
	|   |   |---literal. Attributes: {uid => "16", value => "]"}

=item o edge

The 'value' of the attribute will be either '--' or '->'.

Thus the 'tail' will be the previous daughter (node or subgraph), and the 'head' will be the next.

Samples are:

	n1 -> n2
	n1 -> {n2}
	{n1} -> n2

Note: Post-processing of the tree moves edge attributes off the head daughter (node or subgraph), and
attaches them to the closest proceeding edge.

Thus:

	n1 -> n2 [penwidth = 5]

is stored as though the input were:

	n1 -> [penwidth = 5] n2

This means that at the time of encountering the edge, its attributes are immediately available.

However:

	n1 -> n2 -> n3 [penwidth = 5]

is stored as:

	n1 -> n2 -> [penwidth = 5] n3

Ideally, post-processing will be extended to make that read:

	n1 -> [penwidth = 5] n2 -> [penwidth = 5] n3

=item o equals

The 'value' of the attribute will be '='.

Thus the 'attribute name' will be the previous daughter and the 'attribute value' will be the next.

Input contains this fragment of data/16.gv:

	label = "\"Standard\"\rSyntax\lTest"

And theoutput log contains:

	|   |---node_id. Attributes: {uid => "7", value => "label"}
	|   |---equals. Attributes: {uid => "8", value => "="}
	|   |---node_id. Attributes: {uid => "9", value => "\"Standard\"\rSyntax\lTest"}

=item o literal

'literal' is the name of some nodes, with the 'value' key in the attributes having one of these	values:

=over 4

=item o {

=item o }

=item o [

This indicate the start of a set of attributes for a specific class, edge or node.

The 1st and last daughters will be literals whose attribute 'value' keys are '[' and ']' respectively.

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

See the next point for details about ports and compass points.

Note: A node name can appear more than once in succession, either as a declaration of the node's
existance and then as the tail of an edge, or, as in this fragment of data/56.gv:

	node [shape=rpromoter colorscheme=rdbu5 color=1 style=filled fontcolor=3]; Hef1a; TRE; UAS; Hef1aLacOid;
	Hef1aLacOid [label="Hef1a-LacOid"];

And the output log contains:

	|   |---node_id. Attributes: {uid => "20", value => "Hef1aLacOid"}
	|   |---node_id. Attributes: {uid => "21", value => "Hef1aLacOid"}
	|   |   |---literal. Attributes: {uid => "22", value => "["}
	|   |   |---label. Attributes: {uid => "23", value => "Hef1a-LacOid"}
	|   |   |---literal. Attributes: {uid => "24", value => "]"}

This is a case where tree compression could be done, but isn't (yet).

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

	|   |---node_id. Attributes: {uid => "32", value => "node_16_1:p11"}
	|   |---edge. Attributes: {uid => "35", value => "->"}
	|   |   |---literal. Attributes: {uid => "41", value => "["}
	|   |   |---arrowhead. Attributes: {uid => "42", value => "odiamond"}
	|   |   |---arrowtail. Attributes: {uid => "43", value => "odot"}
	|   |   |---color. Attributes: {uid => "44", value => "red"}
	|   |   |---dir. Attributes: {uid => "45", value => "both"}
	|   |   |---literal. Attributes: {uid => "46", value => "]"}
	|   |---node_id. Attributes: {uid => "36", value => "node_16_2:p22:s"}

You can see the ports and compass points have been incorporated into the 'value' attribute.

=head2 How are comments stored in the tree?

They aren't stored, they are discarded. And this in turn means rendered C<dot> files can't ever contain
them.

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

Don't. Use L<Marpa::R2>.

=head2 If I input x.old.gv and output x.new.gv, should these 2 files be identical?

Yes - at least in the sense that running C<dot> on them will produce the same output files.
This is assuming the default renderer is used.

Since comments in input files are discarded, they can never be in the output file.

=head2 How are custom graph attributes handled?

They are treated like any other attribute. That is, syntax checking is not performed at that level,
but only at the grammatical level. If the construct matches the grammar, this code accepts it.

See data/32.gv.

=head2 How are the demo files generated?

See scripts/generate.demo.sh.

=head2 What files are in fail/?

They are Graphviz files with deliberate syntax errors. Errors here means from the point of view of
Graphviz itself. They help me test the code.

=head1 Machine-Readable Change Log

The file CHANGES was converted into Changelog.ini by L<Module::Metadata::Changes>.

=head1 Version Numbers

Version numbers < 1.00 represent development versions. From 1.00 up, they are production versions.

=head1 Thanks

Many thanks are due to the people who worked on L<Graphviz|http://www.graphviz.org/>.

Jeffrey Kegler wrote L<Marpa::XS>, and has a blog on it at L<http://blogs.perl.org/users/jeffrey_kegler/>.

=head1 Repository

L<https://github.com/ronsavage/GraphViz2-Marpa>

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
	The Artistic License 2.0, a copy of which is available at:
	http://opensource.org/licenses/alphabetical.

=cut
