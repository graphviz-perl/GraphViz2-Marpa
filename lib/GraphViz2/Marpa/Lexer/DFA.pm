package GraphViz2::Marpa::Lexer::DFA;

use strict;
use warnings;

use Hash::FieldHash ':all';

use Set::Array;
use Set::FA::Element;

fieldhash my %attribute      => 'attribute';
fieldhash my %brace_count    => 'brace_count';
fieldhash my %dfa            => 'dfa';
fieldhash my %graph_text     => 'graph_text';
fieldhash my %item_count     => 'item_count';
fieldhash my %items          => 'items';
fieldhash my %logger         => 'logger';
fieldhash my %report_stt     => 'report_stt';
fieldhash my %start          => 'start';
fieldhash my %state          => 'state';
fieldhash my %subgraph       => 'subgraph';
fieldhash my %subgraph_count => 'subgraph_count';
fieldhash my %verbose        => 'verbose';

our $myself; # Is a copy of $self for functions called by Set::FA::Element.
our $VERSION = '1.11';

# --------------------------------------------------

sub check_end_subgraph
{
	my($self) = @_;

	# We only ever get the with $match eq '}'.

	$self -> new_item('end_scope', $self -> brace_count);
	$self -> decrement_brace_count;

	if ($self -> subgraph -> length && ($self -> subgraph -> last == $self -> brace_count) )
	{
		$self -> subgraph -> pop;
		$self -> new_item('end_subgraph', $self -> subgraph_count);
		$self -> decrement_subgraph_count;
	}


} # End of check_end_subgraph.

# --------------------------------------------------

sub _clean_up
{
	my($self) = @_;

	# Rectify lexed items, if necessary.
	# 1: This graph:
	# digraph abstract {size = "6,6" S24 -> 27}
	# lexes as:                         instead of as:
	# "type","value"                    "type","value"
	# strict              , "no"        strict              , "no"
	# digraph             , "yes"       digraph             , "yes"
	# graph_id            , "abstract"  graph_id            , "abstract"
	# start_scope         , "1"         start_scope         , "1"
	# id                  , "size"      attribute_id        , "size"    <=
	# equals              , "="         equals              , "="
	# id                  , "6,6"       attribute_value     , "6,6"     <=
	# id                  , "S24"       id                  , "S24"
	# edge_id             , "->"        edge_id             , "->"
	# id                  , "27"        id                  , "27"
	# end_scope           , "1"         end_scope           , "1"

	my(@old_items) = $self -> items -> print;

	for my $i (0 .. $#old_items - 2)
	{
		if ( ($old_items[$i]{type} eq 'id') && ($old_items[$i + 1]{type} eq 'equals') && ($old_items[$i + 2]{type} eq 'id') )
		{
			$old_items[$i]{type}     = 'attribute_id';
			$old_items[$i + 2]{type} = 'attribute_value';

			$self -> items -> set($i, $old_items[$i]);
			$self -> items -> set($i + 2, $old_items[$i + 2]);
		}
	}

	# 2: This graph:
	# digraph abstract {label = "Test" node_1 -> node_2}
	# lexes as:                         instead of as:
	# "type","value"                    "type","value"
	# strict              , "no"        strict              , "no"
	# digraph             , "yes"       digraph             , "yes"
	# graph_id            , "abstract"  graph_id            , "abstract"
	# start_scope         , "1"         start_scope         , "1"
	#                                   id                  , "graph"  <=
	#                                   open_bracket        , "["      <=
	# attribute_id        , "label"     attribute_id        , "label"
	# equals              , "="         equals              , "="
	# attribute_value     , "Test"      attribute_value     , "Test"
	#                                   close_bracket       , "]"      <=
	# id                  , "node_2"    id                  , "node_2"
	# edge_id             , "->"        edge_id             , "->"
	# id                  , "node_2"    id                  , "node_2"
	# end_scope           , "1"         end_scope           , "1"

	my($inside_brackets) = 0;
	@old_items           = $self -> items -> print;

	my(@new_items);

	for (my $i = 0; $i <= $#old_items; $i++)
	{
		if ($old_items[$i]{type} eq 'open_bracket')
		{
			$inside_brackets++;

			push @new_items, $old_items[$i];
		}
		elsif ($old_items[$i]{type} eq 'close_bracket')
		{
			$inside_brackets--;

			push @new_items, $old_items[$i];
		}
		elsif ( ($old_items[$i]{type} eq 'attribute_id') && ($inside_brackets == 0) )
		{
			push @new_items,
			{
				count => 0,
				name  => '',
				type  => 'id',
				value => 'graph',
			},
			{
				count => 0,
				name  => '',
				type  => 'open_bracket',
				value => '[',
			},
			$old_items[$i],
			$old_items[$i + 1],
			$old_items[$i + 2],
			{
				count => 0,
				name  => '',
				type  => 'close_bracket',
				value => ']',
			};

			$i +=2;
		}
		else
		{
			push @new_items, $old_items[$i];
		}
	}

	# 3: This graph:
	# digraph graph_42_01 {node_42_01 []}
	# lexes as:                             instead of as:
	# "type","value"                        "type","value"
	# strict              , "no"            strict              , "no"
	# digraph             , "yes"           digraph             , "yes"
	# graph_id            , "graph_42_01"   graph_id            , "graph_42_01"
	# start_scope         , "1"             start_scope         , "1"
	# node_id             , "node_42_01"    node_id             , "node_42_01"
	# open_bracket        , "["
	# close_bracket       , "]"
	# end_scope           , "1"             end_scope           , "1"

	@old_items = ();

	for (my $i = 0; $i < $#new_items; $i++)
	{
		if ( ($new_items[$i]{type} eq 'open_bracket') && ($new_items[$i + 1]{type} eq 'close_bracket') )
		{
			$i += 1;
		}
		else
		{
			push @old_items, $new_items[$i];
		}
	}

	push @old_items, $new_items[$#new_items];

	# 4: This graph:
	# digraph graph_15 {node_15_1:port_15_1 -> node_15_2:port_15_2 [arrowhead = odot]}
	# lexes as:                             instead of as:
	# "type","value"                        "type","value"
	# strict              , "no"            strict              , "no"
	# digraph             , "yes"           digraph             , "yes"
	# graph_id            , "graph_15"      graph_id            , "graph_15"
	# start_scope         , "1"             start_scope         , "1"
	# node_id             , "node_15_1"     node_id             , "node_15_1"
	# open_bracket        , "["             open_bracket        , "["
	# attribute_id        , "port_id"       attribute_id        , "port_id"
	# attribute_value     , "port_15_1"     attribute_value     , "port_15_1"
	# close_bracket       , "]"             close_bracket       , "]"
	# edge_id             , "->"            edge_id             , "->"
	# node_id             , "node_15_2"     node_id             , "node_15_2"
	# open_bracket        , "["             open_bracket        , "["
	# attribute_id        , "port_id"       attribute_id        , "port_id"
	# attribute_value     , "port_15_2"     attribute_value     , "port_15_2"
	# close_bracket       , "]"                                                <=
	# open_bracket        , "["                                                <=
	# attribute_id        , "arrowhead"     attribute_id        , "arrowhead"
	# attribute_value     , "odot"          attribute_value     , "odot"
	# close_bracket       , "]"             close_bracket       , "]"
	# end_scope           , "1"             end_scope           , "1"

	@new_items = ();

	for (my $i = 0; $i < $#old_items; $i++)
	{
		if ( ($old_items[$i]{type} eq 'close_bracket') && ($old_items[$i + 1]{type} eq 'open_bracket') )
		{
			$i += 1;
		}
		else
		{
			push @new_items, $old_items[$i];
		}
	}

	push @new_items, $old_items[$#old_items];

	# 5: Convert all 'id's to 'class_id/node_id's. I /think/ this is ok.

	@old_items = ();

	for my $i (0 .. $#new_items)
	{
		if ($new_items[$i]{type} eq 'id')
		{
			if ($new_items[$i]{value} =~ /^edge|graph|node$/)
			{
				$new_items[$i]{type} = 'class_id';
			}
			else
			{
				$new_items[$i]{type} = 'node_id';
			}
		}

		push @old_items, $new_items[$i];
	}

	$self -> items -> clear;
	$self -> items -> push(@old_items);
	$self -> renumber_items;

} # End of _clean_up.

# --------------------------------------------------

sub decrement_brace_count
{
	my($self) = @_;

	# Warning! Don't use:
	# return $self -> brace_count($self -> brace_count - 1);
	# It returns $self.

	$self -> brace_count($self -> brace_count - 1);

	return $self -> brace_count;

} # End of decrement_brace_count.

# --------------------------------------------------

sub decrement_subgraph_count
{
	my($self) = @_;

	# Warning! Don't use:
	# return $self -> subgraph_count($self -> subgraph_count - 1);
	# It returns $self.

	$self -> subgraph_count($self -> subgraph_count - 1);

	return $self -> subgraph_count;

} # End of decrement_subgraph_count.

# --------------------------------------------------
# Warning: This is a function.

sub fix_node_id
{
	my($dfa) = @_;
	my($value) = trim($dfa -> match);

	$myself -> log(debug => "fix_node_id($value)");

	# If we get here by matching ':', we're just got a node ID.

	if ($value eq ':')
	{
		my($id) = $myself -> items -> pop;

		$myself -> items -> push
			({
				count => $$id{count},
				name  => 'node_id',
				type  => '',
				value => $$id{type},
			 });
	}

} # End of fix_node_id.

# --------------------------------------------------

sub increment_brace_count
{
	my($self) = @_;

	# Warning! Don't use:
	# return $self -> brace_count($self -> brace_count + 1);
	# It returns $self.

	$self -> brace_count($self -> brace_count + 1);

	return $self -> brace_count;

} # End of increment_brace_count.

# --------------------------------------------------

sub increment_item_count
{
	my($self, $name) = @_;

	# Warning! Don't use:
	# return $self -> item_count($self -> item_count + 1);
	# It returns $self.

	$self -> item_count($self -> item_count + 1);

	return $self -> item_count;

} # End of increment_item_count.

# --------------------------------------------------

sub increment_subgraph_count
{
	my($self) = @_;

	# Warning! Don't use:
	# return $self -> subgraph_count($self -> subgraph_count + 1);
	# It returns $self.

	$self -> subgraph_count($self -> subgraph_count + 1);

	return $self -> subgraph_count;

} # End of increment_subgraph_count.

# --------------------------------------------------

sub _init
{
	my($self, $arg)       = @_;
	$$arg{attribute}      = Set::Array -> new; # Attributes are [key] or [key = value]. Convert former to latter.
	$$arg{brace_count}    = 0;
	$$arg{dfa}            = '';
	$$arg{graph_text}     = $$arg{graph_text} || die 'Error: No value supplied for graph_text';
	$$arg{item_count}     = 0;
	$$arg{items}          = Set::Array -> new;
	$$arg{logger}         ||= defined($$arg{logger}) ? $$arg{logger} : undef; # Caller can set.
	$$arg{report_stt}     ||= 0;     # Caller can set.
	$$arg{start}          ||= $$arg{start} || die 'Error: No value supplied for start';
	$$arg{state}          ||= $$arg{state} || die 'Error: No value supplied for state';
	$$arg{subgraph}       = Set::Array -> new;
	$$arg{subgraph_count} = 0;
	$$arg{verbose}        ||= 0;  # Caller can set.
	$self                 = from_hash($self, $arg);
	$myself               = $self;

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

sub _process_graph
{
	my($self)   = @_;
	my($result) = 1 - $self -> dfa -> accept($self -> graph_text);

	if ($result)
	{
		$self -> log(error => "Error: The final state '@{[$self -> dfa -> current]}' is not an accepting state");
	}
	else
	{
		$self -> _clean_up;
	}

	# Return 0 for success and 1 for failure.

	return $result;

} # End of _process_graph.

# -----------------------------------------------

sub renumber_items
{
	my($self)  = @_;
	my(@item)  = @{$self -> items};
	my($count) = 0;

	my(@new);

	for my $item (@item)
	{
		$$item{count} = ++$count;

		push @new, $item;
	}

	$self -> items(Set::Array -> new(@new) );

} # End of renumber_items.

# --------------------------------------------------

sub run
{
	my($self)        = @_;
	my($state_stuff) = $self -> state;

	# Build structures required by Set::FA::Element.

	my(%actions, @accept);
	my($entry, $exit);
	my($item);
	my(@stt);
	my(@transitions);

	for my $state (keys %$state_stuff)
	{
		for my $event_index (0 .. $#{$$state_stuff{$state} })
		{
			if (! $actions{$state})
			{
				$actions{$state} = {};
			}

			$item  = ${$$state_stuff{$state} }[$event_index];
			$entry = $$item{entry};
			$exit  = $$item{exit};

			if ($entry)
			{
				$actions{$state}{entry} = [\&$entry, $entry];
			}

			if ($exit)
			{
				$actions{$state}{exit} = [\&$exit, $exit];
			}

			push @accept, $$item{accept} if ($$item{accept});
			push @stt, "['$state', '$$item{event}', '$$item{next_state}']";
			push @transitions, [$state, $$item{event}, $$item{next_state}];
		}
	}

	# Build and run the DFA.

	$self -> dfa
		(
		 Set::FA::Element -> new
		 (
		  accepting   => \@accept,
		  actions     => \%actions,
		  die_on_loop => 1,
		  logger      => $self -> logger,
		  start       => $self -> start,
		  transitions => \@transitions,
		  verbose     => $self -> verbose,
		 )
		);

	$self -> dfa -> report if ($self -> report_stt);

	# Return 0 for success and 1 for failure.

	return $self -> _process_graph;

} # End of run.

# --------------------------------------------------
# Warning: This is a function.

sub save_attribute
{
	my($dfa)   = @_;
	my($value) = trim($dfa -> match);

	$myself -> log(debug => "save_attribute($value)");

	# Handle the comma between attributes.

	return if ($value eq ',');

	# Check if we have [key] or [key = value].

	if ($value eq '=')
	{
		# If it's [key = ...] we're happy. Let the following code deal with it.

		$myself -> attribute -> push($value);
	}
	elsif ($myself -> attribute -> length == 0)
	{
		# If the stack is empty, it's [key...] so far. Push key i.e.  $value

		$myself -> attribute -> push($value);
	}
	elsif ($myself -> attribute -> length == 1)
	{
		# If the stack is not empty, and it's not '=', then it's [key].

		$myself -> new_item('equals', '=');
		$myself -> new_item('attribute_value', 1); # 1 => True, the default value in the dot language.
		$myself -> attribute -> clear;
		$myself -> attribute -> push($value);
	}
	else
	{
		# If the stack contains 'key' and '=', then it's [key = value], so we're finished.
		$myself -> attribute -> clear;
	}

	my($type) = 'attribute_' . ($myself -> attribute -> length == 1 ? 'id' : 'value');

	$myself -> decrement_brace_count if ($value eq '}');
	$myself -> new_item($value eq ']' ? 'close_bracket' : $value eq '=' ? 'equals' : $type, $value);
	$myself -> attribute -> clear if ($value eq ']');

} # End of save_attribute.

# --------------------------------------------------
# Warning: This is a function.

sub save_graph_id
{
	my($dfa)   = @_;
	my($value) = trim($dfa -> match);

	$myself -> log(debug => "save_graph_id($value)");

	if ($value eq '{')
	{
		$myself -> new_item('graph_id', '');
		$myself -> new_item('start_scope', $myself -> increment_brace_count);
	}
	else
	{
		$myself -> new_item('graph_id', $value);
	}

} # End of save_graph_id.

# --------------------------------------------------
# Warning: This is a function.

sub save_id_1
{
	my($dfa)   = @_;
	my($value) = trim($dfa -> match);

	$myself -> log(debug => "save_id_1($value)");

	# Handle end-of-statement '}'.

	if ($value eq '}')
	{
		$myself -> check_end_subgraph;

		return;
	}

	if ($value eq 'subgraph')
	{
		# Later, this tells us when an end_scope closes a subgraph. See check_end_subgraph().

		$myself -> subgraph -> push($myself -> brace_count);

		$myself -> new_item('start_subgraph', $myself -> increment_subgraph_count);
	}
	elsif ($value =~ /^-/)
	{
		$myself -> new_item('edge_id', $value);
	}
	elsif ($value eq '{')
	{
		$myself -> new_item('start_scope', $myself -> increment_brace_count);
	}
	elsif ($value eq '[')
	{
		$myself -> new_item('open_bracket', $value);
	}
	else
	{
		$myself -> new_item('id', $value);
	}

} # End of save_id_1.

# --------------------------------------------------
# Warning: This is a function.

sub save_id_2
{
	my($dfa)   = @_;
	my($value) = trim($dfa -> match);

	$myself -> log(debug => "save_id_2($value)");

	if ($value eq '}')
	{
		$myself -> check_end_subgraph;
	}
	elsif ($value eq 'subgraph')
	{
		# Later, this tells us when an end_scope closes a subgraph. See check_end_subgraph().

		$myself -> subgraph -> push($myself -> brace_count);
		$myself -> new_item('start_subgraph', $myself -> increment_subgraph_count);
	}
	elsif ($value =~ /^:(.+):(n|ne|se|e|se|s|sw|w|nw|c|_)$/)
	{
		# We got a port /and/ a compass point. Output 8 tokens.

		$myself -> new_item('open_bracket', '[');
		$myself -> new_item('attribute_id', 'port_id');
		$myself -> new_item('equals', '=');
		$myself -> new_item('attribute_value', trim($1) );
		$myself -> new_item('attribute_id', 'compass_point');
		$myself -> new_item('equals', '=');
		$myself -> new_item('attribute_value', trim($2) );
		$myself -> new_item('close_bracket', ']');
	}
	elsif ($value eq '{')
	{
		$myself -> new_item('start_scope', $myself -> increment_brace_count);
	}
	elsif ($value =~ /^:(n|ne|se|e|se|s|sw|w|nw|c|_)$/)
	{
		# We got a compass point. Output 5 tokens.

		$myself -> new_item('open_bracket', '[');
		$myself -> new_item('attribute_id', 'compass_point');
		$myself -> new_item('equals', '=');
		$myself -> new_item('attribute_value', trim($1) );
		$myself -> new_item('close_bracket', ']');
	}
	elsif ($value =~ /^:(.+)/)
	{
		# We got a port id. Output 5 tokens.

		$myself -> new_item('open_bracket', '[');
		$myself -> new_item('attribute_id', 'port_id');
		$myself -> new_item('equals', '=');
		$myself -> new_item('attribute_value', trim($1) );
		$myself -> new_item('close_bracket', ']');
	}
	else
	{
		my($type) = 'id';
		$type     = $value eq '=' ? 'equals' : $value eq '[' ? 'open_bracket' : $value =~ /^-/ ? 'edge_id' : $type;

		$myself -> new_item($type, $value);
	}

} # End of save_id_2.

# --------------------------------------------------
# Warning: This is a function.

sub save_prefix
{
	my($dfa)   = @_;
	my($value) = trim($dfa -> match);

	# Upon each call, $value will be 1 of:
	# o strict.
	# o digraph.
	# o graph.

	$myself -> log(debug => "save_prefix($value)");

	# Output:
	# o strict  => {name => strict,  value => yes/no}.
	# o digraph => {name => digraph, value => yes}.
	# o graph   => {name => digraph, value => no}.

	if ($value eq 'strict')
	{
		$myself -> new_item($value, 'yes');
	}
	else
	{
		# If the first token is '(di)graph' (i.e. there was no 'strict'),
		# jam a 'strict' into the output stream.

		if ($myself -> items -> length == 0)
		{
			$myself -> new_item('strict', 'no');
		}

		$myself -> new_item('digraph', $value eq 'digraph' ? 'yes' : 'no');
	}

} # End of save_prefix.

# --------------------------------------------------
# Warning: This is a function.

sub start_statements
{
	my($dfa) = @_;
	my($value) = trim($dfa -> match);

	# We only ever get here (via the STT) with $value eq '{'.

	$myself -> log(debug => "start_statements($value)");
	$myself -> new_item('start_scope', $myself -> increment_brace_count);

} # End of start_statements.

# --------------------------------------------------
# Warning: This is a function.

sub trim
{
	my($s) = @_;
	$s =~ s/^\s+//;
	$s =~ s/\s+$//;
	$s =~ s/^"(.*)"$/$1/;

	return $s;

} # End of trim.

# --------------------------------------------------

1;

=pod

=head1 NAME

L<GraphViz2::Marpa::Lexer::DFA> - A Perl lexer for Graphviz dot files

=head1 Synopsis

This module is called from L<GraphViz2::Marpa::Lexer>.

=head1 Description

L<GraphViz2::Marpa::Lexer::DFA> provides a L<Set:FA::Element>-based lexer for L<Graphviz|http://www.graphviz.org/> (dot) graph definitions.

Demo lexer/parser output: L<http://savage.net.au/Perl-modules/html/graphviz2.marpa/index.html>.

State Transition Table: L<http://savage.net.au/Perl-modules/html/graphviz2.marpa/default.stt.html>.

Command line options and object attributes: L<http://savage.net.au/Perl-modules/html/graphviz2.marpa/code.attributes.html>.

My article on this set of modules: L<http://www.perl.com/pub/2012/10/an-overview-of-lexing-and-parsing.html>.

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

C<new()> is called as C<< my($dfa) = GraphViz2::Marpa::Lexer::DFA -> new(k1 => v1, k2 => v2, ...) >>.

It returns a new object of type C<GraphViz2::Marpa::Lexer::DFA>.

Key-value pairs accepted in the parameter list (see corresponding methods for details
[e.g. graph_text()]):

=over 4

=item o graph_text => $string

Specify a string for the L<Graphviz|http://www.graphviz.org/> (dot) graph definition.

Default: ''.

=item o logger => $logger

Specify a logger object to use.

Default: ''.

=item o report_stt => $Boolean

Log the State Transition Table.

Default: 0.

=item o start => $start_state_name

Specify the name of the start state.

There is no default. The code dies if a value is not supplied.

=item o state => $state_hashref

Specify the State Transition Table.

There is no default. The code dies if a value is not supplied.

=item o verbose => $Boolean

Specify the verbosity level in the call to L<Set::FA::Element>.

The value is only used by the latter module when a logger is not passed to it from the current module.

Default: 0.

=back

=head1 Methods

=head2 graph_text([$graph])

The [] indicate an optional parameter.

Get or set the L<Graphviz|http://www.graphviz.org/> (dot) graph definition.

'graph_text' is a parameter to L</new()>. See L</Constructor and Initialization> for details.

=head2 items()

Returns a object of type L<Set::Array>, which is an arrayref of items output by the DFA.

These items are I<not> the same as the arrayref of items returned by the items() method in
L<GraphViz2::Marpa::Parser>, but they are the same as in L<GraphViz2::Marpa::Lexer>.

Each element is a hashref with these keys:

=over 4

=item o count => $integer

Just counts the items as 1 .. N.

=item o name => $string

Not used. Always ''.

=item o type => $string

The type of the token.

The range of values is documented in the lexer's FAQ item: L<How is the lexed graph stored in RAM?|GraphViz2::Marpa::Lexer>.

=item o value => $string

The value of the token from the input stream.

=back

Usage:

	my(@items) = @{$dfa -> items};

=head2 log($level, $s)

Calls $self -> logger -> $level($s) if ($self -> logger).

=head2 logger([$logger_object])

Here, the [] indicate an optional parameter.

Get or set the logger object.

=head2 new()

See L</Constructor and Initialization> for details on the parameters accepted by L</new()>.

=head2 report_stt([$Boolean])

The [] indicate an optional parameter.

Get or set the value which determines whether or not to log the parsed state transition table (STT).

Calls L<Set::FA::Element/report()>. Set min and max log levels to 'info' for this.

'report_stt' is a parameter to L</new()>. See L</Constructor and Initialization> for details.

=head2 run()

Runs the state machine.

Afterwards, you call L</items()> to retrieve the arrayref of results.

=head2 start([$start_state_name])

The [] indicate an optional parameter.

Get or set the name of the state in which the STT starts.

'start' is a parameter to L</new()>. See L</Constructor and Initialization> for details.

=head2 state([\%state_hashref])

The [] indicate an optional parameter.

Get or set the hashref defining the STT.

'state' is a parameter to L</new()>. See L</Constructor and Initialization> for details.

=head2 verbose([$Boolean])

The [] indicate an optional parameter.

Get or set the verbosity level when calling L<Set::FA::Element>.

'verbose' is a parameter to L</new()>. See L</Constructor and Initialization> for details.

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
