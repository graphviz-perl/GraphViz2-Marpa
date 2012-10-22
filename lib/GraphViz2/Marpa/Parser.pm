package GraphViz2::Marpa::Parser;

use strict;
use warnings;

use Data::Dumper::Concise; # For Dumper().

use GraphViz2::Marpa::Renderer::GraphViz2;
use GraphViz2::Marpa::Utils;

use Hash::FieldHash ':all';

use IO::File;

use Log::Handler;

use Marpa::R2;

use Set::Array;

use Tree;

use Text::CSV_XS;

use Try::Tiny;

fieldhash my %forest        => 'forest';
fieldhash my %global        => 'global';
fieldhash my %graph         => 'graph';
fieldhash my %item_count    => 'item_count';
fieldhash my %items         => 'items';
fieldhash my %lexed_file    => 'lexed_file';
fieldhash my %logger        => 'logger';
fieldhash my %maxlevel      => 'maxlevel';
fieldhash my %minlevel      => 'minlevel';
fieldhash my %node          => 'node';
fieldhash my %output_file   => 'output_file';
fieldhash my %parsed_file   => 'parsed_file';
fieldhash my %renderer      => 'renderer';
fieldhash my %report_forest => 'report_forest';
fieldhash my %report_items  => 'report_items';
fieldhash my %tokens        => 'tokens';
fieldhash my %utils         => 'utils';

# $myself is a copy of $self for use by functions called by Marpa.

our $myself;
our $VERSION = '1.06';

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

# -----------------------------------------------

sub _build_attribute_list
{
	my($self, $items, $i) = @_;

	my(%attribute);
	my($j);

	if ($$items[$i + 1]{type} eq 'start_attribute')
	{
		for ($j = $i + 2; ($j <= $#$items) && ($$items[$j]{type} ne 'end_attribute'); $j += 2)
		{
			$attribute{$$items[$j]{value} } = $$items[$j + 1]{value};
		}
	}
	else
	{
		$j = $i;
	}

	return ($j, {%attribute});

} # End of _build_attribute_list.

# -----------------------------------------------

sub _build_node_list
{
	my($self, $items, $i) = @_;
	my($j) = $i - 1;

	my(@node);

	while (1)
	{
		last if ( ($j > $#$items) || ($$items[$j]{type} ne 'node_id') );

		push @node, $$items[$j]{value};

		last if ($$items[$j + 1]{type} ne 'edge_id');

		$j += 2;
	}

	($j, my $attribute) = $self -> _build_attribute_list($items, $j);

	return ($j, [@node], $attribute);

} # End of _build_node_list.

# -----------------------------------------------
# Outputs:
# o $self -> forest, an object of type Tree.
# o $self -> graph, a hashref of graph attributes.
# o $self -> global, a hashref of (digraph => $Boolean, graph_id => $string).
# o $self -> node, a hashref of {node => hashref of attributes}.

sub _build_tree
{
	my($self) = @_;

	# Phase 1: Find class (edge, graph and node) attributes,
	# and find attributes of individual nodes.

	my(@class)  = qw/edge graph node/;
	my($global) = {};
	my($i)      = - 1;
	my($items)  = $self -> items;

	my($attribute, %attribute);
	my($class_attribute, %class_attribute, $child);
	my($graph_id);
	my($j);
	my($node, $node_set, %node);
	my($parent);
	my(@stack);
	my($type);
	my($value);

	$class_attribute{$_} = {} for (@class);

	while ($i < $#$items)
	{
		$i++;

		$type  = $$items[$i]{type};
		$value = $$items[$i]{value};

		$self -> log(notice => "$i: $type => $value");

		if ($type eq 'class_id')
		{
			($i, $class_attribute)   = $self -> _build_attribute_list($items, $i);
			$class_attribute{$value} = {%{$class_attribute{$value} }, %$class_attribute};
		}
		elsif ($type eq 'digraph')
		{
			$$global{digraph} = $value eq 'yes' ? 1 : 0;
		}
		elsif ($type eq 'edge_id')
		{
			($i, $node_set, $attribute) = $self -> _build_node_list($items, $i);
			$attribute                  = {%{$class_attribute{edge} }, %$attribute};
			$parent                     = $self -> forest;

			for $j (0 .. $#$node_set)
			{
				$node        = $$node_set[$j];
				$node{$node} = {attribute => {%{$class_attribute{node} } }, fixed => 0} if (! $node{$node});
				$child       = Tree -> new($node);

				$parent -> add_child($child);
				$parent -> meta($attribute) if (! $parent -> is_root);

				$parent = $child;
			}
		}
		elsif ($type eq 'end_scope')
		{
			$class_attribute{$_} = pop @stack for reverse (@class);

		}
		elsif ($type eq 'graph_id')
		{
			$$global{graph_id} = $value if (! defined $$global{graph_id});
			$graph_id          = $value;
		}
		elsif ($type eq 'node_id')
		{
			# Declare this node if we haven't see it before.

			$node{$value} = {attribute => {%{$class_attribute{node} } }, fixed => 0} if (! $node{$value});

			# If it's followed by an edge, any attributes belong to the edge.
			# So, don't try to update the node's attributes.

			if ($$items[$i + 1]{type} eq 'edge_id')
			{
			}
			else
			{
				# If the node has been explicitly declared (fixed == 1),
				# only update it's attributes with non-class attributes.

				($i, $attribute) = $self -> _build_attribute_list($items, $i);

				if ($node{$value}{fixed} == 1)
				{
					$node{$value}{attribute} = {%{$node{$value}{attribute} }, %$attribute};
				}
				else
				{
					# Otherwise, include class attributes.

					$node{$value}{attribute} = {%{$class_attribute{node} }, %{$node{$value}{attribute} }, %$attribute};
				}

				$node{$value}{fixed} = 1;
			}
		}
		elsif ($type eq 'start_scope')
		{
			next if ($value == 1); # First entry.

			push @stack, $class_attribute{$_} for (@class);
		}
		elsif ($type eq 'strict')
		{
			$$global{strict} = $value eq 'yes' ? 1 : 0;
		}
	}

	$self -> global($global);
	$self -> graph($class_attribute{graph});
	$self -> node(\%node);

} # End of _build_tree.

# --------------------------------------------------
# This is a function, not a method.

sub class_id
{
	my($stash, $t1, undef, $t2)  = @_;

	$myself -> new_item('class_id', $t1);

	return $t1;

} # End of class_id.

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

sub default_action
{
	my($stash, $t1, undef, $t2)  = @_;

	# Return 0 for success and 1 for failure.

	return 0;

} # End of default_action.

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

sub end_scope
{
	my($stash, $t1, undef, $t2)  = @_;

	$myself -> new_item('end_scope', $t1);

	return $t1;

} # End of end_scope.

# --------------------------------------------------
# This is a function, not a method.

sub end_subgraph
{
	my($stash, $t1, undef, $t2)  = @_;

	$myself -> new_item('end_subgraph', $t1);

	return $t1;

} # End of end_subgraph.

# --------------------------------------------------

sub format_node
{
	my($self, $node) = @_;

	return
		$node -> value .
		'. Edge attrs: ' .
		$self -> hashref2string($node -> meta);

} # End of format_node.

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
	my($grammar) = Marpa::R2::Grammar -> new
		({
		actions        => __PACKAGE__,
		default_action => 'default_action',
		start          => 'graph_grammar',
		rules          =>
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
				 rhs => [qw/start_skope graph_sequence_list end_skope/],
			 },
			 {
				 lhs    => 'start_skope',
				 rhs    => [qw/start_scope/],
				 action => 'start_scope',
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
				 lhs    => 'end_skope',
				 rhs    => [qw/end_scope/],
				 action => 'end_scope',
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
				 rhs => [qw/start_skope graph_sequence_list end_skope/],
			 },
			 {
				 lhs => 'node_statement', # 1 of 5.
				 rhs => [qw/class_item/],
			 },
			 {
				 lhs => 'node_statement', # 2 of 5.
				 rhs => [qw/node_item/],
			 },
			 {
				 lhs => 'node_statement', # 3 of 5.
				 rhs => [qw/node_item colon_item port_item/],
			 },
			 {
				 lhs => 'node_statement', # 4 of 5.
				 rhs => [qw/node_item colon_item port_item colon_item compass_item/],
			 },
			 {
				 lhs => 'node_statement', # 5 of 5.
				 rhs => [qw/node_item colon_item compass_item/],
			 },
			 {
				 lhs    => 'class_item',
				 rhs    => [qw/class_id/],
				 action => 'class_id',
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

# -----------------------------------------------

sub hashref2string
{
	my($self, $h) = @_;
	$h ||= {};

	return '{' . join(', ', map{"$_ => $$h{$_}"} sort keys %$h) . '}';

} # End of hashref2string.

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
	my($self, $arg)      = @_;
	$$arg{forest}        = Tree -> new('root');
	$$arg{global}        = {};
	$$arg{graph}         = {};
	$$arg{item_count}    = 0;
	$$arg{items}         = Set::Array -> new;
	$$arg{lexed_file}    ||= '';       # Caller can set.
	$$arg{logger}        = defined($$arg{logger}) ? $$arg{logger} : undef; # Caller can set.
	$$arg{maxlevel}      ||= 'notice'; # Caller can set.
	$$arg{minlevel}      ||= 'error';  # Caller can set.
	$$arg{node}          = {};
	$$arg{output_file}   ||= '';       # Caller can set.
	$$arg{parsed_file}   ||= '';       # Caller can set.
	$$arg{renderer}      = defined($$arg{renderer}) ? $$arg{renderer} : undef; # Caller can set.
	$$arg{report_forest} ||= 0;        # Caller can set.
	$$arg{report_items}  ||= 0;        # Caller can set.
	$$arg{tokens}        ||= [];       # Caller can set.
	$$arg{utils}         = GraphViz2::Marpa::Utils -> new;
	$self                = from_hash($self, $arg);
	$myself              = $self;

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
	$level = 'notice' if (! defined $level);
	$s     = ''       if (! defined $s);

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

# -----------------------------------------------

sub pretty_print_forest
{
	my($self) = @_;

	my(@out);
	my(@vert_dashes);

	for my $t ($self -> forest -> traverse)
	{
		push @out, $self -> pretty_print_node($t, \@vert_dashes);
	}

	$self -> log(notice => $_) for @out;

} # End of pretty_print_forest.

# -----------------------------------------------

sub pretty_print_node
{
	my($self, $t, $vert_dashes) = @_;
	my($depth)         = $t -> depth;
	my($sibling_count) = $t -> is_root ? 1 : scalar $t -> parent -> children;
	my($offset)        = ' ' x 4;
	my(@indent)        = map{$$vert_dashes[$_] || $offset} 0 .. $depth - 1;
	@$vert_dashes      =
	(
		@indent,
		($sibling_count == 1 ? $offset : '   |'),
	);

	if ($sibling_count == ($t -> get_index_for($t) + 1) )
	{
		$$vert_dashes[$depth] = $offset;
	}

	return
		join('' => @indent[1 .. $#indent])
		. ($depth ? '   |---' : '')
		. $self -> format_node($t);

} # End of pretty_print_node.

# --------------------------------------------------

sub print_forest
{
	my($self)   = @_;
	my($global) = $self -> global;

	$self -> log(notice => 'Globals:');
	$self -> log(notice => join(', ', map{"$_ => $$global{$_}"} sort keys %$global) );
	$self -> log(notice => 'Nodes:');

	my($node) = $self -> node;

	for my $name (sort keys %$node)
	{
		$self -> log(notice => "$name. Attr: " . $self -> hashref2string($$node{$name}{attribute}) );
	}

	$self -> log(notice => 'Edges:');
	$self -> pretty_print_forest;

} # End of print_forest.

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
	my($recognizer) = Marpa::R2::Recognizer -> new({grammar => $self -> grammar});

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
	$result     = ref $result ? $$result : $result;
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

	# Build a Graphviz-like tree of the data.

	$self -> _build_tree;
	$self -> print_forest if ($self -> report_forest);

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

sub start_scope
{
	my($stash, $t1, undef, $t2)  = @_;

	$myself -> new_item('start_scope', $t1);

	return $t1;

} # End of start_scope.

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

=head1 Description

L<GraphViz2::Marpa::Lexer> provides a L<Marpa::XS>-based parser for L<http://www.graphviz.org/> dot files.

The input is expected to be, via RAM or a CSV file, from L<GraphViz2::Marpa::Lexer>.

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

=head2 Are the certain cases I should watch out for?

Yes. Consider these 3 situations and their corresponding lexed or parsed output:

=over 4

=item o digraph g {...}

	digraph     , "yes"
	graph_id    , "g"
	start_scope , "1"

=over 4

=item o The I<start_scope> count must be 1 because it's at the very start of the graph

=back

=item o subgraph s {...}

	start_subgraph , "1"
	graph_id       , "s"
	start_scope    , "2"

=over 4

=item o The I<start_scope> count must be 2 or more

=item o When I<start_scope> is preceeded by I<graph_id>, it's a subgraph

=item o Given 'subgraph {...}', the I<graph_id> will be ""

=back

=item o {...}

	start_scope , "2"

=over 4

=item o The I<start_scope> count must be 2 or more

=item o When I<start_scope> is I<not> preceeded by I<graph_id>, it's a stand-alone {...}

=back

=back

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

=item o class_id => /^edge|graph|node$/

This represents 3 special tokens where the author of the dot file used one or more of the 3 words
edge, graph, or node, to specify attributes which apply to all such cases. So:

	node [shape = Msquare]

means all nodes after this point in the input stream default to having a square shape. Of course this
can be overidden by another such line, or by any specific node having a shape as part of its list of
attributes.

See data/51.* for sample code.

=item o colon => ':'

This separates nodes from ports and ports from compass points.

=item o compass_point => $id

=item o digraph => $yes_no

'yes' => digraph and 'no' => graph.

=item o edge_id => $id

$id is either '->' for a digraph or '--' for a graph.

=item o end_attribute => ']'

This indicates the end of a set of attributes.

=item o end_scope => $brace_count

This indicates the end of a graph or subgraph or any stand-alone {}, and - for subgraphs - preceeds the subgraph's 'end_subgraph'.

$brace_count increments by 1 each time 'graph_id' is detected in the input string, and decrements each time a matching 'end_scope' is detected.

=item o end_subgraph => $subgraph_count

This indicates the end of a subgraph, and follows the subgraph's 'end_scope'.

$subgraph_count increments by 1 each time 'start_subgraph' is detected in the input string, and decrements each time a matching 'end_subgraph' is detected.

=item o graph_id => $id

This indicates both the graph's $id and each subgraph's $id.

For graphs and subgraphs, the $id may be '' (the empty string).

=item o node_id => $id

=item o port_id => $id

=item o start_attribute => '['

This indicates the start of a set of attributes.

=item o start_scope => $brace_count

This indicates the start of the graph, a subgraph, or any stand-alone {}.

$brace_count increments by 1 each time 'graph_id' is detected in the input string, and decrements each time a matching 'end_scope' is detected.

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
