package GraphViz2::Marpa::Parser::TreeUtils;

use parent 'GraphViz2::Marpa::Parser';
use strict;
use warnings;

use Hash::FieldHash ':all';

use Tree::DAG_Node;

fieldhash my %allow_loops  => 'allow_loops';
fieldhash my %fixed_paths  => 'fixed_paths';
fieldhash my %image_size   => 'image_size';
fieldhash my %path_length  => 'path_length';
fieldhash my %report_paths => 'report_paths';
fieldhash my %root         => 'root';
fieldhash my %start_node   => 'start_node';

our $VERSION = '1.04';

# -----------------------------------------------
# Add edges to the tree of nodes.

sub _add_edge2tree
{
	my($self, $index, $root, $items) = @_;

	my($daughter);
	my($name);

	while ($index < $#$items)
	{
		$index++;

		# Skip if not a node.

		next if ($$items[$index]{type} ne 'node_id');

		# Nodes found by find_edges() belong to the 'current' root.

		$name     = $$items[$index]{value};
		$daughter = Tree::DAG_Node -> new;

		$daughter -> name($name);
		$root -> add_daughter($daughter);

		# Is there room in the list for another edge and node?

		last if ($index > $#$items - 2);

		# Skip if the next element is not an edge.

		last if ($$items[$index + 1]{type} ne 'edge_id');

		# Skip if the next-but-1 element is not a node.

		last if ($$items[$index + 2]{type} ne 'node_id');

		# Step to the edge, and the top of the loop steps to the node.

		$root  = $daughter;
		$index += 1;
	}

	# We must return the index to tell the caller where to
	# continue from in its search for daughters of the real root.

	return $index;

} # End of _add_edge2tree.

# -----------------------------------------------
# Build a tree of nodes from the Graphviz file.

sub _build_tree
{
	my($self, $index, $root, $items) = @_;

	my($daughter);
	my($name);

	while ($index < $#$items)
	{
		$index++;

		# Skip if not a node.

		next if ($$items[$index]{type} ne 'node_id');

		$name = $$items[$index]{value};

		# Skip special cases. TODO: How to handle real nodes with these names?

		next if ($name =~ /^(?:edge|graph|node)/);

		# Nodes found by find_nodes() belong to the real root.

		$daughter = Tree::DAG_Node -> new;

		$daughter -> name($name);
		$root -> add_daughter($daughter);

		# Is there room in the list for another edge and node?

		next if ($index > $#$items - 2);

		# Skip if the next element is not an edge.

		next if ($$items[$index + 1]{type} ne 'edge_id');

		# Skip if the next-but-1 element is not a node.

		next if ($$items[$index + 2]{type} ne 'node_id');

		$index = $self -> _add_edge2tree($index + 1, $daughter, $items);
	}

} # End of _build_tree.

# -----------------------------------------------

sub _find_fixed_length_candidates
{
	my($self, $solution, $stack) = @_;
	my($current_node) = $$solution[$#$solution];

	# Add the node's parent, if it's not the root.
	# Then add the node's daughters.

	my(@neighbours);

	$self -> root -> walk_down
	({
		callback   => \&_find_fixed_length_cb,
		neighbours => \@neighbours,
		_depth     => 0,
		name       => $current_node -> name,
	});

	# Elements:
	# 0 .. N: The neighbours.
	# N + 1:  The count of neighbours.

	push @$stack, @neighbours, $#neighbours + 1;

} # End of find_fixed_length_candidates.

# -----------------------------------------------
# Warning: This is a function.

sub _find_fixed_length_cb
{
	my($node, $option) = @_;

	# We only want neighbours of the current node, $$option{name}.
	# So, skip this node if:
	# o It is the root node.
	# o It is not the current node.
	# Return 1 to keep scanning the tree.

	return 1 if ( (! defined $node -> mother) || ($node -> name ne $$option{name}) );

	# Save this node if it's a neighbour of the current node.

	my(@neighbours);

	for my $n ($node -> mother, $node -> daughters)
	{
		# Skip the root node.

		next if (! defined $n -> mother);

		push @{$$option{neighbours} }, $n;
	}

	# Return 1 to keep scanning the tree.

	return 1;

} # End of _find_fixed_length_cb.

# -----------------------------------------------
# Find all paths starting from any copy of the target start_node.

sub _find_fixed_length_path_set
{
	my($self, $start) = @_;
	my($one_solution) = [];
	my($stack)        = [];

	my(@all_solutions);
	my($count, $candidate);

	# Push the first copy of the start node, and its count (1), onto the stack.

	push @$stack, $$start[0], 1;

	# Process these N candidates 1-by-1.
	# The top-of-stack is a candidate count.

	while ($#$stack >= 0)
	{
		while ($$stack[$#$stack] > 0)
		{
			($count, $candidate) = (pop @$stack, pop @$stack);

			push @$stack, $count - 1;
			push @$one_solution, $candidate;

			# Does this candidate suit the solution so far?

			if ($#$one_solution == $self -> path_length)
			{
				# Yes. Save this solution.

				push @all_solutions, [@$one_solution];

				# Discard this candidate, and try another.

				pop @$one_solution;
			}
			else
			{
				# No. The solution is still too short.
				# Push N more candidates onto the stack.

				$self -> _find_fixed_length_candidates($one_solution, $stack);
			}
		}

		# Pop the candidate count (0) off the stack.

		pop @$stack;

		# Remaining candidates, if any, must be contending for the 2nd last slot.
		# So, pop off the node in the last slot, since we've finished
		# processing all candidates for that slot.
		# Then, backtrack to test the next set of candidates for what,
		# after this pop, will be the new last slot.

		pop @$one_solution;
	}

	$self -> fixed_paths([@all_solutions]);

} # End of _find_fixed_length_path_set.

# -----------------------------------------------
# Find all paths starting from any copy of the target start_node.

sub _find_fixed_length_paths
{
	my($self) = @_;

	# Phase 1: Find all copies of the start node.

	my(@stack);

	$self -> root -> walk_down
	({
		callback => \&_find_start_node_cb,
		_depth   => 0,
		name     => $self -> start_node,
		stack    => \@stack,
	});

	# Give up if the given node was not found.
	# Return 0 for success and 1 for failure.

	die 'Error: Start node (', $self -> start_node, ") not found\n" if ($#stack < 0);

	# Phase 2: Process each copy of the start node.

	$self -> _find_fixed_length_path_set(\@stack);

} # End of _find_fixed_length_paths.

# -----------------------------------------------

sub find_fixed_length_paths
{
	my($self)  = @_;
	my($title) = 'Paths of length ' . $self -> path_length . ' starting from node ' . $self -> start_node;

	# Generate the RAM-based version of the graph.

	$self -> run;

	# Assemble the nodes into a tree.

	my(@items) = @{$self -> items};

	$self -> _build_tree(-1, $self -> root, \@items);

	# Process the tree.

	$self -> _find_fixed_length_paths;
	$self -> _winnow_fixed_length_paths;
	$self -> report_fixed_length_paths($title) if ($self -> report_paths);
	$self -> output_fixed_length_paths($title) if ($self -> output_file);

	# Return 0 for success and 1 for failure.

	return 0;

} # End of find_fixed_length_paths.

# -----------------------------------------------
# Warning: This is a function.

sub _find_start_node_cb
{
	my($node, $option) = @_;

	push @{$$option{stack} }, $node if ($node -> name eq $$option{name});

	# Return 1 to keep scanning the tree.

	return 1;

} # End of _find_start_node_cb.

# -----------------------------------------------

sub _init
{
	my($self, $arg)     = @_;
	$$arg{allow_loops}  ||= 0;     # Caller can set.
	$$arg{image_size}   ||= '8,8'; # Caller can set.
	$$arg{path_length}  ||= 0;     # Caller can set.
	$$arg{report_paths} ||= 0;     # Caller can set.
	$$arg{root}         = Tree::DAG_Node -> new;
	$$arg{start_node}   = defined($$arg{start_node}) ? $$arg{start_node} : undef; # Caller can set.
	$self               = $self -> SUPER::_init($arg);

	die "No start node specified\n"  if (! defined $self -> start_node);
	die "Path length must be >= 0\n" if ($self -> path_length < 0);

	# This root node is hereafter always skipped.

	$self -> root -> name('Root');

	return $self;

} # End of _init.

# -----------------------------------------------

sub output_fixed_length_paths
{
	my($self, $title) = @_;
	my(@solutions)    = @{$self -> fixed_paths};
	$title            .= '. Solutions: ' . scalar @solutions;

	# We have to rename all the nodes so they can all be included
	# in a DOT file without dot linking them based on their names.

	my($new_name) = 0;

	my($name);
	my(@set);

	for my $set (@solutions)
	{
		my(@name);
		my(%seen);

		for my $node (@$set)
		{
			$name = $node -> name;

			if (! defined($seen{$name}) )
			{
				$seen{$name} = ++$new_name;
			}

			push @name, {label => $name, name => $seen{$name} };
		}

		push @set, [@name];
	}

	# Now output the paths, using the nodes' original names as labels.

	my($image_size) = $self -> image_size;

	open(OUT, '>', $self -> output_file) || die "Can't open(> ", $self -> output_file, "): $!\n";
	print OUT <<"EOS";
strict digraph
{
	graph [label = \"$title\" rankdir = LR size = \"$image_size\"];

EOS
	for my $set (@set)
	{
		for my $node (@$set)
		{
			print OUT qq|\t"$$node{name}" [label = "$$node{label}"]\n|;
		}
	}

	for my $set (@set)
	{
		print OUT "\t", join(' -> ', map{qq|"$$_{name}"|} @$set), ";\n";
	}

	print OUT "}\n";
	close OUT;

} # End of output_fixed_length_paths.

# -----------------------------------------------

sub report_fixed_length_paths
{
	my($self, $title) = @_;
	my(@solutions)    = @{$self -> fixed_paths};

	print "$title:\n";

	for my $candidate (@solutions)
	{
		print join(' -> ', map{$_ -> name} @$candidate), "\n";
	}

	print 'Solution count: ', scalar @solutions, "\n";

} # End of report_fixed_length_paths.

# -----------------------------------------------

sub _winnow_fixed_length_paths
{
	my($self)  = @_;
	my($loops) = $self -> allow_loops;

	my(@solutions);

	for my $candidate (@{$self -> fixed_paths})
	{
		# Count the number of times each node appears in this candidate.

		my(%seen);

		$seen{$_}++ for map{$_ -> name} @$candidate;

		# Exclude nodes depending on the allow_loops option:
		# o 0 - Do not allow any loops.
		# o 1 - Allow any node to be included once or twice.

		if ($loops == 0)
		{
			@$candidate = grep{$seen{$_ -> name} == 1} @$candidate;
		}
		elsif ($loops == 1)
		{
			@$candidate = grep{$seen{$_ -> name} <= 2} @$candidate;
		}

		push @solutions, [@$candidate] if ($#$candidate == $self -> path_length);
	}

	$self -> fixed_paths([@solutions]);

} # End of _winnow_fixed_length_paths.

# -----------------------------------------------

1;

=pod

=head1 NAME

L<GraphViz2::Marpa::Parser::TreeUtils> - Analyze Graphviz dot files for various purposes

=head1 Synopsis

Perl usage:

Either pass parameters in to new():

	GraphViz2::Marpa::Parser::TreeUtils -> new
		(
			output_file => 'fixed.length.paths.gv',
			path_length => 3,
			start_node  => 5,
		) -> find_fixed_length_paths;

Or call methods to set parameters;

	my($parser) = GraphViz2::Marpa::Parser::TreeUtils -> new;

	$parser -> output_file('fixed.length.paths.gv');
	$parser -> path_length(3);
	$parser -> start_node(5);
	$parser -> find_fixed_length_paths;

Command line usage:

	shell> perl scripts/fixed.length.paths.pl -h
	shell> perl scripts/fixed.length.paths.pl -lex data/90.Petersen.lex \
			-out data/fixed.length.paths.gv \
			-path_length 3 -report_paths 1 -start_node 5

See *.lex TODO, data/fixed.length.paths.gv and html/fixed.length.paths.svg.

The output DOT file was converted into an SVG file with:

	shell> dot -Tsvg data/fixed.length.paths.gv > html/fixed.length.paths.svg

Note: See scripts/fixed.length.paths.sh.

=head1 Description

GraphViz2::Marpa::Parser::TreeUtils parses L<Graphviz|http://www.graphviz.org/> dot files and processes the output in various ways.

Currently, the only feature available is to find all paths of a given length starting from a given node.

Sample output: L<http://savage.net.au/Perl-modules/html/graphviz2.marpa/fixed.length.paths.html>.

This HTML file was hand-rolled to showcase the module. See the synopsis for sample code.

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

=head2 Calling new()

C<new()> is called as C<< my($obj) = GraphViz2::Marpa::Parser::TreeUtils -> new(k1 => v1, k2 => v2, ...) >>.

It returns a new object of type C<GraphViz2::Marpa::Parser::TreeUtils>.

This class is a descendent of L<GraphViz2::Marpa::Parser>, and hence inherits all its keys to new().

Further, these key-value pairs are accepted in the parameter list (see corresponding methods for details
[e.g. L</path_length($integer)>]):

=over 4

=item o output_file => $ADotFileName

Specify the name of a DOT file to write, which will be the set of directed graphs of all paths found.

Default: ''.

If a file name is not provided, a DOT file is not written.

=item o path_length => $integer

Specify the length of all paths to be included in the output.

Here, length means the number of edges between nodes.

Default: 0.

This parameter is mandatory, and must be > 0.

=item o report_paths => $Boolean

Specify whether or not to print a report of the paths found.

Default: 0 (do not print).

=item o start_node => $theNameOfANode

Specify the name of the node where all paths must start from.

Default: ''.

This parameter is mandatory, and the name must be non-empty.

=back

=head1 Methods

This class is a descendent of L<GraphViz2::Marpa::Parser>, and hence inherits all its methods.

Further, these methods are implemented.

=head2 find_fixed_length_paths()

This is the method which does all the work, and hence must be called.

See the L</Synopsis> and scripts/fixed.length.paths.pl.

=head2 fixed_paths()

Returns the arrayref of paths found. Each element is 1 path, and paths are stored as an arrayref of objects of type L<Tree::DAG_Node>.

See the source code of sub report_fixed_length_paths() for sample usage.

=head2 new()

See L</Constructor and Initialization> for details on the parameters accepted by L</new()>.

=head2 output_file($ADotFileName)

Get or set the name of the DOT file to write.

See scripts/fixed.length.paths.sh.

'output_file' is a parameter to L</new()>. See L</Constructor and Initialization> for details.

=head2 output_fixed_length_paths()

This writes the paths found, as long as new(output_file => $name) was specified, or if
output_file($name) was called before find_fixed_length_paths() was called.

=head2 path_length($integer)

Get or set the length of the paths to be searched for.

'path_length' is a parameter to L</new()>. See L</Constructor and Initialization> for details.

=head2 report_fixed_length_paths()

This prints the paths found, as long as new(report_paths => 1) was specified, or if
report_paths(1) was called before find_fixed_length_paths() was called.

=head2 report_paths($Boolean)

Get or set the option which determines whether or not the paths found are printed.

'report_paths' is a parameter to L</new()>. See L</Constructor and Initialization> for details.

=head2 root()

Returns the root of the tree of nodes parsed from the original DOT file. Each element in this tree
is an object of type L<Tree::DAG_Node>.

=head2 start_node()

Get or set the name of the node from where all paths must start.

'start_node' is a parameter to L</new()>. See L</Constructor and Initialization> for details.

=head1 FAQ

=head2 Isn't your code at risk from the 'combinatorial explosion' problem?

Yes. The code does limit the number of possibilies as quickly as possible, but of course there will always be
graphs which can't be processed by this module.

Such graphs are deemed to be pathological.

=head2 How are cycles in the graph handled?

Firstly, paths are forbidden to return to the starting node.

Secondly, apart from the starting node, any other node may appear at most twice in the path. This is a way of
saying you can exit from, and return to, any non-starting node only once.

So, when searching for paths of length 3 starting from node 5, we have:

	5 -> 4 -> 3 -> 5 is excluded from the result
	5 -> 4 -> 3 -> 4 is included in the result

=head2 Are all paths found unique?

Yes. The code checks for duplicates.

=head1 Reference Book

Combinatorial Algorithms for Computers and Calculators, A Nijenhuis and H Wilf, p 240.

This books explains very neatly the backtracking parser I used to process the combinations of nodes found
at each point along each path. Source code in the book is in Fortran.

The book is now downloadable as a PDF from L<http://www.math.upenn.edu/~wilf/website/CombAlgDownld.html>.

=head1 Version Numbers

Version numbers < 1.00 represent development versions. From 1.00 up, they are production versions.

=head1 Machine-Readable Change Log

The file CHANGES was converted into Changelog.ini by L<Module::Metadata::Changes>.

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
