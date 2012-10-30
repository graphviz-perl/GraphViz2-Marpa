#!/usr/bin/env perl

use feature qw/say unicode_strings/;
use open qw(:std :utf8);
use strict;
use warnings;
use warnings qw(FATAL utf8);

use Tree::DAG_Node;

# ------------

my($count) = 0;
my($tree)  = Tree::DAG_Node -> new;
my(%child) =
(
	I => 'J',
	H => 'J',
	J => 'L',
	L => 'M',
	D => 'F',
	E => 'F',
	F => 'G',
	B => 'C',
);

$tree -> name('root');

my($child);
my($kid_1, $kid_2);
my($node);

for my $name (qw/I H J J L D E F B/)
{
	$count++;

	$node  = Tree::DAG_Node -> new({name => $name});
	$child = Tree::DAG_Node -> new({name => $child{$name} });

	$child -> name('K') if ($count == 3);

	if ($child{$name} eq 'M')
	{
		$kid_1 = Tree::DAG_Node -> new({name => 'N'});
		$kid_2 = Tree::DAG_Node -> new({name => 'O'});

		$kid_1 -> add_daughter($kid_2);
		$child -> add_daughter($kid_1);
	}

	$node -> add_daughter($child);
	$tree -> add_daughter($node);
}

print join("\n", @{$tree -> draw_ascii_tree}), "\n";

$tree -> walk_down
({
	callback =>
	sub
	{
		my($node) = @_;

		print 'Name: ', $node -> name, '. Depth: ', scalar $node -> ancestors, "\n";

		return 1;
	}
});

__END__

edges. Edge attrs: {}
   |---I. Edge attrs: {arrowhead => "none", color => "grey", label => "", samehead => "1"}
   |   |---J. Edge attrs: {}
   |---H. Edge attrs: {arrowhead => "none", color => "grey", label => "", samehead => "1"}
   |   |---J. Edge attrs: {}
   |---J. Edge attrs: {color => "grey", label => "", sametail => "1"}
   |   |---L. Edge attrs: {}
   |---J. Edge attrs: {color => "grey", label => "", sametail => "1"}
   |   |---K. Edge attrs: {}
   |---L. Edge attrs: {arrowhead => "odot", color => "grey"}
   |   |---M. Edge attrs: {arrowhead => "odot", color => "grey"}
   |       |---N. Edge attrs: {arrowhead => "odot", color => "grey"}
   |           |---O. Edge attrs: {}
   |---D. Edge attrs: {arrowhead => "none", color => "grey", label => "", samehead => "1"}
   |   |---F. Edge attrs: {}
   |---E. Edge attrs: {arrowhead => "none", color => "grey", label => "", samehead => "1"}
   |   |---F. Edge attrs: {}
   |---F. Edge attrs: {color => "grey", label => "", sametail => "1"}
   |   |---G. Edge attrs: {}
   |---B. Edge attrs: {arrowhead => "none", color => "grey", label => "", samehead => "1"}
   |   |---C. Edge attrs: {}
