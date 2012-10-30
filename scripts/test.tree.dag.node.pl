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
my($name, $node);

for $name (qw/I H J J L D E F B/)
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
print '-' x 50, "\n";

my(@ancestors) = map{$_ -> name} $tree -> daughters;

my(%ancestors);

@ancestors{@ancestors} = (1) x @ancestors;

my(@stack);

$tree -> walk_down
({
	ancestors => \%ancestors,
	callback  =>
	sub
	{
		my($node, $options) = @_;

		if ($$options{_depth} > 1)
		{
			$name = $node -> name;

			if (defined $$options{ancestors}{$name})
			{
				push @{$$options{stack} }, $node;
			}
		}

		return 1;
	},
	_depth => 0,
	stack  => \@stack,
});

my(@kids);

for $node (@stack)
{
	$name = $node -> name;
	@kids = grep{$_ -> name eq $name} $tree -> daughters;

	$node -> replace_with(map{$_ -> copy_at_and_under} @kids);
}

print join("\n", @{$tree -> draw_ascii_tree}), "\n";
print '-' x 50, "\n";

__END__

for $node (@stack)
{
	$name = $node -> name;
	@kids = grep{$_ -> name eq $name} $tree -> daughters;

	$tree -> remove_daughter($_) for @kids;
}

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
