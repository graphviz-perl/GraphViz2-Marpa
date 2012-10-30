#!/usr/bin/env perl

use feature qw/say unicode_strings/;
use open qw(:std :utf8);
use strict;
use warnings;
use warnings qw(FATAL utf8);

use Tree::DAG_Node;

# ----------------------------------------------

sub find_links
{
	my($tree)      = @_;
} # End of find_links.

# ----------------------------------------------

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

my($finished) = 0;

while (! $finished)
{
	$finished = find_links($tree) == 0;
}

my($attributes);
my(%replaced);

$tree -> walk_down
({
	callback  =>
	sub
	{
		my($node, $options)        = @_;
		$name                      = $node -> name;
		$attributes                = $node -> attributes;
		$$options{replaced}{$name} = 1 if ($$attributes{replaced});

		return 1;
	},
	_depth   => 0,
	replaced => \%replaced,
});

for $name (keys %replaced)
{
	$tree -> remove_daughter($_) for grep{$_ -> name eq $name} $tree -> daughters;
}

print join("\n", @{$tree -> draw_ascii_tree}), "\n";
print '-' x 50, "\n";
