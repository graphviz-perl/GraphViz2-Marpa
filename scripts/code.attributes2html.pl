#!/usr/bin/env perl

use strict;
use warnings;

use File::Spec;

use GraphViz2::Marpa;
use GraphViz2::Marpa::Utils;

use IO::File;

use Text::CSV_XS;
use Text::Xslate 'mark_raw';

# -----------------------------------------------

my(@heading)         = qw/Name Usage lex.pl Lexer.pm DFA.pm parse.pl Parser.pm rend.pl Renderer g2m.pl Marpa Notes/;
my($data_dir_name)   = 'data';
my($code_file_name)  = File::Spec -> catfile($data_dir_name, 'code.attributes.csv');
my($code_attributes) = GraphViz2::Marpa::Utils -> new -> read_csv_file($code_file_name);
my($templater)       = Text::Xslate -> new
(
 input_layer => '',
 path        => File::Spec -> catdir('htdocs', 'assets', 'templates', 'graphviz2', 'marpa'),
);

my($column, @column);
my(@row);

for $column (@heading)
{
	push @column, {td => $column};
}

push @row, [@column];

for my $item (@$code_attributes)
{
	@column = ();

	for $column (@heading)
	{
		push @column, {td => mark_raw($$item{$column} || '.')};
	}

	push @row, [@column];
}

@column = ();

for $column (@heading)
{
	push @column, {td => $column};
}

push @row, [@column];

my($table) =
{
	border  => 0,
	row     => [@row],
	size    => $#row + 1,
	summary => 'STT',
};

print $templater -> render
(
 'code.attributes.tx',
 {
	 title   => 'Code and Command Line Attributes for GraphViz2::Marpa',
	 table   => $table,
	 version => $GraphViz2::Marpa::VERSION,
 },
);
