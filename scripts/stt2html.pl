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

my(@heading)       = qw/Start Accept State Event Next Entry Exit Regexp Interpretation/;
my($data_dir_name) = 'data';
my($stt_file_name) = File::Spec -> catfile($data_dir_name, 'default.stt.csv');
my($stt)           = GraphViz2::Marpa::Utils -> new -> read_csv_file($stt_file_name);
my($templater)     = Text::Xslate -> new
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

for my $item (@$stt)
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
 'stt.tx',
 {
	 title   => 'State Transition Table for GraphViz2::Marpa::Lexer',
	 table   => $table,
	 version => $GraphViz2::Marpa::VERSION,
 },
);
