#!/usr/bin/env perl

use strict;
use warnings;

use File::Spec;

use GraphViz2;
use GraphViz2::Parse::Marpa;

use Log::Handler;

use Perl6::Slurp;

# ------------------------------------------------

my($logger) = Log::Handler -> new;

$logger -> add
	(
	 screen =>
	 {
		 maxlevel       => 'debug',
		 message_layout => '%m',
		 minlevel       => 'error',
	 }
	);

my($graph)  = GraphViz2 -> new
	(
	 edge   => {color => 'grey'},
	 global => {directed => 1, record_orientation => 'horizontal'},
	 graph  => {rankdir => 'TB'},
	 logger => $logger,
	);
my($g)      = GraphViz2::Parse::Marpa -> new(graph => $graph);
my $grammar = slurp(File::Spec -> catfile('data', 'Marpa.Grammar.dat') );

$g -> create(grammar => $grammar);

my($format)      = shift || 'svg';
my($output_file) = shift || File::Spec -> catfile('html', "Marpa.Grammar.$format");

$graph -> run(format => $format, output_file => $output_file);
