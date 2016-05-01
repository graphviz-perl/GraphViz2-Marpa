use strict;
use warnings;

use Config;

use ExtUtils::MakeMaker;

use File::Spec;
use File::Temp;

require 5.014002; # For the utf8 stuff.

# -----------------------------------------------

sub is_dot_installed
{
	# 1: Create a temp file containing DOT commands.
	# The EXLOCK option is for BSD-based systems.
	# newdir() croaks() if it fails, which is what we want.
	# Hence there is no need to use Try::Tiny.

	my($temp_dir) = File::Temp -> newdir('temp.XXXX', CLEANUP => 1, EXLOCK => 0, TMPDIR => 1);
	my($gv_file)  = File::Spec -> catfile($temp_dir, 'test.gv');

	# We use the deprecated 2-argument form of open in case
	# this code is run on an ancient version of Perl.

	open(OUT, ">$gv_file")                   || die "Can't create temp file: $!\n";
	print OUT "digraph graph_14 {node_14}\n" || die "Can't write to temp file: $!\n";
	close OUT                                || die "Can't close temp file: $!\n";

	# 2: Run dot to create an SVG file.

	my $stdout = `dot -Tsvg $gv_file`;

	# 3: If that failed, we die.

	die "Please install Graphviz from http://www.graphviz.org/\n" if ($stdout !~ m|</svg>|);

} # End of is_dot_installed.

# -----------------------------------------------

is_dot_installed();

# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.

my(%params) =
(
	($] ge '5.005') ?
	(
		AUTHOR   => 'Ron Savage (ron@savage.net.au)',
		ABSTRACT => 'A Marpa-based parser for Graphviz dot files',
	) : (),
	clean =>
	{
		FILES => 'blib/* Makefile MANIFEST GraphViz2-Marpa-*'
	},
	dist =>
	{
		COMPRESS => 'gzip',
		SUFFIX   => 'gz'
	},
	DISTNAME  => 'GraphViz2-Marpa',
	NAME      => 'GraphViz2::Marpa',
	PL_FILES  => {},
	PREREQ_PM =>
	{
		'Algorithm::Diff' => 1.1902,
		'Capture::Tiny' => 0.25,
		'Config' => undef,
		'Config::Tiny' => 2.14,
		'Date::Format' => 2.24,
		'Date::Simple' => 3.03,
		'File::Basename' => 2.84,
		'File::Copy' => 2.26,
		'File::HomeDir' => 1.00,
		'File::Spec' => 3.40,
		'File::Temp' => 0.2301,
		'Getopt::Long' => 2.38,
		'HTML::Entities::Interpolate' => 1.04,
		'Log::Handler' => 0.71,
		'Marpa::R2' => 2.096000,
		'Moo' => 1.006000,
		'Path::Iterator::Rule' => 1.008,
		'Path::Tiny' => 0.061,
		'perl' => 5.010001,
		'Pod::Usage' => 1.36,
		'strict' => 0,
		'Text::Xslate' => 1.4001,
		'Tree::DAG_Node' => 1.07,
		'Try::Tiny' => 0.09,
		'Types::Standard' => 1.000004,
		'warnings' => 0,
	},
	TEST_REQUIRES =>
	{
		'Test::More' => 1.001014,
		'Test::Pod' => 1.48,
	},
	VERSION_FROM => 'lib/GraphViz2/Marpa.pm',
);

if ( ($ExtUtils::MakeMaker::VERSION =~ /^\d\.\d\d$/) && ($ExtUtils::MakeMaker::VERSION > 6.30) )
{
	$params{LICENSE} = 'perl';
}

if ($ExtUtils::MakeMaker::VERSION ge '6.46')
{
	$params{META_MERGE} =
	{
		'meta-spec' =>
		{
			version => 2,
		},
		resources   =>
		{
			bugtracker =>
			{
				web => 'https://rt.cpan.org/Public/Dist/Display.html?Name=GraphViz2-Marpa',
			},
			license		=> 'http://dev.perl.org/licenses/',
			repository	=>
			{
				type	=> 'git',
				url		=> 'https://github.com/ronsavage/GraphViz2-Marpa.git',
				web		=> 'https://github.com/ronsavage/GraphViz2-Marpa',
			},
		},
	};
}

WriteMakefile(%params);
