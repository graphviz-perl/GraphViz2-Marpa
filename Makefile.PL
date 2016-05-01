#!/usr/bin/env perl

use strict;
use warnings;

use Config;

use ExtUtils::MakeMaker;

# -----------------------------------------------

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
		'Algorithm::Diff'				=> 1.1902,
		'Capture::Tiny'					=> 0.25,
		'Config'						=> undef,
		'Config::Tiny'					=> 2.14,
		'Date::Format'					=> 2.24,
		'Date::Simple'					=> 3.03,
		'File::Basename'				=> 2.84,
		'File::Copy'					=> 2.14,
		'File::HomeDir'					=> 1.00,
		'File::Spec'					=> 3.40,
		'File::Temp'					=> 0.2301,
		'File::Which'					=> 1.09,
		'Getopt::Long'					=> 2.38,
		'HTML::Entities::Interpolate'	=> 1.04,
		'Log::Handler'					=> 0.71,
		'Marpa::R2'						=> 2.096000,
		'Moo'							=> 1.006000,
		'Path::Iterator::Rule'			=> 1.008,
		'Path::Tiny'					=> 0.061,
		'perl'							=> 5.010001,
		'Pod::Usage'					=> 1.36,
		'strict'						=> 0,
		'Text::Xslate'					=> 1.4001,
		'Tree::DAG_Node'				=> 1.07,
		'Try::Tiny'						=> 0.09,
		'Types::Standard'				=> 1.000004,
		'warnings'						=> 0,
	},
	TEST_REQUIRES =>
	{
		'Test::Pod'				=> 1.48,
		'Test2::Suite'			=> 0.000029,
		'Test2::Tools::Basic'	=> 0.000029, # Keep one of my build tools happy.
		'Test2::Tools::Compare'	=> 0.000029, # Keep one of my build tools happy.
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
		resources =>
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
