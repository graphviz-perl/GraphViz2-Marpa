use ExtUtils::MakeMaker;
	LICENSE   => 'artistic_2',

if (open my $fh, '|dot', )
{
	close $fh;
}
else
{
	die "Please install Graphviz from http://www.graphviz.org/.\n";
}

# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.

WriteMakefile
(
	($] ge '5.005') ?
	(
		AUTHOR   => 'Ron Savage (ron@savage.net.au)',
		ABSTRACT => 'A Perl lexer and parser for Graphviz dot files',
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
	LICENSE   => 'artistic_2',
	PL_FILES  => {},
	PREREQ_PM =>
	{
		Algorithm::Diff             => 1.1902,
		Config                      => undef,
		Config::Tiny                => 2.14,
		Data::Section::Simple       => 0.02,
		Date::Format                => 2.24,
		Date::Simple                => 3.03,
		feature                     => 0,
		File::Copy                  => 2.21,
		File::HomeDir               => 0.99,
		File::Slurp                 => 9999.19,
		File::Spec                  => 3.33,
		File::Temp                  => 0.22,
		Getopt::Long                => 2.38,
		GraphViz2                   => 1.09,
		GraphViz2::Parse::Marpa     => 1.12,
		Hash::FieldHash             => 0.10,
		HTML::Entities::Interpolate => 1.04,
		List::Compare               => 0.37,
		Log::Handler                => 0.71,
		Marpa::R2                   => 2.022000,
		Module::Path                => 0.08,
		Path::Class                 => 0.24,
		perl                        => '5.10.0', # For given/when.
		Perl6::Slurp                => 0.03,
		Pod::Usage                  => 1.36,
		Set::Array                  => 0.23,
		Set::FA::Element            => 1.08,
		strict                      => 0,
		Test::More                => 0.98,
#		Test::Pod                   => 1.45, # Make it optional. See t/pod.t
		Text::CSV                   => 1.21,
		Text::CSV::Slurp            => 1,
		Text::Xslate                => 1.4001,
		Tree::DAG_Node              => 1.07,
		Try::Tiny                   => 0.09,
		warnings                    => 0,
	},
	VERSION_FROM => 'lib/GraphViz2/Marpa.pm',
);
