use ExtUtils::MakeMaker;

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
PL_FILES  => {},
PREREQ_PM =>
{
	Algorithm::Diff             => 1.1902,
	Data::Section::Simple       => 0.02,
	Date::Format                => 2.24,
	feature                     => 1.00,
	File::Slurp                 => 9999.19,
	File::Spec                  => 3.33,
	File::Temp                  => 0.22,
	Getopt::Long                => 2.38,
	GraphViz2                   => 1.09,
	GraphViz2::Parse::Marpa     => 1.12,
	 Hash::FieldHash             => 0.10,
	HTML::Entities::Interpolate => 1.04,
	IO::File                    => 1.14,
	List::Compare               => 0.37,
	Log::Handler                => 0.71,
	Marpa::R2                   => 2.022000,
	perl                        => '5.10.0', # For given/when.
	Perl6::Slurp                => 0.03,
	Pod::Usage                  => 1.36,
	Set::Array                  => 0.23,
	Set::FA::Element            => 1.08,
	strict                      => 1.03,
	Test::More                  => 0.98,
	Test::Pod                   => 1.44,
	Tree                        => 1.02,
	Text::CSV_XS                => 0.80,
	Text::Xslate                => 1.4001,
	Try::Tiny                   => 0.09,
	warnings                    => 1.03,
},
VERSION_FROM => 'lib/GraphViz2/Marpa.pm',
);
