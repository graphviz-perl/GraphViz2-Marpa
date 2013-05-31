use Config;

use File::Spec;
use File::Temp;

use Module::Build;

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

	print "V $Config{version}. \n";

} # End of is_dot_installed.

# -----------------------------------------------

sub is_perl_recent
{
	# We want Perl V 5.12 or later, because we 'use feature qw/say unicode_strings/'.

	my(@version) = split(/\./, $Config{version});

	die "Please install Perl V 5.12 or later\n" if ($version[1] < 12);

} # End of is_perl_recent.

# -----------------------------------------------

is_dot_installed();
is_perl_recent();

Module::Build -> new
(
	module_name    => 'GraphViz2::Marpa',
	license        => 'artistic_2',
	dist_abstract  => 'A Perl lexer and parser for Graphviz dot files',
	dist_author    => 'Ron Savage <ron@savage.net.au>',
	build_requires =>
	{
		Test::More=> 0.98,
#		Test::Pod  => 1.45, # Make it optional. See t/pod.t
	},
	configure_requires =>
	{
		Module::Build => 0.3800,
	},
	requires =>
	{
		Algorithm::Diff             => 1.1902,
		Config::Tiny                => 2.14,
		Data::Section::Simple       => 0.02,
		Date::Format                => 2.24,
		Date::Simple                => 3.03,
		feature                     => 0,
		File::Copy                  => 2.21,
		File::HomeDir               => 0.99,
		File::Slurp                 => 9999.19,
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
		Perl6::Slurp                => 0.03,
		Pod::Usage                  => 1.36,
		Set::Array                  => 0.23,
		Set::FA::Element            => 1.08,
		strict                      => 0,
		Text::CSV                   => 1.21,
		Text::CSV::Slurp            => 1,
		Text::Xslate                => 1.4001,
		Tree::DAG_Node              => 1.07,
		Try::Tiny                   => 0.09,
		warnings                    => 0,
	},
) -> create_build_script();
