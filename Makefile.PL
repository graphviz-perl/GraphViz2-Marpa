use Config;

use ExtUtils::MakeMaker;

use inc::Module::Install;

use File::Spec;
use File::Temp;

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

name 'GraphViz2-Marpa';

license 'artistic_2';

perl_version '5.014000';

all_from 'lib/GraphViz2/Marpa.pm';

install_share;

requires 'Algorithm::Diff'             => 1.1902;
requires 'Capture::Tiny'               => 0.25;
requires 'Config'                      => undef;
requires 'Config::Tiny'                => 2.14;
requires 'Date::Format'                => 2.24;
requires 'Date::Simple'                => 3.03;
requires 'File::HomeDir'               => 1.00;
requires 'File::Spec'                  => 3.40;
requires 'File::Slurp'                 => 9999.19;
requires 'File::Temp'                  => 0.2301;
requires 'Getopt::Long'                => 2.38;
requires 'HTML::Entities::Interpolate' => 1.04;
requires 'Log::Handler'                => 0.71;
requires 'Marpa::R2'                   => 2.022000;
requires 'Moo'                         => 1.006000,
requires 'Pod::Usage'                  => 1.36;
requires 'strict'                      => 0;
requires 'Text::Xslate'                => 1.4001;
requires 'Tree::DAG_Node'              => 1.07;
requires 'Try::Tiny'                   => 0.09;
requires 'Types::Standard'             => 1.000004;
requires 'warnings'                    => 0;

test_requires 'Test::More'     => 0.98;
#test_requires 'Test::Version' => 1.002003;
#test_requires 'Test::Pod'     => 1.45; # Make it optional. See t/pod.t

WriteAll;
