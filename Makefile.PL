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

sub is_perl_recent
{
	# We want Perl V 5.12 or later, because we 'use feature qw/say unicode_strings/'.

	my(@version) = split(/\./, $Config{version});

	die "Please install Perl V 5.12 or later\n" if ($version[1] < 12);

} # End of is_perl_recent.

# -----------------------------------------------

is_dot_installed();
is_perl_recent();

name 'GraphViz2-Marpa';

license 'artistic_2';

perl_version '5.014000';

all_from 'lib/GraphViz2/Marpa.pm';

install_share;

requires 'Algorithm::Diff'             => 1.1902;
requires 'Config'                      => undef;
requires 'Config::Tiny'                => 2.14;
requires 'Data::Section::Simple'       => 0.02;
requires 'Date::Format'                => 2.24;
requires 'Date::Simple'                => 3.03;
requires 'feature'                     => 0;
requires 'File::Spec'                  => 3.40;
requires 'File::ShareDir'              => 1.03;
requires 'File::Slurp'                 => 9999.19;
requires 'File::Temp'                  => 0.2301;
requires 'Getopt::Long'                => 2.38;
requires 'Hash::FieldHash'             => 0.10;
requires 'HTML::Entities::Interpolate' => 1.04;
requires 'List::Compare'               => 0.37;
requires 'Log::Handler'                => 0.71;
requires 'Marpa::R2'                   => 2.022000;
requires 'Module::Path'                => 0.08;
requires 'open'                        => 0;
requires 'Path::Tiny'                  => 0.022;
requires 'Perl6::Slurp'                => 0.03;
requires 'Pod::Usage'                  => 1.36;
requires 'Set::Array'                  => 0.23;
requires 'Set::FA::Element'            => 1.08;
requires 'strict'                      => 0;
requires 'Text::CSV'                   => 1.21;
requires 'Text::CSV::Slurp'            => 1;
requires 'Text::Xslate'                => 1.4001;
requires 'Tree::DAG_Node'              => 1.07;
requires 'Try::Tiny'                   => 0.09;
requires 'warnings'                    => 0;

test_requires 'Test::More' => 0.98;
#test_requires 'Test::Pod'  => 1.45; # Make it optional. See t/pod.t

WriteAll;
