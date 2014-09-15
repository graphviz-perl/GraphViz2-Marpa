use strict;
use warnings;

use Config;

use File::Spec;
use File::Temp;

use Module::Build;

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

my $class = Module::Build->subclass(code => <<'EOF');

sub ACTION_authortest
{
	my($self) = @_;

	$self->depends_on('build');
	$self->depends_on('manifest');
	$self->depends_on('distmeta');

	$self->test_files( qw< t xt/author > );
	$self->recursive_test_files(1);

	$self->depends_on('test');

	return;
}
EOF

$class -> new
(
	module_name    => 'GraphViz2::Marpa',
	license        => 'artistic_2',
	dist_abstract  => 'A Marpa-based parser for Graphviz dot files',
	dist_author    => 'Ron Savage <ron@savage.net.au>',
	build_requires =>
	{
		'Test::More'    => 1.001006,
# 	 	'Test::Pod'     => 1.45, # Make it optional. See xt/author/pod.t.
	},
	configure_requires =>
	{
		'Module::Build' => 0.4210,
	},
	requires =>
	{
		'Algorithm::Diff'             => 1.1902,
		'Capture::Tiny'               => 0.25,
		'Config'                      => undef,
		'Config::Tiny'                => 2.14,
		'Date::Format'                => 2.24,
		'Date::Simple'                => 3.03,
		'File::HomeDir'               => 1.00,
		'File::Spec'                  => 3.40,
		'File::Slurp'                 => 9999.19,
		'File::Temp'                  => 0.2301,
		'Getopt::Long'                => 2.38,
		'HTML::Entities::Interpolate' => 1.04,
		'Log::Handler'                => 0.71,
		'Marpa::R2'                   => 2.022000,
		'Moo'                         => 1.006000,
		'open'                        => 0, # For writing utf8 node etc names.
		'perl'                        => 5.014002,
		'Pod::Usage'                  => 1.36,
		'strict'                      => 0,
		'Text::Xslate'                => 1.4001,
		'Tree::DAG_Node'              => 1.07,
		'Try::Tiny'                   => 0.09,
		'Types::Standard'             => 1.000004,
		'utf8'                        => 0,
		'warnings'                    => 0,
	},
	resources =>
	{
		bugtracker => 'https://rt.cpan.org/Public/Dist/Display.html?Name=GraphViz2-Marpa',
		license    => 'http://opensource.org/licenses/Artistic-2.0',
	},
) -> create_build_script();
