package GraphViz2::Marpa::Demo;

use feature qw/say unicode_strings/;
use open qw(:std :utf8);
use strict;
use warnings;
use warnings qw(FATAL utf8);

use Config;

use Date::Format; # For time2str().
use Date::Simple;

use File::Spec;
use File::Slurp; # For read_dir().

use GraphViz2::Marpa::Config;
use GraphViz2::Marpa::Utils;

use Hash::FieldHash ':all';

use HTML::Entities::Interpolate;

use Perl6::Slurp;

use Text::Xslate 'mark_raw';

fieldhash my %config => 'config';

our $VERSION = '1.06';

# -----------------------------------------------

sub generate_demo
{
	my($self)          = @_;
	my($data_dir_name) = 'data';
	my($html_dir_name) = 'html';
	my($format)        = 'svg';
	my(@dot_file)      = GraphViz2::Marpa::Utils -> new -> get_files($data_dir_name, 'gv');

	my(@content);
	my($dot_file);
	my($image_file, %image_file);
	my($lex_file);

	for my $file_name (@dot_file)
	{
		$dot_file               = File::Spec -> catfile($data_dir_name, "$file_name.gv");
		$lex_file               = File::Spec -> catfile($data_dir_name, "$file_name.lex");
		$image_file             = File::Spec -> catfile($html_dir_name, "$file_name.$format");
		@content                = map{$Entitize{$_} } slurp($dot_file);
		$image_file{$file_name} =
		{
			image_file   => -e $image_file ? $image_file : '',
			image_size   => -e $image_file ? -s $image_file : 0,
			input        => $dot_file,
			input_bytes  => 'byte' . (-s $dot_file == 1 ? '' : 's'),
			input_size   => -s $dot_file,
			lex_result   => -e $lex_file ? 'OK' : 'Error',
			object_file  => "./$file_name.$format",
			output       => -e $image_file && -s $image_file ? $image_file : '',
			output_bytes => 'byte' . (-e $image_file && -s $image_file == 1 ? '' : 's'),
			output_size  => -s $image_file,
			raw          => join('<br />', @content),
		};
	}

	my($config) = $self -> config;
	my($templater) = Text::Xslate -> new
	(
		input_layer => '',
		path        => $$config{template_path},
	);
	my($count) = 0;
	my($index) = $templater -> render
	(
	'graphviz2.marpa.index.tx',
	{
		data =>
		[
			map
			{
				{
					count        => ++$count,
					image_file   => mark_raw($image_file{$_}{image_file}),
					image_size   => $image_file{$_}{image_size},
					input        => mark_raw($image_file{$_}{input}),
					input_bytes  => $image_file{$_}{input_bytes},
					input_size   => mark_raw($image_file{$_}{input_size}),
					lex_result   => $image_file{$_}{lex_result},
					object_file  => $image_file{$_}{object_file},
					output       => mark_raw($image_file{$_}{output}),
					output_bytes => $image_file{$_}{output_bytes},
					output_size  => $image_file{$_}{output_size},
					raw          => mark_raw($image_file{$_}{raw}),
				}
			} @dot_file
		],
		default_css     => "$$config{css_url}/default.css",
		environment     => $self -> generate_demo_environment,
		fancy_table_css => "$$config{css_url}/fancy.table.css",
		version         => $GraphViz2::Marpa::Utils::VERSION,
 }
);
	my($file_name) = File::Spec -> catfile($html_dir_name, 'index.html');

	open(OUT, '>', $file_name);
	print OUT $index;
	close OUT;

	print "Wrote: $file_name\n";

} # End of generate_demo.

# ------------------------------------------------

sub generate_demo_environment
{
	my($self) = @_;

	my(@environment);

	# mark_raw() is needed because of the HTML tag <a>.

	push @environment,
	{left => 'Author', right => mark_raw(qq|<a href="http://savage.net.au/">Ron Savage</a>|)},
	{left => 'Date',   right => Date::Simple -> today},
	{left => 'OS',     right => 'Debian V 6'},
	{left => 'Perl',   right => $Config{version} };

	return \@environment;
}
 # End of generate_demo_environment.

# -----------------------------------------------

sub _init
{
	my($self, $arg) = @_;
	$$arg{config}   = GraphViz2::Marpa::Config -> new -> config;
	$self           = from_hash($self, $arg);

	return $self;

} # End of _init.

# --------------------------------------------------

sub new
{
	my($class, %arg) = @_;
	my($self)        = bless {}, $class;
	$self            = $self -> _init(\%arg);

	return $self;

}	# End of new.

# -----------------------------------------------

1;

