package GraphViz2::Marpa::Utils;

use feature qw/say unicode_strings/;
use open qw(:std :utf8);
use strict;
use warnings;
use warnings qw(FATAL utf8);

use Config;

use Date::Format; # For time2str().
use Date::Simple;

use File::Spec;
use File::Slurp; # For read_dir() and read_file().

# Apart from GraphViz2::Marpa::Config, the next 4 modules are 'use'd
# in order to get them into @INC, where Module::Path finds them.
# This means for the search for mutators to work, they need to be
# both up-to-date and installed.

use GraphViz2::Marpa;
use GraphViz2::Marpa::Config;

use Hash::FieldHash ':all';

use HTML::Entities::Interpolate;

use Module::Path 'module_path';

use Text::CSV;
use Text::CSV::Slurp;
use Text::Xslate 'mark_raw';

fieldhash my %config => 'config';

our $VERSION = '2.00';

# -----------------------------------------------

sub generate_demo_index
{
	my($self)          = @_;
	my($data_dir_name) = 'data';
	my($html_dir_name) = 'html';
	my($format)        = 'svg';
	my(@dot_file)      = $self -> get_files($data_dir_name, 'gv');

	my(@content);
	my($dot_file);
	my($image_file, %image_file);
	my($lex_file);

	for my $file_name (@dot_file)
	{
		$dot_file               = File::Spec -> catfile($data_dir_name, "$file_name.gv");
		$lex_file               = File::Spec -> catfile($data_dir_name, "$file_name.lex");
		$image_file             = File::Spec -> catfile($html_dir_name, "$file_name.$format");
		@content                = map{$Entitize{$_} } read_file($dot_file);
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

	my($config)    = $self -> config;
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
		version         => $VERSION,
	}
	);
	my($file_name) = File::Spec -> catfile($html_dir_name, 'index.html');

	open(OUT, '>', $file_name);
	print OUT $index;
	close OUT;

	print "Wrote: $file_name\n";

	# Return 0 for success and 1 for failure.

	return 0;

} # End of generate_demo_index.

# ------------------------------------------------

sub generate_demo_environment
{
	my($self) = @_;

	my(@environment);

	# mark_raw() is needed because of the HTML tag <a>.

	push @environment,
	{left => 'Author', right => mark_raw(qq|<a href="http://savage.net.au/">Ron Savage</a>|)},
	{left => 'Date',   right => Date::Simple -> today},
	{left => 'OS',     right => 'Debian V 7'},
	{left => 'Perl',   right => $Config{version} };

	return \@environment;

} # End of generate_demo_environment.

# ------------------------------------------------

sub get_files
{
	my($self, $dir_name, $type) = @_;

	return (sort map{s/\.$type//; $_} grep{/\.$type$/} read_dir $dir_name);

} # End of get_files.

# -----------------------------------------------

sub _init
{
	my($self, $arg) = @_;
	$$arg{config}   = GraphViz2::Marpa::Config -> new -> config;
	$self           = from_hash($self, $arg);

	return $self;

} # End of _init.

# --------------------------------------------------

sub justify
{
	my($self, $s) = @_;
	my($width)    = 20;

	return $s . ' ' x ($width - length $s);

} # End of justify.

# -----------------------------------------------

sub new
{
	my($class, %arg) = @_;
	my($self)        = bless {}, $class;
	$self            = $self -> _init(\%arg);

	return $self;

}	# End of new.

# -----------------------------------------------

1;

=pod

=head1 NAME

L<GraphViz2::Marpa::Utils> - A Perl lexer and parser for Graphviz dot files

=head1 Synopsis

See scripts/generate.index.pl, and scripts/dot2lex.pl etc.

=head1 Description

Some utils to simplify reading CSV files, and testing.

=head1 Distributions

This module is available as a Unix-style distro (*.tgz).

See L<http://savage.net.au/Perl-modules/html/installing-a-module.html>
for help on unpacking and installing distros.

=head1 Installation

Install L<GraphViz2::Marpa> as you would for any C<Perl> module:

Run:

	cpanm GraphViz2::Marpa

or run:

	sudo cpan GraphViz2::Marpa

or unpack the distro, and then either:

	perl Build.PL
	./Build
	./Build test
	sudo ./Build install

or:

	perl Makefile.PL
	make (or dmake or nmake)
	make test
	make install

=head1 Constructor and Initialization

=head2 Calling new()

C<new()> is called as C<< my($obj) = GraphViz2::Marpa::Utils -> new(k1 => v1, k2 => v2, ...) >>.

It returns a new object of type C<GraphViz2::Marpa::Utils>.

Key-value pairs accepted in the parameter list:

=over 4

=item o (none)

=back

=head1 Methods

=head2 generate_demo_index()

Generates html/index.html.

Does not run any programs to generate other files, e.g. html/*.svg. See scripts/generate.demo.sh for that.

=head2 generate_demo_environment()

Called by generate_demo_index().

Generates a table to be inserted into html/index.html.

See scripts/generate.demo.pl.

=head2 get_files($dir_name, $type)

Returns a sorted list of files of type (extension) $type from directory $dir_name.

=head2 justify($string)

Right justify the $string in a field of 20 spaces.

=head2 new()

See L</Constructor and Initialization> for details on the parameters accepted by L</new()>.

=head1 Version Numbers

Version numbers < 1.00 represent development versions. From 1.00 up, they are production versions.

=head1 Machine-Readable Change Log

The file CHANGES was converted into Changelog.ini by L<Module::Metadata::Changes>.

=head1 Support

Email the author, or log a bug on RT:

L<https://rt.cpan.org/Public/Dist/Display.html?Name=GraphViz2::Marpa>.

=head1 Author

L<GraphViz2::Marpa> was written by Ron Savage I<E<lt>ron@savage.net.auE<gt>> in 2012.

Home page: L<http://savage.net.au/index.html>.

=head1 Copyright

Australian copyright (c) 2012, Ron Savage.

	All Programs of mine are 'OSI Certified Open Source Software';
	you can redistribute them and/or modify them under the terms of
	The Artistic License, a copy of which is available at:
	http://www.opensource.org/licenses/index.html

=cut
