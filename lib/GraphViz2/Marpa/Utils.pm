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
use GraphViz2::Marpa::Lexer;
use GraphViz2::Marpa::Lexer::DFA;
use GraphViz2::Marpa::Parser;

use Hash::FieldHash ':all';

use HTML::Entities::Interpolate;

use Module::Path 'module_path';

use Text::CSV;
use Text::CSV::Slurp;
use Text::Xslate 'mark_raw';

fieldhash my %config => 'config';

our $VERSION = '1.11';

# -----------------------------------------------

sub generate_code_attributes_csv
{
	my($self, $heading)  = @_;
	my(@script)          = grep{/pl$/} @$heading;
	my($script_dir_name) = 'scripts';

	my(@lines);
	my(@mutators, %mutators);
	my($name);
	my($script_file_name);

	for my $script (@script)
	{
		$script_file_name      = File::Spec -> catfile($script_dir_name, $script);
		@lines                 = read_file($script_file_name, {chomp => 1});
		@mutators              = grep{s/=.//; $_} grep{s/^\t'(.+)'.+/$1/; $1} @lines;
		$mutators{$script}     = {} if (! $mutators{$script});
		$mutators{$script}{$_} = 1 for @mutators;
	}

	my(@module)      = grep{/pm$/} @$heading;
	my(%module_name) =
	(
		'DFA.pm'      => 'GraphViz2::Marpa::Lexer::DFA',
		'Lexer.pm'    => 'GraphViz2::Marpa::Lexer',
		'Marpa.pm'    => 'GraphViz2::Marpa',
		'Parser.pm'   => 'GraphViz2::Marpa::Parser',
		'Renderer.pm' => 'GraphViz2::Marpa::Renderer::GraphViz2',
	);

	my($module_file_name);

	for my $module (@module)
	{
		$module_file_name      = module_path($module_name{$module}) || die "Unable to find $module\n";
		@lines                 = read_file($module_file_name, {chomp => 1});
		@mutators              = grep{s/=.//; $_} grep{s/^fieldhash my %([^\s]+).+/$1/; $1} @lines;
		$mutators{$module}     = {} if (! $mutators{$module});
		$mutators{$module}{$_} = 1 for @mutators;
	}

	my(%names);

	for my $row (keys %mutators)
	{
		for my $column (keys %{$mutators{$row} })
		{
			$names{$column}{$row} = defined($mutators{$row}{$column}) ? 'Y' : '.';
		}
	}

	my($data_dir_name)  = 'data';
	my($code_file_name) = File::Spec -> catfile($data_dir_name, 'code.attributes.csv');
	my($csv)            = Text::CSV -> new;

	my($status);

	open(OUT, '>', $code_file_name) || die "Can't open(> $code_file_name)";

	$csv -> combine('Mutator', @$heading) || die "Can't combine headings\n";

	print OUT $csv -> string, "\n";

	my(@column);

	for my $mutator (sort keys %names)
	{
		@column = ();

		for $name (@$heading)
		{
			push @column, $mutator if ($#column < 0);
			push @column, $names{$mutator}{$name};
		}

		$csv -> combine(@column) || die "Can't combine columns\n";

		print OUT $csv -> string, "\n";
	}

	close OUT;

} # End of generate_code_attributes_csv.

# -----------------------------------------------

sub generate_code_attributes_index
{
	my($self)    = @_;
	my(@heading) = qw/lex.pl Lexer.pm DFA.pm parse.pl Parser.pm rend.pl g2m.pl Marpa.pm Renderer.pm/;

	$self -> generate_code_attributes_csv(\@heading);

	my($data_dir_name)   = 'data';
	my($code_file_name)  = File::Spec -> catfile($data_dir_name, 'code.attributes.csv');
	my($code_attributes) = Text::CSV::Slurp -> new -> load(file => $code_file_name, allow_whitespace => 1);

	my($column, @column);
	my(@row);

	for $column ('Mutator', @heading)
	{
		push @column, {td => $column};
	}

	push @row, [@column];

	for my $item (@$code_attributes)
	{
		@column = ();

		for $column ('Mutator', @heading)
		{
			push @column, {td => $$item{$column} };
		}

		push @row, [@column];
	}

	@column = ();

	for $column ('Mutator', @heading)
	{
		push @column, {td => $column};
	}

	push @row, [@column];

	my($config)    = $self -> config;
	my($templater) = Text::Xslate -> new
	(
		input_layer => '',
		path        => $$config{template_path},
	);
	my($html_dir_name) = 'html';
	my($file_name)     = File::Spec -> catfile($html_dir_name, 'code.attributes.html');

	open(OUT, '>', $file_name);
	print OUT $templater -> render
	(
	'code.attributes.tx',
	{
		border          => 1,
		default_css     => "$$config{css_url}/default.css",
		environment     => $self -> generate_demo_environment,
		fancy_table_css => "$$config{css_url}/fancy.table.css",
		title           => 'Code and Command Line Attributes for GraphViz2::Marpa',
		row             => \@row,
		summary         => 'Code attributes',
		version         => $VERSION,
	},
	);
	close OUT;

	# Return 0 for success and 1 for failure.

	return 0;

} # End of generate_code_attributes_index.

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
	{left => 'OS',     right => 'Debian V 6'},
	{left => 'Perl',   right => $Config{version} };

	return \@environment;

} # End of generate_demo_environment.

# ------------------------------------------------

sub generate_stt_index
{
	my($self) = @_;
	my(@heading)       = qw/Start Accept State Event Next Entry Exit Regexp Interpretation/;
	my($data_dir_name) = 'data';
	my($stt_file_name) = File::Spec -> catfile($data_dir_name, 'default.stt.csv');
	my($stt)           = Text::CSV::Slurp -> new -> load(file => $stt_file_name, allow_whitespace => 1);

	my($column, @column);
	my(@row);

	for $column (@heading)
	{
		push @column, {td => $column};
	}

	push @row, [@column];

	for my $item (@$stt)
	{
		@column = ();

		for $column (@heading)
		{
			push @column, {td => mark_raw($$item{$column} || '.')};
		}

		push @row, [@column];
	}

	@column = ();

	for $column (@heading)
	{
		push @column, {td => $column};
	}

	push @row, [@column];

	my($config)    = $self -> config;
	my($templater) = Text::Xslate -> new
	(
		input_layer => '',
		path        => $$config{template_path},
	);
	my($html_dir_name) = 'html';
	my($file_name)     = File::Spec -> catfile($html_dir_name, 'stt.html');

	open(OUT, '>', $file_name) || die "Can't open(> $file_name): $!";
	print OUT $templater -> render
	(
	'stt.tx',
	{
		border          => 1,
		default_css     => "$$config{css_url}/default.css",
		environment     => $self -> generate_demo_environment,
		fancy_table_css => "$$config{css_url}/fancy.table.css",
		title           => 'State Transition Table for GraphViz2::Marpa::Lexer',
		row             => \@row,
		summary         => 'STT',
		version         => $VERSION,
	},
	);
	close OUT;

	# Return 0 for success and 1 for failure.

	return 0;

} # End of generate_stt_index.

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

=head2 generate_code_attributes_csv()

Generate data/code.attributes.csv, for conversion into html/code.attributes.html.

=head2 generate_code_attributes_index()

Generate html/code.attributes.html.

=head2 generate_demo_index()

Generates html/index.html.

Does not run any programs to generate other files, e.g. html/*.svg. See scripts/generate.demo.sh for that.

=head2 generate_demo_environment()

Called by generate_demo_index().

Generates a table to be inserted into html/index.html.

See scripts/generate.demo.pl.

=head2 generate_stt_index()

Generate html/stt.html.

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
