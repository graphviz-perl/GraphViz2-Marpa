package GraphViz2::Marpa::Utils;

use strict;
use warnings;

use File::Slurp; # For read_dir.

use Hash::FieldHash ':all';

use IO::File;

use Text::CSV_XS;

our $VERSION = '1.04';

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
	$self = from_hash($self, $arg);

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

sub read_csv_file
{
	my($self, $file_name) = @_;
	my($csv) = Text::CSV_XS -> new({allow_whitespace => 1});
	my($io)  = IO::File -> new($file_name, 'r');

	$csv -> column_names($csv -> getline($io) );

	return $csv -> getline_hr_all($io);

} # End of read_csv_file.

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

=head2 get_files($dir_name, $type)

Returns a sorted list of files of type (extension) $type from directory $dir_name.

=head2 justify($string)

Right justify the $string in a field of 20 spaces.

=head2 new()

See L</Constructor and Initialization> for details on the parameters accepted by L</new()>.

=head2 read_csv_file($file_name)

Read the named CSV file into an arrayref of hashrefs.

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
