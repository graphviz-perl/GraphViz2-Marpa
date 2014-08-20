package GraphViz2::Marpa::Actions;

use strict;
use utf8;
use warnings;
use warnings  qw(FATAL utf8);    # Fatalize encoding glitches.
use open      qw(:std :utf8);    # Undeclared streams in UTF-8.
use charnames qw(:full :short);  # Unneeded in v5.16.

# Warning: Do not use Moo or anything similar.
# This class needs a sub new() due to the way
# Marpa::R2 calls the constructor.

our $caller;

our $VERSION = '2.03';

# --------------------------------------------------

sub digraph
{
	my($stash, $t1) = @_;

	$caller -> log(debug => "digraph($t1)");

	return $t1;

} # End of digraph.

# --------------------------------------------------

sub graph
{
	my($stash, $t1) = @_;

	$caller -> log(debug => "graph($t1)");

	return $t1;

} # End of graph.

# --------------------------------------------------

sub graph_id
{
	my($stash, $t1) = @_;
	$t1 = '' if (length($t1) == 0);

	$caller -> log(debug => "graph_id($t1)");

	return $t1;

} # End of graph_id.

# ------------------------------------------------

sub new
{
	my($class) = @_;

	$caller -> log(debug => 'new()');

	return bless {}, $class;

} # End of new.

# --------------------------------------------------

sub non_strict
{
	my($stash, $t1) = @_;

	$caller -> log(debug => "non_strict($t1)");

	return $t1;

} # End of non_strict.

# --------------------------------------------------

sub prolog
{
	my($stash, $t1) = @_;

	$caller -> log(debug => "prolog($t1)");

	return $t1;

} # End of prolog.

# --------------------------------------------------

sub strict
{
	my($stash, $t1) = @_;

	$caller -> log(debug => "strict($t1)");

	return $t1;

} # End of strict.

# ------------------------------------------------

1;

=pod

=head1 NAME

C<Graph::Easy::Marpa::Actions> - A namespace for Graph::Easy::Marpa::Parser, called via Marpa

=head1 Synopsis

End-users do not need to call the methods in this module.

Only L<Marpa::R2> does that, under certain conditions as specified in the grammar declared in
L<Graph::Easy::Marpa::Parser>.

=head1 Description

C<Graph::Easy::Marpa::Actions> provides a namespace for L<Graph::Easy::Marpa::Parser>'s Marpa-style actions.

See L<Graph::Easy::Marpa::Parser> for details.

=head1 Installation

Install L<Graph::Easy::Marpa> as you would for any C<Perl> module:

Run:

	cpanm Graph::Easy::Marpa

or run:

	sudo cpan Graph::Easy::Marpa

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

C<new()> is called as C<< my($action) = Graph::Easy::Marpa::Actions -> new() >>.

It returns a new object of type C<Graph::Easy::Marpa::Actions>.

Such calls to new() have no parameters, and are only ever created by L<Marpa::R2>.

=head1 Methods

=head2 graph($graph)

Returns $graph.

Called as appropriate by L<Marpa::R2>.

=head2 new()

Returns a hashref, currently empty.

Called as appropriate by L<Marpa::R2>.

=head1 Machine-Readable Change Log

The file Changes was converted into Changelog.ini by L<Module::Metadata::Changes>.

=head1 Version Numbers

Version numbers < 1.00 represent development versions. From 1.00 up, they are production versions.

=head1 Support

Email the author, or log a bug on RT:

L<https://rt.cpan.org/Public/Dist/Display.html?Name=Graph::Easy::Marpa>.

=head1 Author

L<Graph::Easy::Marpa> was written by Ron Savage I<E<lt>ron@savage.net.auE<gt>> in 2011.

Home page: L<http://savage.net.au/index.html>.

=head1 Copyright

Australian copyright (c) 2011, Ron Savage.

	All Programs of mine are 'OSI Certified Open Source Software';
	you can redistribute them and/or modify them under the terms of
	The Artistic License, a copy of which is available at:
	http://www.opensource.org/licenses/index.html

=cut
