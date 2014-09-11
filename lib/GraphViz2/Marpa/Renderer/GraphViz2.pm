package GraphViz2::Marpa::Renderer::GraphViz2;

use strict;
use warnings;

use Log::Handler;

use Moo;

use Types::Standard qw/Any Str/;

has logger =>
(
	default  => sub{return undef},
	is       => 'rw',
	isa      => Any,
	required => 0,
);

has maxlevel =>
(
	default  => sub{return 'info'},
	is       => 'rw',
	isa      => Str,
	required => 0,
);

has minlevel =>
(
	default  => sub{return 'error'},
	is       => 'rw',
	isa      => Str,
	required => 0,
);

has output_file =>
(
	default  => sub{return ''},
	is       => 'rw',
	isa      => Str,
	required => 0,
);

has tree =>
(
	default  => sub{return ''},
	is       => 'rw',
	isa      => Any,
	required => 0,
);


our $VERSION = '2.00';

# --------------------------------------------------

sub BUILD
{
	my($self) = @_;

	if (! defined $self -> logger)
	{
		$self -> logger(Log::Handler -> new);
		$self -> logger -> add
		(
			screen =>
			{
				maxlevel       => $self -> maxlevel,
				message_layout => '%m',
				minlevel       => $self -> minlevel,
			}
		);
	}

} # End of BUILD.

# --------------------------------------------------

sub format_node
{
	my($self, $node, $opts) = @_;
	my($name)           = $node -> name;
	my($attributes)     = $node -> attributes;
	my($attr_string)    = $self -> tree -> hashref2string($attributes);
	my($type)           = $$attributes{type} || '';
	my($value)          = $$attributes{value} || '';
	my($depth)          = $$opts{_depth};
	my($previous_name)  = ${$$opts{previous_name} };
	my($previous_value) = ${$$opts{previous_value} };
	my($dot_input)      = '';

	my($indent);
	my($offset);

	if ($name eq 'literal')
	{
		if ($value =~ /(?:digraph|graph|strict|=)/)
		{
			$dot_input .= "$value ";
		}
		elsif ($value =~ /(?:\{|\[)/)
		{
			$offset    = ($previous_name =~ /(?:class|node_id|string)/) ? 3 : 2;
			$indent    = "\t" x ($depth - $offset);
			$dot_input .= "\n$indent$value\n";
		}
		elsif ($value =~ /(?:}|])/)
		{
			$offset    = ($previous_name =~ /(?:class|node_id|string)/) ? 3 : 2;
			$indent    = "\t" x ($depth - $offset);
			$dot_input .= "\n$indent$value\n";
		}
	}
	elsif ($name =~ /(?:node_id|string)/)
	{
		$indent                    = ($previous_value eq '=') ? ' ' : "\t" x ($depth - 2);
		$dot_input                 .= "$indent$value";
		$dot_input                 .= ($previous_value eq '=') ? "\n" : ' ';
		${$$opts{previous_name} }  = $name;
	}
	elsif ($name eq 'attribute')
	{
		$indent    = "\t" x ($depth - 2);
		$dot_input .= "\n$indent$type = $value";
	}
	elsif ($name eq 'class')
	{
		$indent                    = "\t" x ($depth - 2);
		$dot_input                 .= "\n$indent$value";
		${$$opts{previous_name} }  = $name;
	}

	${$$opts{dot_input} }      .= $dot_input;
	${$$opts{previous_value} } = $value;

} # End of format_node.

# --------------------------------------------------

sub log
{
	my($self, $level, $s) = @_;

	$self -> logger -> $level($s) if ($self -> logger);

} # End of log.

# --------------------------------------------------

sub run
{
	my($self)           = @_;
	my($dot_input)      = '';
	my($previous_name)  = '';
	my($previous_value) = '';

	$self -> log(info => 'Rendering.....');

	$self -> tree -> walk_down
	({
		callback => sub
		{
			my($node, $opts) = @_;

			# Note: This $node is a Tree::DAG_Node node, not a Graphviz node.

			$self -> format_node($node, $opts);

			# Keep recursing.

			return 1;
		},
		_depth         => 0,
		dot_input      => \$dot_input,
		previous_name  => \$previous_name,
		previous_value => \$previous_value,
	});

	my($separator) = '-' x 50;

	$self -> log(info => $separator);
	$self -> log(info => $dot_input);
	$self -> log(info => $separator);

	# Return 0 for success and 1 for failure.

	return 0;

} # End of run.

# --------------------------------------------------

1;

=pod

=head1 NAME

L<GraphViz2::Marpa::Renderer::GraphViz2> - A renderer for Graphviz dot files

=head1 Synopsis

See L<GraphViz2::Marpa/Synopsis>.

=head1 Description

L<GraphViz2::Marpa::Renderer> provides a renderer for L<Graphviz|http://www.graphviz.org/> (dot) graph definitions
parsed by L<GraphViz2::Marpa>.

It outputs a string to the output file, which (ideally) exactly matches the graph definition input to the paser, although there might be small differences in the line-by-line
formatting.

This module is the default rendering engine for L<GraphViz2::Marpa>.

The Marpa grammar as an image: L<http://savage.net.au/Ron/html/graphviz2.marpa/Marpa.Grammar.svg>. This image was created
with L<Graphviz|http://www.graphviz.org/> via L<GraphViz2>.

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

C<new()> is called as C<< my($renderer) = GraphViz2::Marpa::Renderer::GraphViz2 -> new(k1 => v1, k2 => v2, ...) >>.

It returns a new object of type C<GraphViz2::Marpa::Renderer::GraphViz2>.

Key-value pairs accepted in the parameter list (see corresponding methods for details
[e.g. maxlevel()]):

=over 4

=item o logger => $logger_object

Specify a logger object.

To disable logging, just set logger to the empty string.

Default: An object of type L<Log::Handler>.

To disable logging, just set 'logger' to the empty string (not undef).

=item o maxlevel => $level

This option is only used if this module creates an object of type L<Log::Handler>. See L<Log::Handler::Levels>.

Default: 'notice'.

=item o minlevel => $level

This option is only used if this module creates an object of type L<Log::Handler>. See L<Log::Handler::Levels>.

Default: 'error'.

No lower levels are used.

=item o output_file => $file_name

Specify the name of the output file to write. This will contain the text string of the rendered graph.

Default: ''.

The default means the output file is not written. Use the L</output_string()> method to retrieve the string.

=item o tree => anObjectOfTypeTreeDAG_Node

Specify the tree tokens output by the parser.

This tree is output from L<GraphViz2::Marpa>.

Default: [].

=back

=head1 Methods

=head2 log($level, $s)

Calls $self -> logger -> $level($s) if ($self -> logger).

=head2 logger([$logger_object])

Here, the [] indicate an optional parameter.

Get or set the logger object.

To disable logging, just set 'logger' to the empty string (not undef), in the call to L</new()>.

'logger' is a parameter to L</new()>. See L</Constructor and Initialization> for details.

=head2 maxlevel([$string])

Here, the [] indicate an optional parameter.

Get or set the value used by the logger object.

This option is only used if L<GraphViz2::Marpa:::Lexer> or L<GraphViz2::Marpa::Parser>
create an object of type L<Log::Handler>. See L<Log::Handler::Levels>.

'maxlevel' is a parameter to L</new()>. See L</Constructor and Initialization> for details.

=head2 minlevel([$string])

Here, the [] indicate an optional parameter.

Get or set the value used by the logger object.

This option is only used if L<GraphViz2::Marpa:::Lexer> or L<GraphViz2::Marpa::Parser>
create an object of type L<Log::Handler>. See L<Log::Handler::Levels>.

'minlevel' is a parameter to L</new()>. See L</Constructor and Initialization> for details.

=head2 output_file([$file_name])

Here, the [] indicate an optional parameter.

Get or set the name of the output file. This will contain the text string of the rendered graph.

If the output file name is not set, use the L</output_string()> method to retrieve the string.

'output_file' is a parameter to L</new()>. See L</Constructor and Initialization> for details.

=head2 run()

Renders the tree of parsed tokens as a string and, optionally, writes that string to the output file.

Returns 0 for success and 1 for failure.

=head2 tokens()

Here, the [] indicate an optional parameter.

Gets or sets the tree of tokens to be rendered.

'tree' is a parameter to L</new()>. See L</Constructor and Initialization> for details.

=head1 FAQ

=head2 If I input x.gv and output x.rend, should these 2 files be identical?

Yes - at least in the sense that running dot with them as input will produce the same output files.
This is using the default renderer, of course.

Since comments in *.gv files are discarded, they can never be in the output files (*.rend).

So, if x.gv is formatted as I do, then x.rend will be formatted identically.

=head1 Machine-Readable Change Log

The file CHANGES was converted into Changelog.ini by L<Module::Metadata::Changes>.

=head1 Version Numbers

Version numbers < 1.00 represent development versions. From 1.00 up, they are production versions.

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
