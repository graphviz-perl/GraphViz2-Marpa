package GraphViz2::Marpa::Renderer::GraphViz2;

use feature 'switch';
use strict;
use warnings;

use GraphViz2::Marpa::Utils;

use Hash::FieldHash ':all';

use Log::Handler;

use Set::Array;

fieldhash my %items         => 'items';
fieldhash my %logger        => 'logger';
fieldhash my %maxlevel      => 'maxlevel';
fieldhash my %minlevel      => 'minlevel';
fieldhash my %output_file   => 'output_file';
fieldhash my %output_string => 'output_string';
fieldhash my %parsed_file   => 'parsed_file';
fieldhash my %tokens        => 'tokens';
fieldhash my %utils         => 'utils';

our $VERSION = '1.06';

# --------------------------------------------------

sub format_attributes
{
	my($self, $attributes) = @_;

	$self -> new_item('[', 1, 0);

	my($name);
	my($value);

	while (@$attributes)
	{
		($name, $value) = (shift @$attributes, shift @$attributes);
		$value          = qq|"$value"| if ($value !~ /^<</); # No quotes for HTML-like labels.

		$self -> new_item(qq|$name = $value|, 1, 0, 0);
	}

	$self -> new_item(']', 1, 0, 0);

} # End of format_attributes.

# --------------------------------------------------

sub format_output
{
	my($self)       = @_;
	my($depth)      = 0;
	my($output)     = '';
	my($last_value) = '';

	my($newline);
	my($value);

	for my $item ($self -> items -> print)
	{
		$newline = $$item{newline};
		$value   = $$item{value};

		if ($newline)
		{
			$output .= "\n";
		}

		if ($value =~ /[}\]]/)
		{
			$depth--;
		}

		# Check for '--' and '->'. They are space-separated from the previous item. Likewise a subgraph id.
		# Ports and colons are juxtaposed with the previous item.
		# Lastly, if the $value is, don't ouput any spaces around it.

		$output .= ($$item{juxtaposed} ? '' : $$item{spaced} ? $value ? ' ' : '' : ("\t" x $depth) ) . $value;

		if ($value =~ /[{[]/)
		{
			$depth++;
		}

		$last_value = $value;
	}

	$output .= "\n";

	$self -> output_string($output);

	return $output;

} # End of format_output.

# --------------------------------------------------

sub _init
{
	my($self, $arg)      = @_;
	$$arg{items}         = Set::Array -> new;
	$$arg{logger}        ||= defined($$arg{logger}) ? $$arg{logger} : undef; # Caller can set.
	$$arg{maxlevel}      ||= 'notice'; # Caller can set.
	$$arg{minlevel}      ||= 'error';  # Caller can set.
	$$arg{output_file}   ||= '';       # Caller can set.
	$$arg{output_string} = '';
	$$arg{parsed_file}   ||= ''; # Caller can set.
	$$arg{tokens}        ||= []; # Caller can set.
	$$arg{utils}         = GraphViz2::Marpa::Utils -> new;
	$self                = from_hash($self, $arg);

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

	return $self;

} # End of _init.

# --------------------------------------------------

sub log
{
	my($self, $level, $s) = @_;

	$self -> logger -> $level($s) if ($self -> logger);

} # End of log.

# --------------------------------------------------

sub new
{
	my($class, %arg) = @_;
	my($self)        = bless {}, $class;
	$self            = $self -> _init(\%arg);

	return $self;

}	# End of new.

# --------------------------------------------------

sub new_id
{
	my($self, $last_type, $type, $value) = @_;

	my($newline);

	if ($last_type eq 'edge_id')
	{
		$newline = 0;
	}
	elsif ($type eq 'edge_id')
	{
		$newline = 0;
	}
	else
	{
		$newline = 1;
	}

	# Some node names must be enclosed in "...".

	if ( ($type eq 'node_id') and ($value !~ /^[A-Za-z0-9][A-Za-z0-9_]*$/) )
	{
		$value = qq|"$value"|;
	}

	$self -> items -> push
		({
			juxtaposed => 0,
			newline    => $newline,
			spaced     => $last_type eq 'edge_id' ? 1 : $value =~ /^-/ ? 1 : 0,
			value      => $value,
		 });

} # End of new_id.

# --------------------------------------------------

sub new_item
{
	my($self, $value, $newline, $juxtaposed, $spaced) = @_;

	$self -> items -> push
		({
			juxtaposed => $juxtaposed,
			newline    => $newline,
			spaced     => $spaced,
			value      => $value,
		 });

} # End of new_item.

# --------------------------------------------------

sub run
{
	my($self) = @_;

	if ($#{$self -> tokens} < 0)
	{
		for my $record (@{$self -> utils -> read_csv_file($self -> parsed_file)})
		{
			push @{$self -> tokens}, {type => $$record{type}, value => $$record{value} };
		}
	}

	my($depth)     = 0;
	my($last_type) = '';
	my($output)    = '';
	my(@token)     = @{$self -> tokens};

	my(@attributes);
	my(@output);
	my($token, $type);
	my($value);

	for my $i (0 .. $#token)
	{
		$token = $token[$i];
		$type  = $$token{type};
		$value = $$token{value};

		given ($type)
		{
			when ('strict')          {$self -> new_item($value eq 'no' ? '' : 'strict ', 0, 0, 0);}
			when ('digraph')         {$self -> new_item($value eq 'no' ? 'graph' : 'digraph', 0, 0, 0);}
			when ('graph_id')        {$self -> new_item($value, 0, 0, 1);}
			when ('start_scope')     {$self -> new_item('{', 1, 0, 0);}
			when ('end_scope')       {$self -> new_item('}', 1, 0, 0);}
			when ('start_subgraph')  {$self -> new_item('subgraph', 1, 0, 0);}
			when ('end_subgraph')    {}
			when ('start_attribute') {@attributes = ();}
			when ('attribute_id')    {push @attributes, $value;}
			when ('attribute_value') {push @attributes, $value;}
			when ('end_attribute')   {$self -> format_attributes([@attributes]);}
			when ('open_brace')      {$self -> new_item('{', 1, 0, 0);}
			when ('close_brace')     {$self -> new_item('}', 1, 0, 0);}
			when ('class_id')        {$self -> new_id($last_type, $type, $value);}
			when ('edge_id')         {$self -> new_id($last_type, $type, $value);}
			when ('node_id')         {$self -> new_id($last_type, $type, $value);}
			when ('colon')           {$self -> new_item($value, 0, 1, 0);}
			when ('port_id')         {$self -> new_item($value, 0, 1, 0);}
			when ('compass_point')   {$self -> new_item($value, 0, 1, 0);}
			default                  {die "Unexpected type '$type' (with value '$value') in the input";}
		}

		$last_type = $type;
	}

	my($output_file) = $self -> output_file;

	if ($output_file)
	{
		open(OUT, '>', $output_file) || die "Can't open(> $output_file): $!";
		print OUT $self -> format_output;
		close OUT;
	}

	# Return 0 for success and 1 for failure.

	return 0;

} # End of run.

# --------------------------------------------------

1;

=pod

=head1 NAME

L<GraphViz2::Marpa::Renderer::GraphViz2> - A renderer for lexed and parsed Graphviz dot files

=head1 Synopsis

See L<GraphViz2::Marpa/Synopsis>.

=head1 Description

L<GraphViz2::Marpa::Renderer> provides a renderer for L<Graphviz|http://www.graphviz.org/> (dot) graph definitions
lexed by L<GraphViz2::Marpa::Lexer> and parsed by L<GraphViz2::Marpa::Parser>.

It outputs a string to the output file, which (ideally) exactly matches the graph definition input to the lexer, although there might be small differences in the line-by-line
formatting.

This module is the default rendering engine for L<GraphViz2::Marpa>.

Demo lexer/parser output: L<http://savage.net.au/Perl-modules/html/graphviz2.marpa/index.html>.

State Transition Table: L<http://savage.net.au/Perl-modules/html/graphviz2.marpa/default.stt.html>.

Command line options and object attributes: L<http://savage.net.au/Perl-modules/html/graphviz2.marpa/code.attributes.html>.

My article on this set of modules: L<http://www.perl.com/pub/2012/10/an-overview-of-lexing-and-parsing.html>.

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

=item o parsed_file => aParsedOutputFileName

Specify the name of a CSV file of parsed tokens to read.

Default: ''.

This file is read only if the token option (next) does not contain an arrayref of tokens to process.

=item o tokens => $arrayref

Specify the arrayref of tokens output by the parser.

The format of this array is documented in L<GraphViz2::Marpa::Parser/How is the parsed graph stored in RAM?>.

Default: [].

=back

=head1 Methods

=head2 format_attributes($attributes)

$attributes is an arrayref of interleaved attribute ids and values.

Called by L</run()>.

=head2 format_output()

Called by L</run()>.

Sets L</output_string()>.

=head2 items()

Returns an arrayref of items output by the renderer.

=head2 log($level, $s)

Calls $self -> logger -> $level($s) if ($self -> logger).

=head2 logger([$logger_object])

'logger' is a parameter to L</new()>. See L</Constructor and Initialization> for details.

Here, the [] indicate an optional parameter.

Get or set the logger object.

To disable logging, just set 'logger' to the empty string (not undef), in the call to L</new()>.

=head2 maxlevel([$string])

'maxlevel' is a parameter to L</new()>. See L</Constructor and Initialization> for details.

Here, the [] indicate an optional parameter.

Get or set the value used by the logger object.

This option is only used if L<GraphViz2::Marpa:::Lexer> or L<GraphViz2::Marpa::Parser>
create an object of type L<Log::Handler>. See L<Log::Handler::Levels>.

=head2 minlevel([$string])

'minlevel' is a parameter to L</new()>. See L</Constructor and Initialization> for details.

Here, the [] indicate an optional parameter.

Get or set the value used by the logger object.

This option is only used if L<GraphViz2::Marpa:::Lexer> or L<GraphViz2::Marpa::Parser>
create an object of type L<Log::Handler>. See L<Log::Handler::Levels>.

=head2 new()

See L</Constructor and Initialization> for details on the parameters accepted by L</new()>.

=head2 output_file([$file_name])

'output_file' is a parameter to L</new()>. See L</Constructor and Initialization> for details.

Here, the [] indicate an optional parameter.

Get or set the name of the output file. This will contain the text string of the rendered graph.

If the output file name is not set, use the L</output_string()> method to retrieve the string.

=head2 output_string()

Returns the text string of the rendered graph.

To save the output in a file, use the 'output_file' parameter to L</new()>.

=head2 parsed_file([$file_name])

'parsed_file' is a parameter to L</new()>. See L</Constructor and Initialization> for details.

Here, the [] indicate an optional parameter.

Get or set the name of the file of parsed tokens to read.

=head2 run()

Renders the arrayref of items as a string and, optionally, writes that string to the output file.

Calls L</format_output()>.

See L</output_string()>.

Returns 0 for success and 1 for failure.

=head2 tokens([$arrayref])

'tokens' is a parameter to L</new()>. See L</Constructor and Initialization> for details.

Here, the [] indicate an optional parameter.

Gets or sets the arrayref of tokens to be rendered.

=head2 utils([$aUtilsObject])

Here, the [] indicate an optional parameter.

Get or set the utils object.

Default: A object of type L<GraphViz2::Marpa::Utils>.

=head1 FAQ

=head2 If I input x.gv and output x.rend, should these 2 files be identical?

Yes - at least in the sense that running dot with them as input will produce the same output files.
This is using the default renderer, of course.

Since comments in *.gv files are discarded, they can never be in the output files (*.lex, *.parse and *.rend).

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
