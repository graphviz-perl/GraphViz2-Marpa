package GraphViz2::Marpa::Lexer;

use strict;
use warnings;

use Data::Section::Simple 'get_data_section';

use GraphViz2::Marpa::Lexer::DFA;
use GraphViz2::Marpa::Utils;

use Hash::FieldHash ':all';

use List::Compare;

use Log::Handler;

use Perl6::Slurp;

use Set::Array;
use Set::FA::Element;

use Text::CSV::Slurp;

use Try::Tiny;

fieldhash my %description  => 'description';
fieldhash my %dfa          => 'dfa';
fieldhash my %graph_text   => 'graph_text';
fieldhash my %input_file   => 'input_file';
fieldhash my %items        => 'items';
fieldhash my %lexed_file   => 'lexed_file';
fieldhash my %logger       => 'logger';
fieldhash my %maxlevel     => 'maxlevel';
fieldhash my %minlevel     => 'minlevel';
fieldhash my %report_items => 'report_items';
fieldhash my %report_stt   => 'report_stt';
fieldhash my %stt_file     => 'stt_file';
fieldhash my %timeout      => 'timeout';
fieldhash my %type         => 'type';
fieldhash my %utils        => 'utils';

our $VERSION = '1.10';

# --------------------------------------------------

sub _check_accept
{
	my($self, $value, $current, $state) = @_;

	my($this)                = $$current{name};
	my($tos)                 = $#{$$state{$this} };
	my($item)                = ${$$state{$this} }[$tos];
	$$item{accept}           = $value && ($value =~ /Yes/i) ? $$current{name} : 0;
	${$$state{$this} }[$tos] = $item;

} # End of _check_accept.

# --------------------------------------------------

sub _check_all_nexts
{
	my($self, $state) = @_;

	# Every state's next state must exist.

	my($item);
	my($next);

	for my $name (keys %$state)
	{
		for my $event_index (0 .. $#{$$state{$name} })
		{
			$item = ${$$state{$name} }[$event_index];
			$next = $$item{next_state};

			if (! $$state{$next})
			{
				die "State '$name'. The next state '$next' is not defined";
			}
		}
	}

} # End of _check_all_nexts;

# --------------------------------------------------

sub _check_csv_headings
{
	my($self, $stt) = @_;
	my($result)     = List::Compare -> new([grep{!/Interpretation|Regexp/} keys(%$stt)], [qw/Start Accept State Event Next Entry Exit/]);
	my(@unique)     = $result -> get_unique;
	my(@complement) = $result -> get_complement;

	if ($#unique >= 0)
	{
		die "Unexpected column heading(s) '" . join("', '", @unique) . "' in the CSV file";
	}

	if ($#complement >= 0)
	{
		die "Column heading(s) '" . join("', '", @complement) . "' not found in the CSV file";
	}

} # End of _check_csv_headings.

# --------------------------------------------------

sub _check_event
{
	my($self, $value, $current, $state) = @_;

	if (! defined $value)
	{
		die "Cell for state '$$current{current}' must contain an event";
	}

	# Every state's events must be unique.

	my($this) = $$current{name};

	my(%event);
	my($item);

	for my $event_index (0 .. $#{$$state{$this} })
	{
		$item = ${$$state{$this} }[$event_index];

		if ($$item{event} eq $value)
		{
			die "State '$this'. The event '$value' is not unique";
		}
	}

	push @{$$state{$this} },
	{
		entry      => '',
		event      => $value,
		exit       => '',
		next_state => '',
	};

} # End of _check_event.

# --------------------------------------------------

sub _check_function
{
	my($self, $value, $current, $state, $function) = @_;

	my($this)                = $$current{name};
	my($tos)                 = $#{$$state{$this} };
	my($item)                = ${$$state{$this} }[$tos];
	$$item{$function}        = $value;
	${$$state{$this} }[$tos] = $item;

} # End of _check_function.

# --------------------------------------------------

sub _check_next
{
	my($self, $value, $current, $state) = @_;

	my($this)                = $$current{name};
	my($tos)                 = $#{$$state{$this} };
	my($item)                = ${$$state{$this} }[$tos];
	$$item{next_state}       = $value;
	${$$state{$this} }[$tos] = $item;

} # End of _check_next.

# --------------------------------------------------

sub _check_ods_headings
{
	my($self, $stt) = @_;
	my(%heading)    =
		(
		 A1 => 'Start',
		 B1 => 'Accept',
		 C1 => 'State',
		 D1 => 'Event',
		 E1 => 'Next',
		 F1 => 'Entry',
		 G1 => 'Exit',
		);

	my($column, $coord, $cell);
	my($value);

	for $column (qw/A B C D E F G/)
	{
		$coord = "${column}1";
		$cell  = $stt -> getTableCell(0, $coord);
		$value = $stt -> getCellValue($cell);

		if (! $value || ($value ne $heading{$coord}) )
		{
			die "Cell '$cell' should contain '$heading{$cell}'";
		}
	}

} # End of _check_ods_headings.

# --------------------------------------------------

sub _check_state
{
	my($self, $value, $current, $state) = @_;

	if ($value)
	{
		$$current{name} = $$current{previous} = $value;
	}
	else
	{
		$$current{name} = $$current{previous};
	}

	$value = $$current{name};

	if (! $$state{$value})
	{
		$$state{$value} = [];
	}

} # End of _check_state.

# --------------------------------------------------

sub generate_lexed_file
{
	my($self, $file_name) = @_;

	open(OUT, '>', $file_name) || die "Can't open(> $file_name): $!";

	print OUT qq|"type","value"\n|;

	for my $item (@{$self -> items})
	{
		print OUT $self -> utils -> justify($$item{type}), qq|, "$$item{value}"|, "\n";
	}

	close OUT;

} # End of generate_lexed_file.

# --------------------------------------------------

sub get_graph_from_command_line
{
	my($self) = @_;
	$self -> graph_text($self -> description);

} # End of get_graph_from_command_line.

# --------------------------------------------------

sub get_graph_from_file
{
	my($self) = @_;
	my(@line) = grep{$_ !~ m!^\s*(?:#|//)!} slurp($self -> input_file, {chomp => 1});

	$self -> graph_text(join(' ', @line) );

} # End of get_graph_from_file.

# --------------------------------------------------

sub _init
{
	my($self, $arg)     = @_;
	$$arg{description}  ||= ''; # Caller can set.
	$$arg{dfa}          = '';
	$$arg{graph_text}   = '';
	$$arg{input_file}   ||= ''; # Caller can set.
	$$arg{items}        = Set::Array -> new;
	$$arg{lexed_file}   ||= '';       # Caller can set.
	$$arg{logger}       = defined($$arg{logger}) ? $$arg{logger} : undef; # Caller can set.
	$$arg{maxlevel}     ||= 'notice'; # Caller can set.
	$$arg{minlevel}     ||= 'error';  # Caller can set.
	$$arg{report_items} ||= 0;        # Caller can set.
	$$arg{report_stt}   ||= 0;        # Caller can set.
	$$arg{stt_file}     ||= ''; # Caller can set.
	$$arg{timeout}      ||= 10; # Caller can set.
	$$arg{type}         ||= ''; # Caller can set.
	$$arg{utils}        = GraphViz2::Marpa::Utils -> new;
	$self               = from_hash($self, $arg);

	if ($self -> input_file =~ /csv$/)
	{
		$self -> type('csv');
	}

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

sub _process
{
	my($self, $start, $state) = @_;

	$self -> log(debug => 'Graph text: ' . $self -> graph_text);

	my($died)   = '';
	my($result) = 0; # Default to success.

	try
	{
		$self -> _check_all_nexts($state);
		$self -> dfa
			(
			 GraphViz2::Marpa::Lexer::DFA -> new
			 (
			  graph_text => $self -> graph_text,
			  logger     => $self -> logger,
			  report_stt => $self -> report_stt,
			  state      => $state,
			  start      => $start,
			 )
			);

		local $SIG{ALRM} = sub{$died = 'DFA timed out'; die};

		alarm $self -> timeout;

		$result = $self -> dfa -> run;
	}
	catch
	{
		# Don't overwrite $died if set due to the alarm.

		$died = $_ if (! $died);
	};

	alarm 0;

	if ($died)
	{
		$result = 1;

		$self -> log(error => $died);
	}

	if ($result == 0)
	{
		$self -> items -> push(@{$self -> dfa -> items});
		$self -> report if ($self -> report_items);

		my($file_name) = $self -> lexed_file;

		$self -> generate_lexed_file($file_name) if ($file_name);
	}

	$self -> log(info => $result ? 'Fail' : 'OK');

	# Return 0 for success and 1 for failure.

	return $result;

} # End of _process.

# --------------------------------------------------

sub _process_csv_file
{
	my($self, $stt) = @_;

	$self -> _check_csv_headings($$stt[0]);

	my($accept);
	my($column, %current);
	my($start, %state);
	my($value);

	for my $item (@$stt)
	{
		# Skip blank lines, i.e. lines not containing an event in column D.

		next if (! $$item{Event});

		for $column (qw/Start Accept State Event Next Entry Exit/)
		{
			$value = $$item{$column};

			if ($column eq 'Start')
			{
				if ($value && ($value =~ /Yes/i) )
				{
					# If column Start is Yes, column State is the name of the start state.

					$start = $$item{State};
				}
			}
			elsif ($column eq 'Accept')
			{
				$accept = $value;
			}
			elsif ($column eq 'State')
			{
				$self -> _check_state($value, \%current, \%state);
			}
			elsif ($column eq 'Event')
			{
				$self -> _check_event($value, \%current, \%state);
				$self -> _check_accept($accept, \%current, \%state);
			}
			elsif ($column eq 'Next')
			{
				$self -> _check_next($value, \%current, \%state);
			}
			else # Entry, Exit. Warning: Change next line to match.
			{
				$self -> _check_function($value, \%current, \%state, $column eq 'Entry' ? 'entry' : 'exit');
			}
		}
	}

	if (! $state{$start})
	{
		die "Start state '$start' is not defined";
	}

	return $self -> _process($start, \%state);

} # End of _process_csv_file.

# -----------------------------------------------

sub _read_internal_file
{
	my($self)   = @_;
	my($stt)    = get_data_section('stt');
	my(@stt)    = split(/\n/, $stt);
	my($csv)    = Text::CSV_XS -> new({allow_whitespace => 1});
	my($status) = $csv -> parse(shift @stt);

	if (! $status)
	{
		die 'Unable to read STT headers from __DATA__';
	}

	my(@column_name) = $csv -> fields;

	my(@field);
	my($i);
	my(%line);
	my(@row);

	for my $line (@stt)
	{
		$status = $csv -> parse($line);

		if (! $status)
		{
			die "Unable to read STT line '$line' from __DATA__";
		}

		@field = $csv -> fields;
		%line  = ();

		for $i (0 .. $#column_name)
		{
			$line{$column_name[$i]} = $field[$i];
		}

		push @row, {%line};
	}

	return \@row;

} # End of _read_internal_file.

# -----------------------------------------------

sub report
{
	my($self)   = @_;
	my($format) = '%4s  %-16s  %s';

	$self -> log(notice => sprintf($format, 'Item', 'Type', 'Value') );

	for my $item ($self -> items -> print)
	{
		$self -> log(notice => sprintf($format, $$item{count}, $$item{type}, $$item{value}) );
	}

} # End of report.

# --------------------------------------------------

sub run
{
	my($self) = @_;

	if ($self -> description)
	{
		$self -> get_graph_from_command_line;
	}
	elsif ($self -> input_file)
	{
		$self -> get_graph_from_file;
	}
	else
	{
		die 'You must provide a graph either by -i or -g';
	}

	my($result) = 1; # Default to failure.

	if (! $self -> type)
	{
		$result = $self -> _process_csv_file($self -> _read_internal_file);
	}
	elsif ($self -> type eq 'csv')
	{
		$result = $self -> _process_csv_file(Text::CSV::Slurp -> new -> load(file => $self -> stt_file, allow_whitespace => 1));
	}
	else
	{
#		die "type must be one of '', 'csv' or 'ods' for the state transition table file";
		die "type must be either '' or 'csv' for the state transition table file";
	}

	# Return 0 for success and 1 for failure.

	return $result;

} # End of run.

# --------------------------------------------------

1;

=pod

=head1 NAME

L<GraphViz2::Marpa::Lexer> - A Perl lexer for Graphviz dot files. Output goes to L<GraphViz2::Marpa::Parser>.

=head1 Synopsis

=over 4

=item o Display help

	perl scripts/lex.pl   -h
	perl scripts/parse.pl -h
	perl scripts/g2m.pl   -h

=item o Run the lexer

	perl scripts/lex.pl -input_file x.gv -lexed_file x.lex

	x.gv is a Graphviz dot file. x.lex will be a CSV file of lexed tokens.

=item o Run the parser without running the lexer or the default renderer

	perl scripts/parse.pl -lexed_file x.lex -parsed_file x.parse

	x.parse will be a CSV file of parsed tokens.

=item o Run the parser and the default renderer

	perl scripts/parse.pl -lexed_file x.lex -parsed_file x.parse -output_file x.rend

	x.rend will be a Graphviz dot file.

=item o Run the lexer, parser and default renderer

	perl scripts/g2m.pl -input_file x.gv -lexed_file x.lex -parsed_file x.parse -output_file x.rend

=back

=head1 Description

L<GraphViz2::Marpa::Lexer> provides a L<Set:FA::Element>-based lexer for L<http://www.graphviz.org/> dot files.

The output is intended to be input into L<GraphViz2::Marpa::Parser>.

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

C<new()> is called as C<< my($lexer) = GraphViz2::Marpa::Lexer -> new(k1 => v1, k2 => v2, ...) >>.

It returns a new object of type C<GraphViz2::Marpa::Lexer>.

Key-value pairs accepted in the parameter list (see corresponding methods for details
[e.g. L<description([$graph])>]):

=over 4

=item o description => $graphDescription

Read the L<Graphviz|http://www.graphviz.org/> (dot) graph definition from the command line.

You are strongly encouraged to surround this string with '...' to protect it from your shell.

See also the 'input_file' option to read the description from a file.

The 'description' option takes precedence over the 'input_file' option.

Default: ''.

=item o input_file => $aDotInputFileName

Read the L<Graphviz|http://www.graphviz.org/> (dot) graph definition from a file.

See also the 'description' option to read the graph definition from the command line.

The 'description' option takes precedence over the 'input_file' option.

Default: ''.

See the distro for data/*.gv.

=item o lexed_file => $aLexedOutputFileName

Specify the name of a CSV file of lexed tokens to write. This file can be input to the parser.

Default: ''.

The default means the file is not written.

See the distro for data/*.lex.

=item o logger => $aLoggerObject

Specify a logger compatible with L<Log::Handler>, for the lexer to use.

Default: A logger of type L<Log::Handler> which writes to the screen.

To disable logging, just set 'logger' to the empty string (not undef).

=item o maxlevel => $logOption1

This option affects L<Log::Handler>.

See the L<Log::Handler::Levels> docs.

Default: 'notice'.

=item o minlevel => $logOption2

This option affects L<Log::Handler>.

See the L<Log::Handler::Levels> docs.

Default: 'error'.

No lower levels are used.

=item o report_items => $Boolean

Log the items recognised by the lexer.

Default: 0.

=item o report_stt => $Boolean

Log the State Transition Table.

Calls L<Set::FA::Element/report()>. Set min and max log levels to 'info' for this.

Default: 0.

=item o stt_file => $sttFileName

Specify which file contains the State Transition Table.

Default: ''.

The default value means the STT is read from the source code of L<GraphViz2::Marpa::Lexer>.

Candidate files are '' and 'data/default.stt.csv'.

The type of this file must be specified by the 'type' option.

If the file name matches /csv$/, the value of the 'type' option is set to 'csv'.

=item o timeout => $seconds

Run the DFA for at most this many seconds.

Default: 10.

=item o type => $type

Specify the type of the stt_file: '' for internal STT and 'csv' for CSV.

Default: ''.

The default value means the STT is read from the source code of L<GraphViz2::Marpa::Lexer>.

This option must be used with the 'stt_file' option.

Warning: The 'ods' option is disabled, because I can find no way in LibreOffice to make it operate in ASCII. What happens is that when you type "
(i.e. the double-quote character on the keyboard), LibreOffice inserts a different double-quote character, which, when exported as CSV in Unicode
format, produces these 3 bytes: 0xe2, 0x80, 0x9c. This means that if you edit the STT, you absolutely must export to a CSV file in ASCII format.
It also means that dot identifiers in (normal) double-quotes will never match the double-quotes in the *.ods file.

=back

=head1 Methods

=head2 description([$graph])

The [] indicate an optional parameter.

Get or set the L<Graphviz|http://www.graphviz.org/> (dot) graph definition.

The value supplied by the 'description' option takes precedence over the value read from the 'input_file'.

See also L</input_file()>.

'description' is a parameter to L</new()>. See L</Constructor and Initialization> for details.

=head2 generate_lexed_file($file_name)

Write the lexed tokens to the named file.

Called as needed by L<run()>.

=head2 get_graph_from_command_line()

If the caller has requested a graph be parsed from the command line, with the 'description' option to L</new()>, get it now.

Called as appropriate by L</run()>.

=head2 get_graph_from_file()

If the caller has requested a graph be parsed from a file, with the 'input_file' option to L</new()>, get it now.

Called as appropriate by L</run()>.

=head2 graph_text([$graph])

The [] indicate an optional parameter.

Get or set the value of the L<Graphviz|http://www.graphviz.org/> (dot) graph definition string.

Called by L</get_graph_from_command_line()> and L</get_graph_from_file()>.

=head2 input_file([$graph_file_name])

Here, the [] indicate an optional parameter.

Get or set the name of the file to read the L<Graphviz|http://www.graphviz.org/> (dot) graph definition from.

The value supplied by the 'description' option takes precedence over the value read from the 'input_file'.

See also the L</description()> method.

'input_file' is a parameter to L</new()>. See L</Constructor and Initialization> for details.

=head2 items()

Returns an arrayref of lexed tokens. Each element of this arrayref is a hashref.

These lexed tokens do I<not> bear a one-to-one relationship to the parsed tokens returned by the parser's L<items()/GraphViz2::Marpa::Parser> method.
However, they are (necessarily) very similar.

If you provide an output file by using the 'lexed_file' option to L</new()>, or the L</lexed_file()> method, the file will have 2 columns, type and value.

E.g.: If the arrayref looks like:

	...
	{count => 10, name => '', type => 'open_bracket'   , value => '['},
	{count => 11, name => '', type => 'attribute_id'   , value => 'color'},
	{count => 12, name => '', type => 'equals'         , value => '='},
	{count => 13, name => '', type => 'attribute_value', value => 'red'},
	{count => 14, name => '', type => 'right_bracket'  , value => ']'},
	...

then the output file will look like:

	"type","value"
	...
	open_bracket    , "["
	attribute_id    , "color"
	equals          , "="
	attribute_value , "red"
	close_bracket   , "]"
	...

If you look at the source code for the run() method in L<GraphViz2::Marpa>, you'll see this arrayref can be
passed directly as the value of the 'tokens' key in the call to L<GraphViz2::Marpa::Parser>'s new().

Usage:

	my($lexer) = GraphViz2::Marpa::Lexer -> new(...);

	# $lexer -> items actually returns an object of type Set::Array.

	if ($lexer -> run == 0)
	{
		my(@items) = @{$lexer -> items};
	}

See also L</How is the lexed graph stored in RAM?> in the L</FAQ> below.
And see any data/*.lex file for sample data.

And now for a real graph:

Input: data/15.gv:

	digraph graph_15
	{
		node
		[
			shape = "record"
		]
		edge
		[
			color = "red"
			penwidth = 5
		]
		node_15_1
		[
			label = "<f0> left|<f1> middle|<f2> right"
		]
		node_15_2
		[
			label = "<f0> one|<f1> two"
		]
		node_15_1:f0 -> node_15_2:f1
		[
			arrowhead = "obox"
		]
	}

Output: data/15.lex:

	"type","value"
	strict              , "no"
	digraph             , "yes"
	graph_id            , "graph_15"
	start_scope         , "1"
	class_id            , "node"
	open_bracket        , "["
	attribute_id        , "shape"
	equals              , "="
	attribute_value     , "record"
	close_bracket       , "]"
	class_id            , "edge"
	open_bracket        , "["
	attribute_id        , "color"
	equals              , "="
	attribute_value     , "red"
	attribute_id        , "penwidth"
	equals              , "="
	attribute_value     , "5"
	close_bracket       , "]"
	node_id             , "node_15_1"
	open_bracket        , "["
	attribute_id        , "label"
	equals              , "="
	attribute_value     , "<f0> left|<f1> middle|<f2> right"
	close_bracket       , "]"
	node_id             , "node_15_2"
	open_bracket        , "["
	attribute_id        , "label"
	equals              , "="
	attribute_value     , "<f0> one|<f1> two"
	close_bracket       , "]"
	node_id             , "node_15_1"
	open_bracket        , "["
	attribute_id        , "port_id"
	equals              , "="
	attribute_value     , "f0"
	close_bracket       , "]"
	edge_id             , "->"
	node_id             , "node_15_2"
	open_bracket        , "["
	attribute_id        , "port_id"
	equals              , "="
	attribute_value     , "f1"
	attribute_id        , "arrowhead"
	equals              , "="
	attribute_value     , "obox"
	close_bracket       , "]"
	end_scope           , "1"

Note the pair:

	open_bracket        , "["
	...
	close_bracket       , "]"

They start and end each set of attributes, which are of 3 types:

=over 4

=item o Node

Node attributes can be specified both at the class (all subsequent nodes) level, or for a specific node.

Class:

	node
	[
		shape = "record" # Attribute.
	]

Node:

	node_15_1
	[
		label = "<f0> left|<f1> middle|<f2> right" # Attribute.
	]

Edge:

	node_15_1:f0 -> node_15_2:f1 # Attributes.
	[
		arrowhead = "obox"
	]

=item o Edge

Edge attributes can be specified both at the class level and after the second of 2 nodes on an edge.

	edge
	[
		color = "red" # Attribute.
		penwidth = 5  # Attribute.
	]

and

	node_15_1:f0 -> node_15_2:f1
	[
		arrowhead = "obox" # Attribute.
	]

=item o Port/compass point

These only ever occur for one or both of the 2 nodes on an edge, i.e. not at the class or node level:

	node_15_1:f0 -> node_15_2:f1 # Attributes.
	[
		arrowhead = "obox"
	]

=back

=head2 lexed_file([$lex_file_name])

Here, the [] indicate an optional parameter.

Get or set the name of the CSV file of lexed tokens to write. This file can be input to the parser.

'lexed_file' is a parameter to L</new()>. See L</Constructor and Initialization> for details.

=head2 log($level, $s)

Calls $self -> logger -> $level($s) if ($self -> logger).

=head2 logger([$logger_object])

Here, the [] indicate an optional parameter.

Get or set the logger object.

To disable logging, just set 'logger' to the empty string (not undef), in the call to L</new()>.

This logger is passed to L<GraphViz2::Marpa::Lexer::DFA>.

'logger' is a parameter to L</new()>. See L</Constructor and Initialization> for details.

=head2 maxlevel([$string])

Here, the [] indicate an optional parameter.

Get or set the value used by the logger object.

This option is only used if L<GraphViz2::Marpa:::Lexer> or L<GraphViz2::Marpa::Parser>
use or create an object of type L<Log::Handler>. See L<Log::Handler::Levels>.

'maxlevel' is a parameter to L</new()>. See L</Constructor and Initialization> for details.

=head2 minlevel([$string])

Here, the [] indicate an optional parameter.

Get or set the value used by the logger object.

This option is only used if L<GraphViz2::Marpa:::Lexer> or L<GraphViz2::Marpa::Parser>
use or create an object of type L<Log::Handler>. See L<Log::Handler::Levels>.

'minlevel' is a parameter to L</new()>. See L</Constructor and Initialization> for details.

=head2 new()

See L</Constructor and Initialization> for details on the parameters accepted by L</new()>.

=head2 report()

Log the list of items recognized by the DFA.

=head2 report_items([$Boolean])

The [] indicate an optional parameter.

Get or set the value which determines whether or not to log the items recognised by the lexer.

'report_items' is a parameter to L</new()>. See L</Constructor and Initialization> for details.

=head2 report_stt([$Boolean])

The [] indicate an optional parameter.

Get or set the value which determines whether or not to log the parsed state transition table (STT).

Calls L<Set::FA::Element/report()>. Set min and max log levels to 'info' for this.

'report_stt' is a parameter to L</new()>. See L</Constructor and Initialization> for details.

=head2 run()

This is the only method the caller needs to call. All parameters are supplied to L</new()> (or other methods).

Returns 0 for success and 1 for failure.

=head2 stt_file([$stt_file_name])

The [] indicate an optional parameter.

Get or set the name of the file containing the State Transition Table.

This option is used in conjunction with the 'type' option to L</new()>.

If the file name matches /csv$/, the value of the 'type' option is set to 'csv'.

'stt_file' is a parameter to L</new()>. See L</Constructor and Initialization> for details.

=head2 timeout($seconds)

The [] indicate an optional parameter.

Get or set the timeout for how long to run the DFA.

'timeout' is a parameter to L</new()>. See L</Constructor and Initialization> for details.

=head2 type([$type])

The [] indicate an optional parameter.

Get or set the value which determines what type of 'stt_file' is read.

'type' is a parameter to L</new()>. See L</Constructor and Initialization> for details.

=head2 utils([$aUtilsObject])

Here, the [] indicate an optional parameter.

Get or set the utils object.

Default: A object of type L<GraphViz2::Marpa::Utils>.

=head1 FAQ

=head2 Are the certain cases I should watch out for?

Yes. Consider these 3 situations and their corresponding lexed output:

=over 4

=item o digraph g {...}

	digraph     , "yes"
	graph_id    , "g"
	start_scope , "1"

=over 4

=item o The I<start_scope> count must be 1 because it's at the very start of the graph

=back

=item o subgraph s {...}

	start_subgraph  , "1"
	graph_id        , "s"
	start_scope     , "2"

=over 4

=item o The I<start_scope> count must be 2 or more

=item o When I<start_scope> is preceeded by I<graph_id>, it's a subgraph

=item o Given 'subgraph {...}', the I<graph_id> will be ""

=back

=item o {...}

	start_scope , "2"

=over 4

=item o The I<start_scope> count must be 2 or more

=item o When I<start_scope> is I<not> preceeded by I<graph_id>, it's a stand-alone {...}

=back

=back

=head2 Why doesn't the lexer/parser handle my HTML-style labels?

Traps for young players:

=over 4

=item o The <br /> component must include the '/'. <br align='center'> is not accepted by Graphviz

=item o The <br />'s attributes must use single quotes because output files use CSV with double quotes

=back

See data/38.* for good examples.

=head2 Where are the scripts documented?

In L<GraphViz2::Marpa/Scripts>.

=head2 Where is the State Transition Table?

I use data/default.stt.ods via LibreOffice, when editing the STT.

Then, I export it to data/default.stt.csv. This file is incorporated into the source code of Lexer.pm, after the __DATA__ token.

Lastly, I run scripts/stt2html.pl, and output the result to html/default.stt.html.

So I ship 3 representations of the STT in the distro.

When the lexer runs, the 'stt_file' and 'type' options to L</new()> default to reading the STT - using L<Data::Section::Simple>'s function get_data_section() - directly from __DATA__.

=head2 Where are the functions named in the STT?

In L<GraphViz2::Marpa::Lexer::DFA>.

=head2 How is the lexed graph stored in RAM?

Items are stored in an arrayref. This arrayref is available via the L</items()> method, which also has a
long explanation of this subject.

These items have the same format as the arrayref of items returned by the items() method in
L<GraphViz2::Marpa::Parser>, and the same as in L<GraphViz2::Marpa::Lexer::DFA>.

However, the precise values in the 'type' field of the following hashref vary between the lexer and the parser.

Each element in the array is a hashref:

	{
		count => $integer, # 1 .. N.
		name  => '',       # Unused.
		type  => $string,  # The type of the token.
		value => $value,   # The value from the input stream.
	}

$type => $value pairs used by the lexer are listed here in alphabetical order by $type:

=over 4

=item o attribute_id => $id

=item o attribute_value => $value

=item o class_id => /^edge|graph|node$/

This represents 3 special tokens where the author of the dot file used one or more of the 3 words
edge, graph, or node, to specify attributes which apply to all such cases. So:

	node [shape = Msquare]

means all nodes after this point in the input stream default to having an Msquare shape. Of course this
can be overidden by another such line, or by any specific node having a shape as part of its list of
attributes.

See data/51.* for sample code.

=item o close_bracket => ']'

This indicates the end of a set of attributes.

=item o digraph => $yes_no

'yes' => digraph and 'no' => graph.

=item o edge_id => $id

$id is either '->' for a digraph or '--' for a graph.

=item o end_scope => $brace_count

This indicates the end of the graph, the end of a subgraph, or the end of a stand-alone {...}.

$brace_count increments by 1 each time '{' is detected in the input string, and decrements each time '}' is detected.

=item o end_subgraph => $subgraph_count

This indicates the end of a subgraph, and follows the subgraph's 'end_scope'.

$subgraph_count increments by 1 each time 'subgraph' is detected in the input string, and decrements each time a matching '}' is detected.

=item o equals => '='

This separates 'attribute_id' from 'attribute_value'.

The parser does not output this token.

=item o graph_id => $id

This indicates both the graph's $id and each subgraph's $id.

For graphs and subgraphs, the $id may be '' (the empty string), and in a case such as:

	{
		rank = same
		A
		B
	}

The $id will definitely be ''.

See data/18.gv, data/19.gv, data/53.gv and data/55.gv.

=item o node_id => $id

=item o start_scope => $brace_count

This indicates the start of the graph, the start of a subgraph, or the start of a stand-alone {...}.

$brace_count increments by 1 each time '{' is detected in the input string, and decrements each time '}' is detected.

=item o open_bracket => '['

This indicates the start of a set of attributes.

=item o start_subgraph => $subgraph_count

This indicates the start of a subgraph, and preceeds the subgraph's 'graph_id'.

$subgraph_count increments by 1 each time 'subgraph' is detected in the input string, and decrements each time a matching '}' is detected.

=item o strict => $yes_no

'yes' => strict and 'no' => not strict.

=back

Consult data/*.gv and the corresponding data/*.lex for many examples.

=head2 How does the lexer handle comments?

See the next point.

=head2 What are the Limitations of the lexer?

=over 4

=item o Comments can be of the form m!^\s*(#|//)!

That is, Bash (Perl) and C++-style line-oriented comments are recognized, and the whole line is discarded.

This happens when the line is read in from a file, and so does not apply to the 'description' parameter to L<new()>.

=item o Comments can be of the form /* ... */

This is, C-style comments are recognized, and the comment is discaded.

This happens via the STT, and so applies to any source of input.

But, no attempt is made to ensure the '/*' and '*/' are not embedded in otherwise non-comment strings, so don't do that.

=item o What does this mean for trailing comments?

Simply that Bash and C++-style comments appearing on the ends of lines containing dot commands are I<not> handled. So, don't do that ether.

=item o Since comments are discarded, they will never appear in the output

This means that no output file, e.g. *.lex, *.parse or *.rend, will ever retain comments from the input *.gv file.

=item o Are there any dot files the lexer or parser cannot handle?

Perhaps. Perfection is an extra-cost option... The cost is unknown, but huge donations are welcome.

Actually, according to DOT's HTML-like label definition, L<http://www.graphviz.org/content/node-shapes#html>
you can use <...> instead of "..." to delimit text labels. The lexer as of V 1.02 does not handle this case.
That is, the code only recognizes HTML-like labels which are delimited with '<<' and '>>'.

=back

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

__DATA__

@@ stt

Start,Accept,State,Event,Next,Entry,Exit,Regexp,Interpretation
Yes,,initial,strict,graph,,save_prefix,"(?:""[^""]*""|<\s*<.*?>\s*>|[a-zA-Z_][a-zA-Z_0-9]*|-?(?:\.[0-9]+|[0-9]+(?:\.[0-9])*))",ID
,,,(?:graph|digraph),graph_id,,,":(?:""[^""]+""|<\s*<.*?>\s*>|[a-zA-Z_][a-zA-Z_0-9]*|-?(?:\.[0-9]+|[0-9]+(?:\.[0-9])*))",: + ID
,,,\/\*.*?\*\/,initial,,,\/\*.*?\*\/,
,,,\s+,initial,,,":(?:""[^""]+""|<\s*<.*?>\s*>|[a-zA-Z_][a-zA-Z_0-9]*|-?(?:\.[0-9]+|[0-9]+(?:\.[0-9])*)):(?:n|ne|e|se|s|sw|w|nw|c|_)(?![a-zA-Z_0-9])",: + ID + : + Compass point
,,,,,,,:(?:n|ne|e|se|s|sw|w|nw|c|_)(?![a-zA-Z_0-9]),: + Compass point
,,graph,(?:graph|digraph),graph_id,,save_prefix,(?:->|--),Edge
,,,\s+,graph,,,,
,,,,,,,,
,,graph_id,"(?:""[^""]*""|<\s*<.*?>\s*>|[a-zA-Z_][a-zA-Z_0-9]*|-?(?:\.[0-9]+|[0-9]+(?:\.[0-9])*))",start_scope,,save_graph_id,,
,,,{,statement_list_1,,,,
,,,\/\*.*?\*\/,graph_id,,,,
,,,\s+,graph_id,,,,
,,,,,,,,
,,start_scope,{,statement_list_1,,start_statements,,
,,,\/\*.*?\*\/,start_scope,,,,
,,,\s+,start_scope,,,,
,,,,,,,,
,,start_statement,{,statement_list_1,,start_statements,,
,,,\/\*.*?\*\/,start_statement,,,,
,,,\s+,start_statement,,,,
,,,,,,,,
,Yes,statement_list_1,subgraph,graph_id,,save_id_1,,
,,,"(?:""[^""]*""|<\s*<.*?>\s*>|[a-zA-Z_][a-zA-Z_0-9]*|-?(?:\.[0-9]+|[0-9]+(?:\.[0-9])*))",id_a,,,,
,,,(?:->|--),edge_id,,,,
,,,(?:\[),attribute_list,,,,
,,,{,statement_list_2,,,,
,,,},end_statement,,,,
,,,;,statement_list_1,,,,
,,,\/\*.*?\*\/,statement_list_1,,,,
,,,\s+,statement_list_1,,,,
,,,,,,,,
,Yes,statement_list_2,subgraph,graph_id,,save_id_1,,
,,,"(?:""[^""]*""|<\s*<.*?>\s*>|[a-zA-Z_][a-zA-Z_0-9]*|-?(?:\.[0-9]+|[0-9]+(?:\.[0-9])*))",id_a,,,,
,,,(?:->|--),edge_id,,,,
,,,(?:\[),attribute_list,,,,
,,,{,statement_list_1,,,,
,,,},end_statement,,,,
,,,;,statement_list_2,,,,
,,,\/\*.*?\*\/,statement_list_2,,,,
,,,\s+,statement_list_2,,,,
,,,,,,,,
,,id_a,subgraph,graph_id,,save_id_2,,
,,,:(?:n|ne|e|se|s|sw|w|nw|c|_)(?![a-zA-Z_0-9]),statement_list_1,,,,
,,,":(?:""[^""]+""|<\s*<.*?>\s*>|[a-zA-Z_][a-zA-Z_0-9]*|-?(?:\.[0-9]+|[0-9]+(?:\.[0-9])*)):(?:n|ne|e|se|s|sw|w|nw|c|_)(?![a-zA-Z_0-9])",statement_list_1,,,,
,,,":(?:""[^""]+""|<\s*<.*?>\s*>|[a-zA-Z_][a-zA-Z_0-9]*|-?(?:\.[0-9]+|[0-9]+(?:\.[0-9])*))",statement_list_1,,,,
,,,"(?:""[^""]*""|<\s*<.*?>\s*>|[a-zA-Z_][a-zA-Z_0-9]*|-?(?:\.[0-9]+|[0-9]+(?:\.[0-9])*))",id_b,,,,
,,,{,statement_list_1,,,A stand-alone {,
,,,},end_statement,,,,
,,,(?:->|--),id_b,,,,
,,,\[,attribute_list,,,,
,,,=,id_b,,,,
,,,;,id_a,,,,
,,,\/\*.*?\*\/,id_a,,,,
,,,\s+,id_a,,,,
,,,,,,,,
,,id_b,subgraph,graph_id,,save_id_2,,
,,,:(?:n|ne|e|se|s|sw|w|nw|c|_)(?![a-zA-Z_0-9]),statement_list_1,,,,
,,,":(?:""[^""]+""|<\s*<.*?>\s*>|[a-zA-Z_][a-zA-Z_0-9]*|-?(?:\.[0-9]+|[0-9]+(?:\.[0-9])*)):(?:n|ne|e|se|s|sw|w|nw|c|_)(?![a-zA-Z_0-9])",statement_list_1,,,,
,,,":(?:""[^""]+""|<\s*<.*?>\s*>|[a-zA-Z_][a-zA-Z_0-9]*|-?(?:\.[0-9]+|[0-9]+(?:\.[0-9])*))",statement_list_1,,,,
,,,"(?:""[^""]*""|<\s*<.*?>\s*>|[a-zA-Z_][a-zA-Z_0-9]*|-?(?:\.[0-9]+|[0-9]+(?:\.[0-9])*))",id_a,,,,
,,,{,statement_list_1,,,A stand-alone {,
,,,},end_statement,,,,
,,,(?:->|--),id_a,,,,
,,,\[,attribute_list,,,,
,,,=,id_a,,,,
,,,;,id_b,,,,
,,,\/\*.*?\*\/,id_b,,,,
,,,\s+,id_b,,,,
,,,,,,,,
,,edge_id,subgraph,id_a,,save_id_1,,
,,,"(?:""[^""]*""|<\s*<.*?>\s*>|[a-zA-Z_][a-zA-Z_0-9]*|-?(?:\.[0-9]+|[0-9]+(?:\.[0-9])*))",id_a,,,,
,,,(?:->|--),statement_list_1,,,,
,,,},end_statement,,,,
,,,\/\*.*?\*\/,edge_id,,,,
,,,\s+,edge_id,,,,
,,,,,,,,
,,attribute_list,"(?:""[^""]*""|<\s*<.*?>\s*>|[a-zA-Z_][a-zA-Z_0-9]*|-?(?:\.[0-9]+|[0-9]+(?:\.[0-9])*))",attribute_a,,save_attribute,,
,,,],statement_list_1,,,,
,,,},statement_list_1,,,,
,,,\/\*.*?\*\/,attribute_list,,,,
,,,\s+,attribute_list,,,,
,,,,,,,,
,,attribute_a,"(?:""[^""]*""|<\s*<.*?>\s*>|[a-zA-Z_][a-zA-Z_0-9]*|-?(?:\.[0-9]+|[0-9]+(?:\.[0-9])*))",attribute_b,,save_attribute,,
,,,=,attribute_b,,,,
,,,",",attribute_list,,,,
,,,],statement_list_1,,,,
,,,\/\*.*?\*\/,attribute_a,,,,
,,,\s+,attribute_a,,,,
,,,,,,,,
,,attribute_b,"(?:""[^""]*""|<\s*<.*?>\s*>|[a-zA-Z_][a-zA-Z_0-9]*|-?(?:\.[0-9]+|[0-9]+(?:\.[0-9])*))",attribute_a,,save_attribute,,
,,,=,attribute_a,,,,
,,,",",attribute_list,,,,
,,,],statement_list_1,,,,
,,,\/\*.*?\*\/,attribute_b,,,,
,,,\s+,attribute_b,,,,
,,,,,,,,
,Yes,end_statement,subgraph,graph_id,,save_id_1,,
,,,"(?:""[^""]*""|<\s*<.*?>\s*>|[a-zA-Z_][a-zA-Z_0-9]*|-?(?:\.[0-9]+|[0-9]+(?:\.[0-9])*))",id_a,,,,
,,,(?:->|--),edge_id,,,,
,,,\[,attribute_list,,,,
,,,{,statement_list_1,,,,
,,,},statement_list_1,,,,
,,,\/\*.*?\*\/,end_statement,,,,
,,,\s+,end_statement,,,,
