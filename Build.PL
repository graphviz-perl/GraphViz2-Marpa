use Module::Build;

if (open my $fh, '|dot', )
{
	close $fh;
}
else
{
	die "Please install Graphviz from http://www.graphviz.org/.\n";
}

Module::Build -> new
(
	module_name    => 'GraphViz2::Marpa',
	license        => 'artistic_2',
	dist_abstract  => 'A Perl lexer and parser for Graphviz dot files',
	dist_author    => 'Ron Savage <ron@savage.net.au>',
	build_requires =>
	{
		Test::Simple=> 0.98,
		Test::Pod  => 1.45,
	},
	configure_requires =>
	{
		Module::Build => 0.3800,
	},
	requires =>
	{
		Algorithm::Diff             => 1.1902,
		Config                      => undef,
		Config::Tiny                => 2.14,
		Data::Section::Simple       => 0.02,
		Date::Format                => 2.24,
		Date::Simple                => 3.03,
		feature                     => 0,
		File::Copy                  => 2.21,
		File::HomeDir               => 0.99,
		File::Slurp                 => 9999.19,
		File::Spec                  => 3.33,
		File::Temp                  => 0.22,
		Getopt::Long                => 2.38,
		GraphViz2                   => 1.09,
		GraphViz2::Parse::Marpa     => 1.12,
		Hash::FieldHash             => 0.10,
		HTML::Entities::Interpolate => 1.04,
		List::Compare               => 0.37,
		Log::Handler                => 0.71,
		Marpa::R2                   => 2.022000,
		Module::Path                => 0.08,
		Path::Class                 => 0.24,
		perl                        => '5.10.0', # For given/when.
		Perl6::Slurp                => 0.03,
		Pod::Usage                  => 1.36,
		Set::Array                  => 0.23,
		Set::FA::Element            => 1.08,
		strict                      => 0,
		Text::CSV                   => 1.21,
		Text::CSV::Slurp            => 1,
		Text::Xslate                => 1.4001,
		Tree::DAG_Node              => 1.07,
		Try::Tiny                   => 0.09,
		warnings                    => 0,
	},
) -> create_build_script();
