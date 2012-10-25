#!/usr/bin/env perl

use strict;
use warnings;

use Date::Format; # For time2str.

use File::Spec;

use GraphViz2::Marpa;
use GraphViz2::Marpa::Utils;

use HTML::Entities::Interpolate;

use Perl6::Slurp;

use Text::Xslate 'mark_raw';

# -----------

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

my($templater) = Text::Xslate -> new
(
	input_layer => '',
	path        => File::Spec -> catdir('htdocs', 'assets', 'templates', 'graphviz2', 'marpa'),
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
	 date_stamp => time2str('%Y-%m-%d %T', time),
	 version    => $GraphViz2::Marpa::VERSION,
 }
);
my($file_name) = File::Spec -> catfile($html_dir_name, 'index.html');

open(OUT, '>', $file_name);
print OUT $index;
close OUT;

print "Wrote: $file_name. \n";
