#!/bin/bash

pod2html.pl -i lib/GraphViz2/Marpa.pm                    -o ~/savage.net.au/Perl-modules/html/GraphViz2/Marpa.html
pod2html.pl -i lib/GraphViz2/Marpa/Lexer.pm              -o ~/savage.net.au/Perl-modules/html/GraphViz2/Marpa/Lexer.html
pod2html.pl -i lib/GraphViz2/Marpa/Parser.pm             -o ~/savage.net.au/Perl-modules/html/GraphViz2/Marpa/Parser.html
pod2html.pl -i lib/GraphViz2/Marpa/Utils.pm              -o ~/savage.net.au/Perl-modules/html/GraphViz2/Marpa/Utils.html
pod2html.pl -i lib/GraphViz2/Marpa/Lexer/DFA.pm          -o ~/savage.net.au/Perl-modules/html/GraphViz2/Marpa/Lexer/DFA.html
pod2html.pl -i lib/GraphViz2/Marpa/Renderer/GraphViz2.pm -o ~/savage.net.au/Perl-modules/html/GraphViz2/Marpa/Renderer/GraphViz2.html

cp -r  ~/savage.net.au/Perl-modules/html/GraphViz2/* $DR/Perl-modules/html/GraphViz2