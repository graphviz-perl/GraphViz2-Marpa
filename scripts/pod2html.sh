#!/bin/bash

pod2html.pl -i lib/GraphViz2/Marpa.pm                    -o /dev/shm/html/Perl-modules/html/GraphViz2/Marpa.html
pod2html.pl -i lib/GraphViz2/Marpa/Lexer.pm              -o /dev/shm/html/Perl-modules/html/GraphViz2/Marpa/Lexer.html
pod2html.pl -i lib/GraphViz2/Marpa/Lexer/DFA.pm          -o /dev/shm/html/Perl-modules/html/GraphViz2/Marpa/Lexer/DFA.html
pod2html.pl -i lib/GraphViz2/Marpa/Parser.pm             -o /dev/shm/html/Perl-modules/html/GraphViz2/Marpa/Parser.html
pod2html.pl -i lib/GraphViz2/Marpa/Renderer/GraphViz2.pm -o /dev/shm/html/Perl-modules/html/GraphViz2/Marpa/Renderer/GraphViz2.html
pod2html.pl -i lib/GraphViz2/Marpa/Utils.pm              -o /dev/shm/html/Perl-modules/html/GraphViz2/Marpa/Utils.html
