#!/bin/bash

DEST=$DR/Perl-modules/html/GraphViz2

pod2html.pl -i lib/GraphViz2/Marpa.pm                    -o $DEST/Marpa.html
pod2html.pl -i lib/GraphViz2/Marpa/Config.pm             -o $DEST/Marpa/Config.html
pod2html.pl -i lib/GraphViz2/Marpa/Lexer.pm              -o $DEST/Marpa/Lexer.html
pod2html.pl -i lib/GraphViz2/Marpa/Parser.pm             -o $DEST/Marpa/Parser.html
pod2html.pl -i lib/GraphViz2/Marpa/Utils.pm              -o $DEST/Marpa/Utils.html
pod2html.pl -i lib/GraphViz2/Marpa/Lexer/DFA.pm          -o $DEST/Marpa/Lexer/DFA.html
pod2html.pl -i lib/GraphViz2/Marpa/Renderer/GraphViz2.pm -o $DEST/Marpa/Renderer/GraphViz2.html
