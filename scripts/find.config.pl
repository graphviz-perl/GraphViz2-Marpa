#!/usr/bin/env perl

use strict;
use warnings;

use File::ShareDir;
File::ShareDir::ProjectDistDir
# --------------

my($app_name)    = 'GraphViz2-Marpa';
my($config_name) = '.htgraphviz2.marpa.conf';
my($path)        = File::ShareDir::dist_file($app_name, $config_name);

print "Using: File::ShareDir::dist_file('$app_name', '$config_name'): \n";
print "Found: $path\n";
