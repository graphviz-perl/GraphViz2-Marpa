#!/bin/bash
#
# Name: rend.1.sh.
#
# Calls: rend.sh.
#
# Parameters:
# 1: The abbreviated name of sample input and output data files.
#    E.g. 19 simultaneously means data/19.parse and data/19.rend.
# 2 & 3: Use for debugging. E.g.: -maxlevel debug.

scripts/rend.sh data/$1.parse data/$1.rend $2 $3
