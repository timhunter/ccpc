#!/bin/csh

# use Mathematica to renormalize an MCFG, construed as a list of weighted rewriting rules
# the result flows out of renormalize.m's stdout
# and first has extraneous quotes remotes
# then twospace.sed converts EMPTY to "  "
# the perl command compensates for Mathematicing printing out newlines as the two-character sequence "backslash" then "n"

set RENORM=./renormalize.m
$RENORM $argv[1] | sed -f unquote.sed | sed -f twospaces.sed | perl -pe 's/\\n/\n/'
