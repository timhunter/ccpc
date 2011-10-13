#!/bin/csh

# use Mathematica to renormalize an MCFG, construed as a list of weighted rewriting rules
# the result flows out of renormalize.m's stdout
# and first has extraneous quotes removed
# then the weird backslash-quotes surrounding terminals are changes to just quotes
# then twospace.sed converts EMPTY to "  "
# the perl command compensates for Mathematicing printing out newlines as the two-character sequence "backslash" then "n"
# (only relevant for the comments)


set RENORM=./locallyrenormalize.m
$RENORM $argv[1] | sed -f unquote.sed | sed -e 's/\\"/\"/g'|  sed -f twospaces.sed | perl -pe 's/\\n/\n/'
