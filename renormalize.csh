#!/bin/csh

# This script accepts a WMCFG file as a command line argument,
# and prints the renormalized version of this grammar to stdout.

# First: replicate any line in the input file that specifies an original 
# (pre-intersection) grammar file. This info is used to reconstruct the MG 
# derivation corresponding to each MCFG derivation.
#grep '^(\* original grammar: [a-zA-Z0-9\/\.]* \*)' $argv[1]

# use Mathematica to renormalize an MCFG, construed as a list of weighted rewriting rules
# the result flows out of renormalize.m's stdout
# and first has extraneous quotes removed
# then the weird backslash-quotes surrounding terminals are changes to just quotes
# then twospace.sed converts EMPTY to "  "
# the perl command compensates for Mathematicing printing out newlines as the two-character sequence "backslash" then "n"
# (only relevant for the comments)

set RENORM=./renormalize.m
$RENORM $argv[1] | sed -f unquote.sed | sed -e 's/\\"/\"/g'|  sed -f twospaces.sed | perl -pe 's/\\n/\n/g'
