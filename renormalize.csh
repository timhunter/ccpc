#!/bin/csh
# use Mathematica to renormalize an MCFG, construed as a list of weighted rewriting rules

set RENORM=./renormalize.m
$RENORM $argv[1] | sed -f onespace.sed
