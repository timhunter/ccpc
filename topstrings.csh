#!/bin/csh

set PARSE=./mcfg_nt
set GRAMMAR=$argv[1]
set PREFIX=`echo "$argv[2]" | sed 's/ /-/g'`
set RENORM=./renormalize.csh
set VISUAL=./visualize
$PARSE grammars/wmcfg/$argv[1].wmcfg -p "$argv[2]" >! $argv[1].$PREFIX.chart
$RENORM $argv[1].$PREFIX.chart >! korean.$PREFIX.renorm.global.chart
$VISUAL $argv[1].$PREFIX.renorm.global.chart 100
pdflatex trees
