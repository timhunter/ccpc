#!/bin/csh

set PARSE=./mcfg_nt
set PREFIX=`echo "$argv[1]" | sed 's/ /-/g'`
set RENORM=./renormalize.csh
set VISUAL=./visualize
$PARSE grammars/wmcfg/korean.wmcfg -p "$argv[1]" >! korean.$PREFIX.chart
$RENORM korean.$PREFIX.chart >! korean.$PREFIX.renorm.global.chart
$VISUAL korean.$PREFIX.renorm.global.chart 100
pdflatex trees
