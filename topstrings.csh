#!/bin/csh

set PARSE=./mcfg_nt
set GRAMMAR=$argv[1]
set PREFIX=`echo "$argv[2]" | sed 's/ /-/g'`
set RENORM=./renormalize.csh
set VISUAL=./visualize
set KBEST=$argv[3]
$PARSE grammars/wmcfg/$GRAMMAR.wmcfg -intersect -p "$argv[2]" >! $GRAMMAR.$PREFIX.chart
$RENORM $GRAMMAR.$PREFIX.chart >! $GRAMMAR.$PREFIX.global.chart
$VISUAL -kbest $GRAMMAR.$PREFIX.global.chart $KBEST $GRAMMAR.$PREFIX.global.tex
pdflatex $GRAMMAR.$PREFIX.global.tex
echo "*** Resulting pdf file is: $GRAMMAR.$PREFIX.global.pdf"
rm $GRAMMAR.$PREFIX.chart $GRAMMAR.$PREFIX.global.chart $GRAMMAR.$PREFIX.global.aux $GRAMMAR.$PREFIX.global.log 
rm $GRAMMAR.$PREFIX.global.tex
