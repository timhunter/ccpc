#!/bin/csh -v

set PARSE=./mcfg_nt
set GRAMMAR=$argv[1]
set PREFIX=`echo "$argv[2]" | sed 's/ /-/g'`
set RENORM=./renormalize.csh
set VISUAL=./visualize
$PARSE grammars/wmcfg/$GRAMMAR.wmcfg -p "$argv[2]" >! $GRAMMAR.$PREFIX.chart
$RENORM $GRAMMAR.$PREFIX.chart >! $GRAMMAR.$PREFIX.global.chart
$VISUAL $GRAMMAR.$PREFIX.global.chart 100
sed -i 's/_/\\_/g' trees.tex   #This is a hack to deal with unescaped underscores (sweiss)
pdflatex trees
mv trees.pdf $GRAMMAR.$PREFIX.global.pdf
