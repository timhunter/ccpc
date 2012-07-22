#!/bin/csh

set MAKE=make
set GRAMMAR=$argv[1]
$MAKE grammars/mcfgs/$GRAMMAR.mcfg
$MAKE grammars/mcfgs/$GRAMMAR.dict
$MAKE grammars/wmcfg/$GRAMMAR.wmcfg
