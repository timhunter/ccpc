#!/bin/csh

set MAKE=make
set TRAIN=./train
set LANGUAGE=$argv[1]
set TYPE=$argv[2]
$MAKE grammars/mcfgs/$LANGUAGE$TYPE.mcfg
$MAKE grammars/mcfgs/$LANGUAGE$TYPE.dict
#$MAKE grammars/wmcfg/$LANGUAGE$TYPE.wmcfg
$TRAIN grammars/mcfgs/$LANGUAGE$TYPE.mcfg $LANGUAGE.train >! grammars/wmcfg/$LANGUAGE$TYPE.wmcfg
