#!/bin/bash
#
# Runs the English PCFG parser on one or more files, printing trees only
#
scriptdir="/home/del82/projects/mcfgcky/stanford-parser-2010-11-30"
java -mx150m -cp "$scriptdir/stanford-parser.jar:$scriptdir/*:" edu.stanford.nlp.parser.lexparser.LexicalizedParser -outputFormat "penn" $scriptdir/englishPCFG.ser.gz $*
