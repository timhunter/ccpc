#!/bin/tcsh

set anyword = '\".*\"'
# this sed command requires modern REs, ie use the -E flag
# use like this
# sed -E -f unlexicalize2.sed /tmp/prefix7-2.chart | sort -k4 | uniq
echo "s/[0-9]+ \/ ([0-9]+)[ ]*(t([0-9]+)_tmp1_${1}-${1}) --> $anyword/\1 \/ \1 \2  --> terminal\3/" > unlexicalize${1}.sed

