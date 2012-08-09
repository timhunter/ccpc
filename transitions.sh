#!/bin/bash

# Given a grammar and a full sentence, this script runs visualization of 
# the intersection of the grammar with each prefix of the sentence. 
# It produces (a) a pdf file for each prefix, and
#             (b) a tex file bringing together the tables of surfaces strings 
#                 at each prefix.

if [ $# -ne 3 ] ; then
        echo "Usage: $0 <grammar-file> <sentence> <number-of-trees>" ;
        exit 1
fi

grammar=$1
sentence=$2
num_trees=$3

if [ ! -f grammars/wmcfg/$grammar.wmcfg ] ; then
        echo "File grammars/wmcfg/$grammar.wmcfg does not exist"
        exit 1
fi

echo "============================"
echo "Starting `basename $0`"
echo "Grammar is: $grammar"
echo "Sentence is: $sentence"
echo "============================"

function get_prefixes () {
        awk '
                {
                        for (i=0; i<=NF; i++) {
                                prefix="";
                                for (j=1; j<=i; j++) {
                                        prefix = prefix " " $j;
                                }
                                sub(/^ /, "", prefix);
                                print prefix;
                        }
                }
        '
}

function get_tables () {
        awk '
                BEGIN { x=0 }
                {
                        if ($0 ~ /\\begin{table}/) {x=1;}
                        if (x==1) {print $0;}
                        if ($0 ~ /\\end{table}/) {x=0; print "";}
                }
        '
}

function no_spaces () {
        echo $1 | sed 's/ /-/g'
}


tables_file=$grammar.`no_spaces "$sentence"`.combined.$$.tex


echo "$sentence" | get_prefixes |\
while read prefix ; do
        id=/tmp/$grammar.`no_spaces "$prefix"`.$$
        ./mcfg_nt grammars/wmcfg/$grammar.wmcfg -intersect -p "$prefix" > $id.chart
        ./renormalize.csh $id.chart > $id.global.chart
        ./visualize -sample $id.global.chart $num_trees $id.tex $$ >/dev/null  # use $$, which also appears in output filenames, as random seed
        pdflatex $id.tex >/dev/null
        echo "*** Created pdf file: `basename $id`.pdf"
        cat $id.tex | get_tables >> $tables_file
        rm -f $id.chart $id.global.chart `basename $id`.aux `basename $id`.log
done

echo "============================"
echo "Tables for all prefixes are collected in $tables_file"
echo "Exiting `basename $0`"
echo "============================"
