#!/bin/bash

# Given a grammar and a full sentence, this script runs visualization of 
# the intersection of the grammar with each prefix of the sentence. 
# It produces (a) a pdf file for each prefix, and
#             (b) a tex file bringing together the tables of surfaces strings 
#                 at each prefix.

function print_usage_exit () {
        echo "Usage: $0 <mode> <grammar-file> <sentence> <number-of-trees>"
        echo "       where <mode> is either '-sample' or '-kbest'"
        exit 1
}

if [ $# -ne 4 ] ; then
        print_usage_exit
fi

mode=$1
grammar=$2
sentence=$3
num_trees=$4

if [ "$mode" != "-kbest" ] && [ "$mode" != "-sample" ] ; then
        print_usage_exit
fi

if [ ! -f $grammar ] ; then
        echo "File $grammar does not exist"
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


tables_file=`basename $grammar .wmcfg`.`no_spaces "$sentence"`.combined.$$.tex


echo "$sentence" | get_prefixes |\
while read prefix ; do
        id=/tmp/`basename $grammar .wmcfg`.`no_spaces "$prefix"`.$$
        ./mcfg_nt $grammar -intersect -p "$prefix" > $id.chart
        ./renormalize.csh $id.chart > $id.global.chart
        ./visualize $mode $id.global.chart $num_trees $id.tex $$ >/dev/null  # use $$, which also appears in output filenames, as random seed
        pdflatex $id.tex >/dev/null
        echo "*** Created pdf file: `basename $id`.pdf"
        cat $id.tex | get_tables >> $tables_file
        rm -f $id.chart $id.global.chart `basename $id`.aux `basename $id`.log
done

echo "============================"
echo "Tables for all prefixes are collected in $tables_file"
echo "Exiting `basename $0`"
echo "============================"
