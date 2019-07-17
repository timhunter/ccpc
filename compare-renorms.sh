#!/bin/bash

CCPCDIR=`dirname $0`

function mathematica () {

    g=$1
    output=`mktemp /tmp/mathematica-output.XXXX`

    $CCPCDIR/renormalize.csh $g > $output
    probability=`gawk 'match($0, /\(\* "probability = ([^"]*)" \*\)/, a) {print a[1]}' $output`
    entropy=`gawk 'match($0, /\(\* "entropy = ([^"]*)" \*\)/, a) {print a[1]}' $output`
    echo "$probability $entropy"

    rm $output

}

function ocaml () {

    g=$1
    output1=`mktemp /tmp/ocaml-output1.XXXX`
    output3=`mktemp /tmp/ocaml-output3.XXXX`

    $CCPCDIR/renormalize -g $g 2>/dev/null > $output1
    $CCPCDIR/findentropy -g $output1 2>/dev/null > $output3
    probability=`gawk 'match($0, /\(\* "?probability = ([^"]*)"? \*\)/, a) {print a[1]}' $output1`
    entropy_straight=`gawk 'match($0, /\(\* "?entropy = ([^"]*)"? \*\)/, a) {print a[1]}' $output3`
    echo "$probability $entropy_straight"

    rm $output1 $output3

}

function mixed_entropy () {

    g=$1
    output=`mktemp /tmp/mixed-output.XXXX`

    $CCPCDIR/renormalize.csh $g | $CCPCDIR/findentropy -g /dev/stdin 2>/dev/null > $output
    entropy=`gawk 'match($0, /\(\* "?entropy = ([^"]*)"? \*\)/, a) {print a[1]}' $output`
    echo "$entropy"

    rm $output

}

function get_real_time () {
    awk '/^real/ {print $2}' $1
}

function get_best_word () {
    g=$1
    ./visualize -g $g -n 1 2>/dev/null | awk '{print $2}'
}

function check_grammar () {

    grammar=$1
    echo "Comparing results for $grammar"

    time_file=`mktemp /tmp/time_file.XXXX`

    cleaned_grammar=`mktemp /tmp/cleaned_grammar.XXXX`
    egrep -v '\(\* "?(probability|entropy) = .* \*\)' $grammar > $cleaned_grammar   # get rid of any existing values

    # Get values for the initial, non-intersected grammar
    read mp mh < <((time mathematica $cleaned_grammar) 2> $time_file)
    mt=`get_real_time $time_file`
    read op oh < <((time ocaml $cleaned_grammar) 2> $time_file)
    ot=`get_real_time $time_file`
    read xh < <(mixed_entropy $cleaned_grammar)

    # Now the intersection grammar
    int_grammar=`mktemp /tmp/intersection.XXXX`
    $CCPCDIR/intersect -g $cleaned_grammar -prefix "`get_best_word $cleaned_grammar`" 2>/dev/null > $int_grammar
    read mpi mhi < <((time mathematica $int_grammar) 2> $time_file)
    mti=`get_real_time $time_file`
    read opi ohi < <((time ocaml $int_grammar) 2>$time_file)
    oti=`get_real_time $time_file`
    read xhi < <(mixed_entropy $int_grammar)
    rm $int_grammar

    printf "  Mathematica: P = %-23s  H = %-23s  T = %-10s  P = %-23s  H = %-23s  T = %-10s\n" $mp $mh $mt $mpi $mhi $mti
    printf "        OCaml: P = %-23s  H = %-23s  T = %-10s  P = %-23s  H = %-23s  T = %-10s\n" $op $oh $ot $opi $ohi $oti
    printf "        Mixed:     %-23s  H = %-23s      %-10s      %-23s  H = %-23s      %-10s\n" "" $xh "" "" $xhi ""

    rm $time_file
    rm $cleaned_grammar

}

for f in $*; do
    check_grammar $f
done

