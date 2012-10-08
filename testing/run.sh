#!/usr/bin/env bash

OUT=`mktemp /tmp/out.XXXX`
ERR=`mktemp /tmp/err.XXXX`

cd `dirname $0`

function run_test () {
        f=$1
        echo -en "$f : \t"
        pushd .. >/dev/null
        source testing/$f 1>$OUT 2>$ERR
        popd >/dev/null
        if (diff `basename $f .sh`.out $OUT >/dev/null) && (diff `basename $f .sh`.err $ERR >/dev/null) ; then
                echo "PASS!"
        else
                echo "FAIL!"
        fi
}

if [[ $# -eq 0 ]] ; then
        for f in test*.sh ; do
                run_test $f
        done
else
        for x in $@ ; do
                run_test $x
        done
fi

rm $OUT $ERR

