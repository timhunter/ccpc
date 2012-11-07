#!/usr/bin/env bash

OUT=`mktemp /tmp/out.XXXX`
ERR=`mktemp /tmp/err.XXXX`

# Make sure we're in the testing directory
cd `dirname $0`

# Check whether make is up to date
pushd .. >/dev/null
make -q
if [ $? -ne 0 ] ; then
        echo "WARNING: make is not up to date" >&2
fi
popd >/dev/null

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
                if [ -f $x ] ; then
                        run_test $x
                else
                        echo "WARNING: Test script $x doesn't exist, skipping it" >&2
                fi
        done
fi

rm $OUT $ERR

