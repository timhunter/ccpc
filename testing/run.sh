#!/usr/bin/env bash

OUT=`mktemp /tmp/out.XXXX`
ERR=`mktemp /tmp/err.XXXX`

for f in test*.sh ; do
        pushd .. >/dev/null
        source testing/$f 1>$OUT 2>$ERR
        popd >/dev/null
        echo -en "$f : \t"
        if (diff `basename $f .sh`.out $OUT >/dev/null) && (diff `basename $f .sh`.err $ERR >/dev/null) ; then
                echo "PASS!"
        else
                echo "FAIL!"
        fi
done

rm $OUT $ERR

