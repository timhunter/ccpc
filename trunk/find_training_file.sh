#!/usr/bin/env bash

# Given the full path of an mcfg file, this script will determine the 
# best-matching corresponding training corpus. We basically look for a 
# *.train file named with the longest possible prefix of the original 
# mcfg file's basename. 
# For example, given foobar.mcfg, we'll try foobar.train, fooba.train, 
# foob.train, etc., until we find a file that exists.

function print_usage_exit () {
    echo "Usage: $0 <mcfg-file>" >&2 ;
    exit 1
}

if [ $# -ne 1 ] ; then
    print_usage_exit
fi

mcfg_file=$1

if [ ! -f $mcfg_file ] ; then
    echo "File $mcfg_file does not exist" >&2 ;
    exit 1
fi
if (! echo $mcfg_file | grep '\.mcfg$' >/dev/null) ; then
    echo "File $mcfg_file does not have .mcfg extension" >&2 ;
    exit 1
fi
if (! dirname $mcfg_file | grep '[/^]mcfgs$' >/dev/null) ; then
    echo "File $mcfg_file is not in a directory called 'mcfgs'" >&2 ;
    exit 1
fi

dir=`dirname $mcfg_file | sed 's/mcfgs$/train/'`
base=`basename $mcfg_file .mcfg`

prefixes=`echo $base | awk '{ for (i=length($0); i>0; i--) print substr($0,1,i); }'`

for p in $prefixes; do
    candidate=$dir/$p.train
    if [ -f $candidate ] ; then
        result=$candidate ;
        break
    fi
done

# test if $result is empty
if [ -z "$result" ] ; then
    echo "*** `basename $0`: No training file found for grammar $mcfg_file" >&2 ;
else
    echo "*** `basename $0`: Using $result as training file for grammar $mcfg_file" >&2 ;
    echo $result
fi

