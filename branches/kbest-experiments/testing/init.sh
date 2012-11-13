#!/usr/bin/env bash

# Requires one command-line argument, which is the name of the test script 
# which we would like to create .out and .err files for.
# (i.e. a script which we assume is working correctly)

if [[ $# -ne 1 ]] ; then
        echo "Usage: $0 <script-name>"
        exit 1
fi

script=$1
basename=`basename $script .sh`
pushd .. >/dev/null
source testing/$script 1>testing/$basename.out 2>testing/$basename.err
popd >/dev/null

