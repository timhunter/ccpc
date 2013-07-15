#!/bin/bash

# this script works on Mac OS X
# case 139692: one of my coworkers...tells me that for some reason the Mathematica 9 kernel does not write to standard out.
# Harry Calkins, Support Engineer, Wolfram Technology Group, Wolfram Research, Inc

i=0 

echo "garbage" |     # this pipe seems to be crucial
while [ $i -lt 1 ] ; do
   ./renormalize.csh $1 | sed -f unquote.sed | sed -e 's/\\"/\"/g'|  sed -f twospaces.sed | perl -pe 's/\\n/\n/g'
   (( i++ ))
done

