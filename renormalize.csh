#!/bin/csh
# use Mathematica to renormalize an MCFG, construed as a list of weighted rewriting rules

#################################################################################

# As a default, leave MATHSCRIPT blank and rely on shebang line
set MATHSCRIPT=""

# See if we're at Cornell, and if so set MATHSCRIPT accordingly
hostname | grep -q '.compling.cornell.edu$'
if ( $? == 0 ) then
    set MATHSCRIPT="/usr/local/Wolfram/Mathematica/10.2/SystemFiles/Kernel/Binaries/Linux/WolframScript -script"
endif

# See if we're at UCLA, and if so set MATHSCRIPT accordingly
hostname | grep -q '.cdh.ucla.edu$'
if ( $? == 0 ) then
    set MATHSCRIPT="/usr/local/bin/MathematicaScript -script"
endif

#################################################################################

set RENORM=./renormalize.m

$MATHSCRIPT $RENORM $argv[1] | sed -f onespace.sed

