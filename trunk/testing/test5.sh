
# Canonicalizes a kbest list by prepending a rank to each line (where lines 
# with equal weights have equal ranks) and then sorting numerically. This has 
# the effect of sorting each set of equally-weighted derivations, but leaving 
# relative ordering among non-equally-weighted derivations intact.
function canonicalize () {
    awk '{if ($1 != w) {w = $1; rank=NR} ; print rank, $0;}' | LC_COLLATE=C sort -n
}

./mcfg_nt grammars/wmcfg/larsonian1.wmcfg -kbest 23 -p "" | canonicalize
