
# Takes an mcfg as input and produces a wmcfg that 
# distributes weight for each nonterminal uniformly 
# across all the rules that expand it.

{
    buffer[NR] = $0
    nonterm = $1
    if (!(nonterm in counters)) { counters[nonterm] = 0 }
    counters[nonterm] = counters[nonterm] + 1
}

END {
    for (k=1; k<=NR; k++) {
        line = buffer[k]
        split(line,fields)
        nonterm = fields[1]
        weight = "1 / " counters[nonterm] "    "
        print weight, line
    }
}
