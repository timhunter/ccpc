#!/usr/bin/env python

import sys
import re
import os

def readDictionary(filename):
    lines = open(filename).readlines()
    dict = {'S':'S'}  # Will be a dictionary mapping a nonterminal (e.g. "t123") to an MG feature-sequence (e.g. "(:: =N D -f)")
    for line in lines:
        if line.strip() == "": continue
        m = re.match("^([a-z]+[0-9]+) : (\(::? .*\))$", line)
        if m:
            nonterminal = m.group(1)
            features = m.group(2)
            dict[nonterminal] = features
        else:
            print >>sys.stderr, "WARNING: ill-formed dictionary line '%s', ignoring it" % line.strip()
    return dict

def showRule(rule, dict):
    if re.match("t\d+ --> E t\d+_tmp2 .*", rule):
        return None
    elif re.match("t\d+_tmp2 --> t\d+_tmp1 E .*", rule):
        return None
    elif re.match("E --> \" ?\"", rule):
        return None
    else:
        m = re.match("(t\d+)_tmp1 --> (\"[^\"]*\") *", rule)
        if m:
            return "%s --> %s" % (dict[m.group(1)], m.group(2))
        else:
            m = re.match("(\w+) --> (\w+ ?(\w+)?) .*", rule)
            if m:
                lhs = dict[m.group(1)]
                rhs = " ".join([dict[x] for x in m.group(2).split()])
                return "%s --> %s" % (lhs,rhs)
            else:
                print >>sys.stderr, "WARNING: Couldn't figure out what to do with this rule:", rule
                return None

def showGrammar(filename, dict):
    lines = open(filename).readlines()
    result = []
    for line in lines:
        m = re.match("^((\d+) \/ (\d+))? *(.*)$", line)
        if m:
            if m.group(2) is not None:
                num, denom = int(m.group(2)), int(m.group(3))
                weightString = "%6d / %6d" % (num, denom)
            else:
                num, denom = None, None
                weightString = ""
            rule = showRule(m.group(4), dict)
            if rule is not None:
                result.insert(0, "%s          %s" % (weightString, rule))
        else:
            print >>sys.stderr, "WARNING: ill-formed grammar line '%s', ignoring it" % line.strip()
    ruleWithoutWeight = lambda s : re.sub("^[\d /]*", "", s)      # " ".join(s.split()[3:])
    result.sort(key=ruleWithoutWeight)
    for x in result:
        print x

def main(argv):
    def usage():
        print >>sys.stderr, "Usage: %s <grammar-file> <dict-file>" % argv[0]
        sys.exit(1)
    try:
        grammar_file = argv[1]
    except IndexError:
        usage()
    try:
        dict_file = argv[2]
    except IndexError:
        dict_attempt = re.sub("\.mcfg$", ".dict", grammar_file)
        if dict_attempt != grammar_file and os.path.isfile(dict_attempt):
            print >>sys.stderr, "Using dict file", dict_attempt
            dict_file = dict_attempt
        else:
            usage()
    dict = readDictionary(dict_file)
    showGrammar(grammar_file, dict)

if __name__ == "__main__":
    main(sys.argv)

