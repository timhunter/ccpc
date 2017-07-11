#!/usr/bin/env python

import sys
import re

def readGrammar(filename):
    lines = open(filename).readlines()
    dict = {}  # Will be a dictionary mapping a rule (as a string) to a weight (as a float)
    for line in lines:
        m = re.match("^ *(\d+) \/ *(\d+) *(.*)$", line)
        if m:
            w = float(m.group(1)) / float(m.group(2))
            rule = m.group(3).strip()
            assert (rule not in dict)
            dict[rule] = w
        else:
            print >>sys.stderr, "WARNING: ill-formed line '%s', ignoring it" % line.strip()
    return dict

def main(argv):
    try:
        file1, file2 = argv[1], argv[2]
    except IndexError:
        print >>sys.stderr, "Usage: %s <file1> <file2>" % argv[0]
        sys.exit(1)
    filename_width = max(len(file1), len(file2))
    dict1, dict2 = readGrammar(file1), readGrammar(file2)
    common_rules = [r for r in dict1.keys() if r in dict2.keys()]
    for rule in sorted(common_rules):
        w1, w2 = dict1[rule], dict2[rule]
        if w1 != w2:
            print "------------------------------------------"
            print "%-*s :\t%0.6f\t%s" % (filename_width, file1, w1, rule)
            print "%-*s :\t%0.6f\t%s" % (filename_width, file2, w2, rule)
            print "------------------------------------------"
    print "------------------------------------------"
    print "Extra rules only in %s" % file1
    for rule in sorted(dict1.keys()):
        if rule not in dict2.keys():
            print "%-*s :\t%s\t%s" % (filename_width, file1, dict1[rule], rule)
    print "------------------------------------------"
    print "------------------------------------------"
    print "Extra rules only in %s" % file2
    for rule in sorted(dict2.keys()):
        if rule not in dict1.keys():
            print "%-*s :\t%s\t%s" % (filename_width, file2, dict2[rule], rule)
    print "------------------------------------------"

if __name__ == "__main__":
    main(sys.argv)
