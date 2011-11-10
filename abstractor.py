#!/usr/bin/python

import string
import sys

def abstractify(grammarFile, outputFile):
    grammar = open(grammarFile).readlines()
    items = set()
    rules = set()
    for line in grammar:
        for item in line.split('.'):
            if item.find("::") != -1:
                pieces = item.split('::')
                if string.strip(pieces[0]) != "[]":
                    name = (string.strip(pieces[1]))
                    if name not in items:
                        items.add(name)
                        name = string.translate(name, None, "',[]")
                        rule = '['+name+']'+'::'+pieces[1]
                        rules.add(rule)
                else:
                    rule = string.strip(item)
                    rules.add(item)
            else:
                rule = string.strip(item)
                if rule.find("startCategory") != -1 or rule.find('[') != -1 and rule.find('%') == -1:
                    rules.add(item)
    output = open(outputFile, 'w')
    for rule in rules:
        output.write(rule+".\n")


def createMap(grammar, absG):
    rules = dict()
    for line in grammar:
        for item in line.split('.'):
            if item.find('::') != -1:
                pieces = item.split('::')
                terminal = string.translate(pieces[0], None, "[]")
                terminal = string.strip(terminal)
                if terminal != '':
                    if absG:
                        rules[pieces[1]] = terminal
                    else:
                        terminal = string.translate(terminal, None, '\'')
                        rules[terminal] = pieces[1]
    return rules

def absSentences(grammarFile, absGrammarFile, sentenceFile, outputFile):
    grammar = open(grammarFile).readlines()
    absGrammar = open(absGrammarFile).readlines()
    sentences = open(sentenceFile).readlines()
    output = open(outputFile, 'w')
    grammarMap = createMap(grammar, False)
    absMap = createMap(absGrammar, True)
    
    for line in sentences:
        absSent = ''
        splitLine = line.split(' ')
        for word in splitLine[1:]:
            word = string.translate(word, None, '\n')
            category = string.strip(grammarMap[word], ' ')
            absTerminal = absMap[category]
            absSent = absSent + absTerminal + ' '             
        output.write(splitLine[0] + ' ' + absSent + "\n")

def main():
    if sys.argv[1] == "-g":
        abstractify(sys.argv[2], sys.argv[3])
    if sys.argv[1] == "-s":
        absSentences(sys.argv[2], sys.argv[3], sys.argv[4], sys.argv[5])
                
if __name__ == '__main__':
    main()
