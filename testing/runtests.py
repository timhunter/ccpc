#!/usr/bin/python
from xml.parsers.expat import ExpatError
from xml.etree import ElementTree
from time import time
import os, sys, subprocess

def print_usage():
  print "Usage: runtests.py configuration_file.xml"
  print "configuration_file formatted in XML as follows:"
  print "<configuration>\n<parser>\n\t<path>parser_path</path>\n\t<configuration>[arg1,arg2,...] GRAMMAR [arg1,arg2,...] SENTENCE [arg1,arg2,...]</configuration>\n\t<pass>pass_string</pass>\n\t<fail>fail_string</fail>\n</parser>\n<testcases>\n\t<test>\n\t\t<grammar>grammar.mcfg</grammar>\n\t\t<sentences>\n\t\t\t<sentence>\"sentence 1 goes here\"</sentence>\n\t\t\t<sentence>\"sentence 2 goes here\"</sentence>\n\t\t</sentences>\n\t</test>\n</testcases>\n</configuration>"
  exit(0)

def xmlparse(filename):
  testcases = []
  etree = ElementTree.parse(filename)
  parser_path = etree.find('parser').find('path').text
  parser_configstr = etree.find('parser').find('configuration').text
  parser_pass = etree.find('parser').find('pass').text
  parser_fail = etree.find('parser').find('fail').text
  testcases = []
  for test in etree.find('testcases').findall('test'):
    grammar = test.find('grammar').text
    sentences = map(lambda s: s.text, test.find('sentences').findall('sentence'))
    testcases.append((grammar, sentences))
  return parser_path, parser_configstr, parser_pass, parser_fail, testcases

def format_parser_cmd(parser_path, parser_configstr):
  if parser_configstr.find('GRAMMAR') == -1 or parser_configstr.find('SENTENCE') == -1 :
    print 'parser configuration malformed (must contain the strings GRAMMAR and SENTENCE)'
    exit(0)
  lst = parser_configstr.split('GRAMMAR')
  lst2 = lst[1].split('SENTENCE')
  first = lst[0]
  second = lst2[0]
  if len(lst2)==2:
    third = lst2[1]
  else:
    third = ''
  return lambda g,s: parser_path + ' ' + first + g + second + s + third

def run(cmd, shell_val=True):
  start = time()
  out = subprocess.Popen([cmd], shell=shell_val, stderr=subprocess.STDOUT, \
  stdout=subprocess.PIPE).communicate()[0]
  runtime = time() - start
  return (out,runtime)

def check(output, parser_pass, parser_fail):
   if output.find(parser_pass) != -1:
      return "PASS"
   elif output.find(parser_fail) != -1:
      return "FAIL"
   else:
      print output
      print 'Error: parser neither passes nor fails! check xml file.'
      exit(-1)

if len(sys.argv) != 2:
  print_usage()
else:
  configfile = sys.argv[1]
try:
  configuration = xmlparse(configfile)
except IOError:
  print 'configuration_file (' + configfile + ') does not exist\n'
  print_usage()
except ExpatError:
  print 'configuration_file (' + configfile + ') is not properly formed\n'
  print_usage()

parser_cmd = format_parser_cmd(configuration[0],configuration[1]) # takes in grammar and sentence
if not os.path.exists(configuration[0]):
  print 'invalid path to parser'
  exit(0)

testcases = configuration[4]
parser_pass = configuration[2]
parser_fail = configuration[3]
results = []
for grammar, sentences in testcases:
  for sentence in sentences:
    out,runtime = run(parser_cmd(grammar,sentence))
    result = check(out, parser_pass, parser_fail)
    results.append((grammar, sentence, result, runtime))

print 'TEST RESULTS:'
for (g,s,r,t) in results:
  print g
  print s
  print 'Recognized: %s' % r
  print 'Parse time: %f' % t
  print '---------------------------------'
