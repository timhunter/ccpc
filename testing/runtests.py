#!/usr/bin/python
from xml.parsers.expat import ExpatError
from xml.etree import ElementTree
import os, sys, tempfile, subprocess

def print_usage():
  print "Usage: runtests.py configuration_file"
  print "configuration_file formatted in XML as follows:"
  print "<configuration>\n<parser>\n\t<path>parser_path</path>\n\t<configuration>[arg1,arg2,...] GRAMMAR [arg1,arg2,...] SENTENCE [arg1,arg2,...]</configuration>\n\t<pass>pass_string</pass>\n\t<fail>fail_string</fail>\n</parser>\n<grammarmodifier>\n\t<path>grammarmodifier_path</path>\n\t<configuration>[arg1,arg2,...] GRAMMAR [arg1,arg2,...] OUT [arg1,arg2,...]</configuration>\n</grammarmodifier>\n<testcases>\n\t<test>\n\t\t<grammar>grammar.mcfg</grammar>\n\t\t<sentences>\n\t\t\t<sentence>\"sentence 1 goes here\"</sentence>\n\t\t\t<sentence>\"sentence 2 goes here\"</sentence>\n\t\t</sentences>\n\t</test>\n</testcases>\n</configuration>"
  exit(0)

def xmlparse(filename):
  testcases = []
  etree = ElementTree.parse(filename)
  parser_path = etree.find('parser').find('path').text
  parser_configstr = etree.find('parser').find('configuration').text
  parser_pass = etree.find('parser').find('pass').text
  parser_fail = etree.find('parser').find('fail').text
  modifier = etree.find('grammarmodifier')
  modifier_path = None
  modifier_configstr = None
  if modifier != None:
    modifier_path = etree.find('grammarmodifier').find('path').text
    modifier_configstr = etree.find('grammarmodifier').find('configuration').text
  testcases = []
  for test in etree.find('testcases').findall('test'):
    grammar = test.find('grammar').text
    sentences = map(lambda s: s.text, test.find('sentences').findall('sentence'))
    testcases.append((grammar, sentences))
  return parser_path, parser_configstr, modifier_path, modifier_configstr, testcases, parser_pass, parser_fail

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

def format_modifier_cmd(path,configstr):
  if configstr.find('GRAMMAR') == -1 or configstr.find('OUT') == -1 :
    print 'grammarmodifier configuration malformed (must contain the strings GRAMMAR and OUT)'
    exit(0)
  lst = configstr.split('GRAMMAR')
  lst2 = lst[1].split('OUT')
  first = lst[0]
  second = lst2[0]
  if len(lst2)==2:
    third = lst2[1]
  else:
    third = ''
  return lambda g,ng: path + ' ' + first + g + second + ng + third

def run(cmd, shell_val=True):
  return subprocess.Popen([cmd], shell=shell_val, stderr=subprocess.STDOUT, \
  stdout=subprocess.PIPE).communicate()[0]

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
runModifier = False
if configuration[3] != None:
  runModifier = True
  modifier_cmd = format_modifier_cmd(configuration[2],configuration[3]) # takes in grammar
if not os.path.exists(configuration[0]):
  print 'invalid path to parser'
  exit(0)
if runModifier and not os.path.exists(configuration[2]):
  print 'invalid path to grammarmodifier'
  exit(0)

testcases = configuration[4]
results = []
modified_results = []
for grammar, sentences in testcases:
  if runModifier:
    modgrammar = tempfile.mktemp()
    out = run(modifier_cmd(grammar, modgrammar))
    if out != "":
      print out
  for sentence in sentences:
    result = check(run(parser_cmd(grammar,sentence)), configuration[5], configuration[6])
    results.append((grammar, sentence, result))
    if runModifier:
      result = check(run(parser_cmd(modgrammar,sentence)), configuration[5], configuration[6])
      modified_results.append((grammar, sentence, result))
  if runModifier and os.path.exists(modgrammar):
    os.unlink(modgrammar)

print 'TEST RESULTS:'
if runModifier:
  for i in range(len(results)):
    (g,s,r) = results[i]
    (mg,ms,mr) = modified_results[i]
    print g
    print s
    print 'Recognized With Grammar: '+str(r)+'\tRecognized With Modified Grammar: '+str(mr)
    print '-------------------------------------------------------------------------'
else:
  for (g,s,r) in results:
    print g
    print s
    print 'Recognized: ' + r
    print '---------------------------------'
