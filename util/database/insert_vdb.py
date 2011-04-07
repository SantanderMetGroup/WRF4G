#!/usr/bin/python
from optparse import OptionParser
from vdb import *

usage="""%prog [OPTIONS] TABLE PAIR

Example: %prog -v experiment id_experiment=nino97,UI_experiment=mon01.macc.unican.es

         INSERT INTO experiment (UI_experiment,id_experiment) VALUES ('mon01.macc.unican.es', 'nino99')
"""
parser = OptionParser(usage,version="%prog 1.0")
parser.add_option("-v", "--verbose",action="store_true", dest="verbose", default=False,help="Verbose mode. Explain what is being done")
   
(options, args) = parser.parse_args()

if len(args) < 2:
    parser.error("Incorrect number of arguments")
    exit(1)


table=args[0]


sent={}
for pair in args[1].split(',') :
   if options.verbose: print pair
   [field,value]=pair.split('=')
   sent[field]=value

a=vdb()
id=a.insert(table,sent,verbose=options.verbose)
print id
