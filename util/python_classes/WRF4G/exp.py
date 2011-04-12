#!/usr/bin/python

from sys import stderr,exit, path
path.append('/home/valva/WRF4G/util/python_classes')
import WRF4G.Element
import WRF4G.utils
import os
import rlcompleter
import readline
from re import search
from optparse import OptionParser
readline.parse_and_bind("tab: complete")


class exp(WRF4G.Element.Element):
    
    """  Wrf4gElement CLASS
    """
    def print_hola(self):
        print "hola"
 

if __name__ == "__main__":
    usage="""%prog [OPTIONS] exp_values function fvalues 
             Example: %prog 
    """
        
    parser = OptionParser(usage,version="%prog 1.0")
    parser.add_option("-v", "--verbose",action="store_true", dest="verbose", default=False,help="Verbose mode. Explain what is being done")
     
    (options, args) = parser.parse_args()
    
    if len(args) < 1:
        parser.error("Incorrect number of arguments")
        exit(1)
      
    function=args[0]

    if len(args) > 1:   exp_values=args[1]
    elif len(args) > 2:   fvalues=args[2:]
        
    if len(args) == 1 and function == "get_all_fields":
        exp=exp(data='',verbose=options.verbose)
        output=getattr(exp,function)()
        print ','.join(output)

        
    else:
        exp=exp(database.utils.pairs2dict(exp_values),options.verbose)
        output=getattr(exp,function)(fvalues)   
        print output





   
