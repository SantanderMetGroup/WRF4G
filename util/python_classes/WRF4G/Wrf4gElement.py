#!/usr/bin/python

from sys import stderr,exit, path
path.append("/home/valva/WRF4G/util/database") 
import vdb
import os
import rlcompleter
import readline
from re import search
from optparse import OptionParser
readline.parse_and_bind("tab: complete")



class Wrf4gElement:
    """  Wrf4gElement CLASS
    """
 
    def __init__(self,data,verbose='no'):
        self.verbose=verbose
        self.element=self.__class__.__name__
        self.element='exp'
        self.data=data
        self.allfields=['','','','']        
        #for field in data.keys():            
        #    setattr(self,field,data[field])
            
    def get_all_fields(self):
        dbc=vdb.vdb()
        salida=dbc.describe(self.element)
        return salida
    
    def get_id(self,fields):
        """    
        Query database to check if the experiment exists
        Returns:
        -1 --> not exists
        exp.id --> exists with the same parameters 
        """
        wheresta=''
        dbc=vdb.vdb()
        for field in fields:
            wheresta="%s AND %s='%s'" %(wheresta,field,self.data[field])
        wheresta=wheresta[4:]
        
        idp=dbc.select(self.element,'id',wheresta,verbose=self.verbose)
        id = vdb.list_query().one_field(idp)
        if id !='': return id
        else: return -1
    
    def get_inmutable_fields(self):
        pass
    
    def get_reconfigure_fields(self):
        pass
    
    def loadfromDB(self,fields):
     """    
     Given an array with the fields to check in the query, this function loads into 
     self.data the Wrf4gElement values.
     Returns:
     0-->OK
     1-->ERROR
     """

     wheresta=''
     dbc=vdb.vdb()
     for field in fields:
         wheresta="%s AND %s='%s'" %(wheresta,field,self.data[field])
     wheresta=wheresta[4:]     
     #dic=dbc.select(self.element,list2fields(fields), wheresta, verbose=1 )
     dic=dbc.select(self.element,list2fields(fields),wheresta, verbose=1 )
     self.__init__(dic[0])
     print self.sdate
     if id>0: return id
     else: return -1
      
    def create(self):

        """
        Create experiment
        Returns
        0--> Creation Worked.
        -1--> Creation Failed
        """
        dbc=vdb.vdb()
        id=dbc.insert(self.element,self.data,verbose=self.verbose)
        if id>0: return id
        else: return -1
       

    
def pairs2dict(pairs):
    d={}
    for p in pairs.split(','):
        s=p.split('=')
        d[s[0]]=s[1]
    return d

def list2fields(arr):
    fields=''
    for i in arr:
       fields=",%s,%s" %(fields,i)
       fields=fields[2:]
    return fields


"""def dict2pairs(dicti):
    fi=''
    for field,value in dicti.items():        
        fi= ",%s=%s"%(fi, field)
    return fi[1:]
"""    

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
        exp=Wrf4gElement(data='',verbose=options.verbose)
        output=getattr(exp,function)()
        print ','.join(output)

        
    else:
        exp=Wrf4gElement(pairs2dict(exp_values),options.verbose)
        output=getattr(exp,function)(fvalues)   
        print output




   
