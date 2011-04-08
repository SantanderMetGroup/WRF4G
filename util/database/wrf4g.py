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



class experiment:
    """ experiment CLASS
    """
       
    def __init__(self,name,sdate='',edate='',mphysics='',cont='',basepath=''):
        self.name=name
        self.sdate=sdate
        self.edate=edate
        self.mphysics=mphysics
        self.cont=cont
        self.basepath=basepath
    
    def reload(self,name,sdate='',edate='',mphysics='',cont='',basepath=''):
        self.name=name
        self.sdate=sdate
        self.edate=edate
        self.mphysics=mphysics
        self.cont=cont
        self.basepath=basepath
     
    def get_id(self, verbose='no'):
        """    
        Query database to check if the experiment exists
        Returns:
        -1 --> not exists
        exp.id --> exists with the same parameters 
        """
        dbc=vdb.vdb()
        wheresta="name='%s'" %self.name
        id=dbc.select('exp','id', wheresta, verbose=1 )
        if id>0: return id
        else: return -1

    def  compare_all(self, verbose='no'):
        """    
        Query database to check in the experiment exists
        Returns:
        0 --> not exists
        1 --> exists with the same parameters 
        2 --> mutliphysics or edate are differents
        3 --> exists another experiment with this name    
        """
        
        dbc=vdb.vdb()
        wheresta=''
        fi=''

        for field,value in self.__dict__.items():        
            #wheresta=wheresta+'%s=%s,'%(field,value)
            wheresta=",name='%s'" %self.name
            fi=fi +','+ field 
        
        # Remove the last comma in both strings
        fi=fi[1:]
        wheresta=wheresta[1:]
        
        dic=dbc.select('exp',fi, wheresta, verbose=1 )
        print dic
        if id>0: return id
        else: return -1
    
    
    def  loadfromDB(self, verbose='no'):
     """    
     Given the experiment name, this function loads into the class all the experiment values.
     Returns:
     0-->OK
     1-->ERROR
     """
     fi=''
     dbc=vdb.vdb()
     for k in self.__dict__.keys(): 
        fi="%s,%s"%(fi, k)
     fi=fi[1:]
             
     wheresta= "name='%s'"%self.name
     dic=dbc.select('exp',fi, wheresta, verbose=1 )
     self.reload(**dic[0])
     print self.sdate
     if id>0: return id
     else: return -1

      
    def  create(self):
        """
        Create experiment
        Returns
        0--> Creation worked.
        """
        dbc=vdb.vdb()
        #ic=dbc.insert('exp',self.__dict__,  verbose=1 )
        p={'name': 'valva'}
        print p
        ic=dbc.insert('exp',p,  verbose=1 )        
        if id>0: return id
        else: return -1

def pairs2dict(pairs):
    d={}
    for p in pairs.split(','):
       s=p.split('=')
       d[s[0]]=s[1]
       return d

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
   
   if len(args) < 2:
     parser.error("Incorrect number of arguments")
     exit(1)
    
   function=args[0]
   exp_values=args[1]
   fvalues=[]
   if len(args)>2:   fvalues=args[2:]
   exp=experiment(**pairs2dict(exp_values))
   
   
   
   output=getattr(exp,function)(*fvalues)
   print output




   
