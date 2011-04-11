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



class realization:
    """ experiment CLASS
    """
       
    def __init__(self,name='',sdate='',edate='',id_exp=''):
        self.name=name
        self.sdate=sdate
        self.edate=edate
        self.id_exp=id_exp
        self.restart=''
        self.status=''
        self.cdate=''
        self.ctime=''
        
    def reload(self,data):
        self.name=data['name']
        self.sdate=data['sdate']
        self.edate=data['edate']
        self.restart=data['restart']
        self.status=data['status']
        self.cdate=data['cdate']
        self.ctime=data['ctime']
   
    """
    def get_id(self, verbose='no'):

        dbc=vdb.vdb()
        wheresta="id_exp='%s'" %self.id_exp
        idp=dbc.select('rea','id', wheresta, verbose=1 )
        id = vdb.list_query().one_field(idp)
        if id>0: return id
        else: return -1
    """
    
    def  compare(self, verbose='no'):
        """    
        Query database to check in the experiment exists
        Returns:
        0 --> not exists
        1 --> exists with the same parameters 
        2 --> mutliphysics or edate are differents
        3 --> exists another experiment with this name    
        """
        dbc=vdb.vdb()
        wheresta="name='%s' AND sdate='%s' AND edate='%s'" %(self.name,self.sdate,self.edate)              
        dic=dbc.select('rea','id', wheresta, verbose=1 )
        id = vdb.list_query().one_field(dic)
        if id>0: return id
        else: return -1
    
    def repeated(self):
        pass
    
    
    def  loadfromDB(self, verbose='no'):
        """    
        Given the experiment name, this function loads into the class all the experiment values.
        Returns:
        0-->OK
        1-->ERROR
        """
        fi = ''
        dbc = vdb.vdb()
        for k in self.__dict__.keys(): 
            fi = "%s,%s" % (fi, k)
        fi = fi[1:]
                
        wheresta = "id_exp='%s'" % (self.id_exp)
        dic = dbc.select('rea', fi, wheresta, verbose=1)
        self.reload(dic[0])
        return 0

      
    def  create(self):
        """
        Create experiment
        Returns
        0--> Creation worked.
        """
        dbc=vdb.vdb()
        id=dbc.insert('exp',self.__dict__,  verbose=1 ) 
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
    
    r=realization(**pairs2dict(exp_values)) 
    output=getattr(r,function)(*fvalues)
    print output




   
