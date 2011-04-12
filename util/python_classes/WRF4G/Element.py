#!/usr/bin/python

from sys import stderr,exit, path
import vdb
import os
from re import search
from optparse import OptionParser




class Element:
    """  Element CLASS
    """
 
    def __init__(self,data='',verbose='no'):
        self.verbose=verbose
        self.element=self.__class__.__name__
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
     dic=dbc.select(self.element,WRF4G.utils.list2fields(fields),wheresta, verbose=1 )
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
       

    





   
