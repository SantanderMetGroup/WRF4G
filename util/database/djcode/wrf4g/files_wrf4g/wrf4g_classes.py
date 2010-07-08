# This Python file uses the following encoding: utf-8

#Classes for wrf4g


import rlcompleter
import readline
import time
import datetime
import string
from commands import getstatusoutput
import re
import os.path
from wrf4g.wrf4gapp.models import *




# Class Experiment

class Experiment:
      """ Class experiment
          atributes:
                    name (string)
                    start_date (date)
                    end_date (date)
                    status (Options=[D,R,S,Q],done,run,schedule,queue)
                    experiment_db (Object Experiment_db from wrf4g.wrf4gapp.models,
                                   to use de data base wrf4g.)
           
          functions:
                    get_Start_Date(self)
                    set_Start_Date(self,start_date)
                    get_End_Date(self)
                    set_End_Date(self,end_date)
                    get_status(self)
                    set_status(self,status)
       """
                    
                   
      


    def __init__(self,name,*verbose):
        self.name=name
        self.experiment_db=Experiment_db()

    def get_Start_Date(self):
        
        if self.verbose==1:
            print "Getting Start Date %s of the realization %s"%(self.start_date,self.name)
        return self.start_date
        
    def set_Start_Date(self,start_date):
        self.start_date=start_date 
        if self.verbose==1:
           print "Setting Start Date  %s of the realization %s"%(self.start_date,self.name)

    def get_End_Date(self):
        if self.verbose==1:
           print "Getting End Date  %s of the realization %s"%(self.end_date,self.name)
        return self.end_date
        
    def set_End_Date(self,end_date):
		self.end_date=end_date
        if self.verbose==1:
           print "Setting End Date of the realization %s"%(self.end_date,self.name)
       
                       
    def get_status(self):
		""" Status of the realization : D,R,S,Q Done,Run,Schedule,Queue"""		
        if self.verbose==1:
           print "Getting the estatus %s of the realization %s %s-%s"%(self.status,self.name,self.start_date,self.end_date)
        return self.status
          
    def set_status(self,status):
		""" Status of the realization : D,R,S,Q Done,Run,Schedule,Queue"""	
		self.status=status
        if self.verbose==1:
           print "Getting the estatus %s of the realization %s %s-%s"%(self.status,self.name,self.start_date,self.end_date)
              
    

		


      
