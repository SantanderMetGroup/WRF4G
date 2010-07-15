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
                    get_Name(self)
                    get_Id(self,name):
                    get_Start_Date(self)
                    set_Start_Date(self,start_date)
                    get_End_Date(self)
                    set_End_Date(self,end_date)
                    get_Status(self)
                    set_Status(self,status)
    """
                    
                   
      


    def __init__(self,name,verbose=0):
        self.name=name
        self.experiment_db=Experiment_db()
        self.verbose=verbose
        #check if the experiment exists
        try:
            Experiment_db.objects.get(name=self.name)
        except DoesNotExist:
            #Create the experiment in the data base if it does not exist
            self.experiment_db.name=self.name
            self.experiment_db.save()
        else:
            #If it have already existed, load it in self.experiment_db
            self.experiment_db=Experiment_db.objects.get(name=self.name)
        

    def get_Id(self):
        self.experiment_db=Experiment_db.objects.get(name=self.name)
        self.experiment_db.id=self.id
        if self.verbose==1:
            print "Getting id %s of the experiment  %s"%(self.name,self.id)
        return self.id

    def get_Start_Date(self):
        self.experiment_db=Experiment_db.objects.get(name=self.name)
        self.start_date=self.experiment_db.start_date
        if self.verbose==1:
            print "Getting Start Date %s of the experiment %s"%(self.start_date,self.name)
        return self.start_date
        
    def set_Start_Date(self,start_date):
        self.start_date=start_date 
        self.experiment_db.start_date=self.start_date
        self.experiment_db.save()
        if self.verbose==1:
           print "Setting Start Date  %s of the  experiment %s"%(self.start_date,self.name)

    def get_End_Date(self):
        self.experiment_db=Experiment_db.objects.get(name=self.name)
        self.end_date=self.experiment_db.end_date
        if self.verbose==1:
           print "Getting End Date  %s of the  experiment %s"%(self.end_date,self.name)
        return self.end_date
        
    def set_End_Date(self,end_date):
	self.end_date=end_date
        if self.verbose==1:
           print "Setting End Date of the  experiment %s"%(self.end_date,self.name)
       
                       
    def get_Status(self):
	""" Status of the  experiment : D,R,S,Q Done,Run,Schedule,Queue"""
        self.experiment_db=Experiment_db.objects.get(name=self.name)
        self.status=self.experiment_db.status		
        if self.verbose==1:
           print "Getting the estatus %s of the  experiment %s %s-%s"%(self.status,self.name,self.start_date,self.end_date)
        return self.status
          
    def set_Status(self,status):
	""" Status of the  experiment : D,R,S,Q Done,Run,Schedule,Queue"""	
	self.status=status
        if self.verbose==1:
           print "Getting the estatus %s of the  experiment %s %s-%s"%(self.status,self.name,self.start_date,self.end_date)
              
    

		
# Class Realization

class Realization:
    """ Class Realization
          atributes:
                    name (string)
                    start_date (datetime)
                    end_date (datetime)
                    exp (id of the experiment)
                    realization_db (Object Realization_db from wrf4g.wrf4gapp.models,
                                   to use de data base wrf4g.)
           
          functions:
                    get_Start_Date(self)
                    set_Start_Date(self,start_date)
                    get_End_Date(self)
                    set_End_Date(self,end_date)
                    get_Exp(self)
                    set_Exp(self,status)
    """
                    
                   
      


    def __init__(self,name,*verbose):
        self.name=name
        self.realization_db=Realization_db()
        self.verbose=verbose
        #check if the experiment exists
        try:
            Realization_db.objects.get(name=self.name)
        except DoesNotExist:
            #Create the experiment in the data base if it does not exist
            self.realization_db.name=self.name
            self.realization_db.save()
        else:
            #If it have already existed, load it in self.experiment_db
            self.realization_db=Realization_db.objects.get(name=self.name)
  
    def get_Id(self):
        self.realization_db=Realization_db.objects.get(name=self.name)
        self.realization_db.id=self.id
        if self.verbose==1:
            print "Getting id %s of the experiment  %s"%(self.name,self.id)
        return self.id


    def get_Start_Date(self):
        self.realization_db=Realization_db.objects.get(name=self.name)
        self.start_date=self.realization_db.start_date
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
       
    def get_Exp(self):
        if self.verbose==1:
           print "Getting experiment id  %s of the realization %s"%(self.exp,self.name)
        return self.exp
                    
    def set_Exp(self,exp):     
        self.exp=exp 
        if self.verbose==1:
           print "Setting experiment  %s of the realization %s"%(self.exp,self.name)
        
    

    

# Class Chunk

class Chunk:
    """ Class chunk
          atributes:
                    name (string)
                    rea (id of the realization)
                    start_date (datetime)
                    end_date (datetime)
                    current_date(datetime)
                    wps_file (True/False) indicates !!! 
                    status (Options=[D,R,S,Q],done,run,schedule,queue)
                    chunk_db (Object Chunk_db from wrf4g.wrf4gapp.models,
                                   to use de data base wrf4g.)
           
          functions:
                    get_Start_Date(self)
                    set_Start_Date(self,start_date)
                    get_End_Date(self)
                    set_End_Date(self,end_date)
                    get_Current_Date(self)
                    set_Current_Date(self,current_date)
                    get_Wps_File(self)
                    set_Wps_File(self,wps_file)
                    get_Status(self)
                    set_Status(self,status)
    """
                    
                   
      


    def __init__(self,name,*verbose):
        self.name=name
        self.experiment_db=Experiment_db()

    def get_Start_Date(self):
        
        if self.verbose==1:
            print "Getting Start Date %s of the chunk %s"%(self.start_date,self.name)
        return self.start_date
        
    def set_Start_Date(self,start_date):
        self.start_date=start_date 
        if self.verbose==1:
           print "Setting Start Date  %s of the  chunk %s"%(self.start_date,self.name)

    def get_End_Date(self):
        if self.verbose==1:
           print "Getting End Date  %s of the  chunk %s"%(self.end_date,self.name)
        return self.end_date
        
    def set_End_Date(self,end_date):
	self.end_date=end_date
        if self.verbose==1:
           print "Setting End Date of the  chunk %s"%(self.end_date,self.name)
       
                       
 
    def get_Current_Date(self):
        if self.verbose==1:
           print "Getting the current_date %s of the  chunk %s %s-%s"%(self.current_date,self.name,self.start_date,self.end_date)
        return self.current_date
        
    def set_Current_Date(self,current_date):
        self.current_date=current_date
        if self.verbose==1:
           print "Setting the current_date %s of the  chunk %s %s-%s"%(self.current_date,self.name,self.start_date,self.end_date)
        
        
    def get_Wps_File(self):
        """ wps_file has values True or False and indicates !!! """
        if self.verbose==1:
           print "Getting the wps_file %s of the  chunk %s %s-%s"%(self.wps_file,self.name,self.start_date,self.end_date)
        return self.wps_file

    def set_Wps_File(self,wps_file):
        """ wps_file has values True or False and indicates !!! """
        self.wps_file=wps_file
        if self.verbose==1:
           print "Setting the wps_file %s of the  chunk %s %s-%s"%(self.wps_file,self.name,self.start_date,self.end_date)


    def get_status(self):
        """ Status of the  chunk : D,R,S,Q Done,Run,Schedule,Queue"""		
        if self.verbose==1:
           print "Getting the estatus %s of the  chunk %s %s-%s"%(self.status,self.name,self.start_date,self.end_date)
        return self.status
          
    def set_status(self,status):
        """ Status of the  chunk : D,R,S,Q Done,Run,Schedule,Queue"""	
        self.status=status
        if self.verbose==1:
           print "Getting the estatus %s of the chunk %s %s-%s"%(self.status,self.name,self.start_date,self.end_date)






# Class File_Type

class File_Type:
    """ Class File_Type
          atributes:
                    type_ (type of the file)
                    freq_h(3h or 6h)         
                    file_type_db (Object File_db from wrf4g.wrf4gapp.models,
                                   to use de data base wrf4g.)
           
          functions:
                    get_Type(self)
                    set_Type(self,type_)
                    get_Freq_H(self)
                    set_Freq_H(self,freq_h)
    """
                    
                   
      


    def __init__(self,name,*verbose):
        self.name=name
        self.file_type_db=File_Type_db()

    def get_Type(self):
        
        if self.verbose==1:
            print "Getting type %s of the file_type id %s "%(self.type_,self.id)
        return self.type_
        
    def set_Type(self,type_):
        self.type_=type_ 
        if self.verbose==1:
           print "Setting type  %s of the  file_type id %s"%(self.type_,self.id)

    def get_Freq_H(self):
        if self.verbose==1:
           print "Getting freq_h %s of the  file_type id %s"%(self.freq_h,self.id)
        return self.freq_h

        
    def set_Freq_H(self,freq_h):
        self.freq_h=freq_h
        if self.verbose==1:
           print "Setting freq_h %s of the  file_type id %s"%(self.freq_h,self.id)
        





# Class File

class File:
    """ Class File
          atributes:
                    type (id of the filetype)
                    path(path of the file)
                    rea(id of the realization)
                    start_date (datetime)
                    end_date (datetime)                
                    file_db (Object File_db from wrf4g.wrf4gapp.models,
                                   to use de data base wrf4g.)
           
          functions:
                    get_Type(self)
                    set_Type(self,type)
                    get_Path(self)
                    set_Path(self,path)
                    get_Rea(self)
                    set_Rea(self,rea)
                    get_Start_Date(self)
                    set_Start_Date(self,start_date)
                    get_End_Date(self)
                    set_End_Date(self,end_date)
                 
    """
                    
                   
      


    def __init__(self,name,*verbose):
        self.name=name
        self.file_db=file_db()

    def get_Start_Date(self):
        
        if self.verbose==1:
            print "Getting Start Date %s of the file  id %s "%(self.start_date,self.id)
        return self.start_date
        
    def set_Start_Date(self,start_date):
        self.start_date=start_date 
        if self.verbose==1:
           print "Setting Start Date  %s of the  file id %s"%(self.start_date,self.id)

    def get_End_Date(self):
        if self.verbose==1:
           print "Getting End Date  %s of the  file id %s"%(self.end_date,self.id)
        return self.end_date
        
    def set_End_Date(self,end_date):
	self.end_date=end_date
        if self.verbose==1:
           print "Setting End Date of the  file id %s"%(self.end_date,self.id)
       
    def get_Type(self):
        if self.verbose==1:
           print "Getting type id  %s of the  file id %s"%(self.type_,self.id)
        return self.type_
                    
    def set_Type(self,type_):     
        self.type_=type_ 
        if self.verbose==1:
           print "Setting  type id  %s of the file %s"%(self.type_,self.id)

    def get_Rea(self):
        if self.verbose==1:
           print "Getting  realization id  %s of the file id %s"%(self.rea,self.id)
        return self.rea
                    
    def set_Rea(self,rea):     
        self.rea=rea 
        if self.verbose==1:
           print "Setting   realization id  %s of the file id %s"%(self.rea,self.id)
   
    def get_Path(self):
        if self.verbose==1:
           print "Getting  path  %s of the file id %s"%(self.path,self.id)
        return self.path
                    
    def set_Path(self,exp):     
        self.exp=exp 
        if self.verbose==1:
           print "Setting   path  %s of the file id %s"%(self.path,self.id)
   



