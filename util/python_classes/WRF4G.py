#!/usr/bin/python

from sys import stderr,exit, path
import sys
import inspect
path.append('/home/valva/WRF4G/util/python_classes')
import vdb
from optparse import OptionParser

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

class Component:
    """  Component CLASS
    """

    def __init__(self,data='',verbose='no',reconfigure='no'):
        self.verbose=verbose
        self.reconfigure=reconfigure
        self.element=self.__class__.__name__
        self.data=data
        self.allfields=['','','','']        
        #for field in data.keys():            
        #    setattr(self,field,data[field])
            
    def get_all_fields(self):
        """    
        Returns a list with all the Component fields.
        """
        dbc=vdb.vdb()
        salida=dbc.describe(self.element)
        return salida
    
    def get_id(self,fields):
        """    
        Query database and Returns the id of the Component 
        whose fields match the Components one.
        
        Returns:
        -1 --> not exists
        Component.id  
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
        Returns id:
        id > 0 --> Creation Worked.
        -1--> Creation Failed
        """
        dbc=vdb.vdb()
        id=dbc.insert(self.element,self.data,verbose=self.verbose)
        if id>0: return id
        else: return -1
        
    def update(self):
        """
        Update experiment
        Returns id:
        id >0 --> Creation Worked.
        -1--> Creation Failed
        """
        dbc=vdb.vdb()
        
        ddata={}
        for field in self.get_reconfigurable_fields():
            ddata[field]=self.data[field]
        
        condition='id=%s'%self.data['id']
        id=dbc.update(self.element,ddata,condition,verbose=self.verbose)
        if id>0: return id
        else: return -1   
     
    def prepare(self):
        """
        Checks if experiment exists in database. If experiment exists, 
        check if it is the same configuration that the one found in 
        the database.
        If the experiment exists and some parameters do not match the ones
        found in database check if its a reconfigure run. 
        Returns
        0--> Database do not have to be changed.
        1--> Change DataBase
        2--> Error. Experiment configuration not suitable with database
        """       
        

        id=self.get_id(self.get_distinct_fields())
        if self.verbose == 1: stderr.write("FLAG Reconfigure=%d\nexp_id= %s\n"%(self.reconfigure,id))
        # Experiment exists in database
        if id > 0:
            self.data['id']=id
            id=self.get_id(self.get_configuration_fields())
            # Experiment is different that the one found in the database
            if id == -1:
                if self.reconfigure == False:
                    stderr.write("Error: %s with the same name and different parameters already exists. If you want to overwrite some paramenters, try the reconfigure option\n." %self.element) 
                    exit(9)
                else: 
                    id=self.get_id(self.get_no_reconfigurable_fields())
                    if id == -1:
                        stderr.write("Error: %s with the same name and different configuration already exists\n"%self.element) 
                        exit(9)
                    else: 
                        self.update()
            else:
                if self.verbose: stderr.write('%s already exists. Submitting...\n'%self.element)
                self.data['id']=-1
        else:
            if self.verbose: stderr.write('Creating %s\n'%self.element)
            self.data['id']=self.create()
                  
        return self.data['id']
       
    
      
class Experiment(Component):
    
    """  Experiment CLASS
    """
    def get_no_reconfigurable_fields(self):
        return ['id','cont','mphysics','basepath']
    
    def get_configuration_fields(self):
        return ['id','sdate','edate','basepath','cont','mphysics','mphysics_labels']
    
    def get_distinct_fields(self):
        return['name']
        
    def get_reconfigurable_fields(self):
        return['sdate','edate','mphysics_labels']
 
      
class Realization(Component):
    """ Realization CLASS
    """

    def get_no_reconfigurable_fields(self):
        return ['id','id_exp','sdate','mphysics_label']
    
    def get_configuration_fields(self):
        return ['id','id_exp','sdate','edate','mphysics_label']
    
    def get_distinct_fields(self):
        return['id_exp','sdate','mphysics_label']
        
    def get_reconfigurable_fields(self):
        return['edate']
        
        
class Chunk(Component):
    """ Chunk CLASS
    """

    def get_no_reconfigurable_fields(self):
        return ['id','id_rea','id_chunk','sdate']
    
    def get_configuration_fields(self):
        return ['id','id_rea','id_chunk','sdate','edate']
    
    def get_distinct_fields(self):
        return['id_rea','id_chunk']
        
    def get_reconfigurable_fields(self):
        return['edate']
 
if __name__ == "__main__":
    usage="""%prog [OPTIONS] exp_values function fvalues 
             Example: %prog 
    """

    
    parser = OptionParser(usage,version="%prog 1.0")
    parser.add_option("-v", "--verbose",action="store_true", dest="verbose", default=False,help="Verbose mode. Explain what is being done")
    parser.add_option("-r", "--reconfigure",action="store_true", dest="reconfigure", default=False,help="Reconfigure element in WRF4G")
    (options, args) = parser.parse_args()
    
    if len(args) < 2:
        parser.error("Incorrect number of arguments")
        exit(1)
        
    class_name=args[0]
    function=args[1]

    data=''
    if len(args) > 2:   data=pairs2dict(args[2])         
    inst="%s(data=%s,verbose=options.verbose,reconfigure=options.reconfigure)"%(class_name,data)
    comp=eval(inst)
    if len(args) > 3:     
        fvalues=args[3:]
        output=getattr(comp,function)(fvalues)
    else:                  
        output=getattr(comp,function)()
    print output




   
