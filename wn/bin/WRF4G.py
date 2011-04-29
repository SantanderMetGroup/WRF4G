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
       fields="%s,%s" %(fields,i)
    fields=fields[1:]
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
            
    def describeDB(self):
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
    
    def loadfromDB(self,list_query,fields):
     """    
     Given an array with the fields to check in the query, this function loads into 
     self.data the Wrf4gElement values.
     Returns:
     0-->OK
     1-->ERROR
     """

     wheresta=''
     dbc=vdb.vdb()
     for field in list_query:
         wheresta="%s AND %s='%s'" %(wheresta,field,self.data[field])
     wheresta=wheresta[4:]     
     #dic=dbc.select(self.element,list2fields(fields), wheresta, verbose=1 )
     dic=dbc.select(self.element,list2fields(fields),wheresta, verbose=1 )
     self.__init__(dic[0])
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
        oc=dbc.update(self.element,ddata,condition,verbose=self.verbose)
        return oc

    
    def get_one_field(self,val,cond):
        dbc=vdb.vdb()
        
        condition=''
        for field in cond:
          condition="%s AND %s='%s'" %(condition,field,self.data[field])
        condition=condition[4:]   
        
        dic=dbc.select(self.element,list2fields(val),condition, verbose=1 )
        return dic[0][val[0]]

    def update_fields(self,val,cond):
        dbc=vdb.vdb()
        
        ddata={}
        for field in val:
            ddata[field]=self.data[field]
        
        condition=''
        for field in cond:
          condition="%s AND %s='%s'" %(condition,field,self.data[field])
        condition=condition[4:]
        
        oc=dbc.update(self.element,ddata,condition,verbose=self.verbose)
        return oc

     
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
        
    def run(self):
        for id_rea in get_realizations_id():
            rea=Realization(data={'id': str(id_rea)},verbose=self.verbose)
            
    
    def get_realizations_id(self):
        """    
        Query database and Returns a list with the realization
        ids of an experiment"""
        
        id_exp=self.get_id(['name'])
        dbc=vdb.vdb()
        idp=dbc.select('Realization','id','id_exp=%s'%id_exp,verbose=self.verbose)
        ids_rea = vdb.list_query().one_field(idp,'python')
        return ids_rea
        
        
 
      
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
 
    def get_restart(self):
        restart=self.get_one_field(['restart'], ['name'])
        return restart
   
    def set_restart(self,restart_date):
        self.data['restart']=restart_date
        oc=self.update_fields(['restart'], ['name'])    
        return oc
    
    def number_of_chunks(self,id_rea):
        dbc=vdb.vdb()
        nchunkd=dbc.select('Chunk','COUNT(Chunk.id_chunk)','id_rea=%s'%id_rea)
        nchunk=vdb.parse_one_field(nchunkd)
        return nchunk
    
    def last_chunk(self,id_rea):
        dbc=vdb.vdb()
        nchunkd=dbc.select('Chunk','MAX(Chunk.id_chunk)','id_rea=%s'%id_rea)
        nchunk=vdb.parse_one_field(nchunkd)
        return nchunk 
         
    def run(self,id_rea,nchunk=0):
        execfile('wrf4g.conf')
        
        dbc=vdb.vdb()
        chunkd=dbc.select('Chunk,Realization','MAX(Chunk.id_chunk),MAX(Chunk.id)','id_rea=%s AND Realization.restart >= Chunk.sdate'%id_rea)
        [first_id_chunk,first_id]=chunkd[0].values()
        if nchunk == 0: 
            nchunk=self.last_chunk(id_rea)
        # The last chunk id is ch-ch_id (first chunk) + nchunk 

        for chunki in range(first_id,first_id-first_id_chunk+nchunk):
            chi=Chunk(data={'id':'%s'%chunki})
            chi.loadfromDB(['id'],chi.get_configuration_fields())         
            job=gridway.job()
            job.create_template(name,arguments,np=NP,req=REQUIREMENTS)
            if chunki == first_id:
                job=gridway.submit(dep=id)
            else:
                job=gridway.submit(dep=id)
            
            
                             
    def is_finished(self,id_rea):
        dbc=vdb.vdb()
        max_id=dbc.select('Chunk','MAX(id)','id_rea=%s'%id_rea,verbose=self.verbose)
        status=dbc.select('Chunk','status','id=%s'%vdb.parse_one_field(max_id),verbose=self.verbose)
        if status == 100:
            return 1
        else:
            return 0
          
        
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

    def get_wps(self):
        wps=self.get_one_field(['wps'], ['id'])
        return wps     
   
    def set_wps(self,f):
        self.data['wps']=f
        oc=self.update_fields(['wps'], ['id'])
        return oc     
   
    def set_status(self,st):
        self.data['status']=st
        oc=self.update_fields(['status'], ['id'])
        return oc   
    
   
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
    # Instantiate the Component Class:
    # comp=Chunk(data={'id': '23'},verbose=options.verbose,reconfigure=options.reconfigure)
    comp=eval(inst)
    if len(args) > 3:     
        fvalues=args[3]
        #fvalues=args[3].split(',')
        # Call the Class method.
        output=getattr(comp,function)(fvalues)
    else:                  
        output=getattr(comp,function)()
    print output




   
