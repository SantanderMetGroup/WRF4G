#!/usr/bin/python

from sys import stderr,exit, path
import sys
import inspect
import vdb
import vcp
from datetime import datetime
from optparse import OptionParser

def datetime2datewrf (date_object):
    return date_object.strftime("%Y-%m-%d_%H:%M:%S")

def datetime2dateiso (date_object):
    return date_object.strftime("%Y%m%dT%H%M%SZ") 

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

def create_hash():
    import md5
    import random
    rand=random.randint(1,60000)
    text=str(datetime.utcnow()) + str(rand)
    ha=md5.new()
    ha.update(text)
    return ha.hexdigest()


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
        self.data['id']=id
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
                        self.prepare_storage()
            else:
                if self.verbose: stderr.write('%s already exists. Submitting...\n'%self.element)
                self.data['id']=-1
        else:
            if self.verbose: stderr.write('Creating %s\n'%self.element)
            self.data['id']=self.create()
            self.prepare_storage()
    
                  
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
        
    def get_id_from_name(self):

        wheresta=''
        dbc=vdb.vdb()
        idp=dbc.select(self.element,'id',"name='%s'"%self.data['name'],verbose=self.verbose)
        id = vdb.list_query().one_field(idp)
        if id !='': return id
        else: return -1
        
    def run(self):
        for id_rea in self.get_realizations_id():
            rea=Realization(data={'id': str(id_rea)},verbose=self.verbose)
            rea.run()            
        return 0
            
    def get_realizations_id(self):
        """    
        Query database and Returns a list with the realization
        ids of an experiment"""
        
        dbc=vdb.vdb()
        idp=dbc.select('Realization','id','id_exp=%s'%self.data['id'],verbose=self.verbose)
        ids_rea = vdb.list_query().one_field(idp,'python')
        return ids_rea 
    
    def prepare_storage(self):          
        # Load the URL into the VCPURL class
        exec open('wrf4g.conf').read()
        exp_dir="%s/experiments/%s" %(WRF4G_BASEPATH, self.data['name'])
        list=vcp.VCPURL(exp_dir)
        list.mkdir(verbose=self.verbose)
        output=vcp.copy_file('wrf4g.conf',exp_dir,verbose=self.verbose) 
        output=vcp.copy_file('wrf.input',exp_dir,verbose=self.verbose) 
      

    
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
 
    def get_name(self):
        name=self.get_one_field(['name'], ['id'])
        return name
    
    def get_restart(self):
        restart=self.get_one_field(['restart'], ['id'])
        return datetime2datewrf(restart)
        
    def set_restart(self,restart_date):
        self.data['restart']=restart_date
        oc=self.update_fields(['restart'], ['id'])    
        return oc

    def get_cdate(self):
        restart=self.get_one_field(['cdate'], ['id'])
        return restart
   
    def set_cdate(self,cdate):
        self.data['cdate']=restart_date
        oc=self.update_fields(['cdate'], ['id'])    
        return oc

    def set_status(self,status):
        self.data['status']=status
        oc=self.update_fields(['status'], ['id'])    
        return oc

    def number_of_chunks(self,id_rea):
        dbc=vdb.vdb()
        nchunkd=dbc.select('Chunk','COUNT(Chunk.id_chunk)','id_rea=%s'%self.data['id'])
        nchunk=vdb.parse_one_field(nchunkd)
        return nchunk
    
    def last_chunk(self):
        dbc=vdb.vdb()
        nchunkd=dbc.select('Chunk','MAX(Chunk.id_chunk)','id_rea=%s'%self.data['id'])
        nchunk=vdb.parse_one_field(nchunkd)
        return nchunk 
         
    def run(self,nchunk=0):
        import gridway
        NP=1
        REQUIREMENTS=''
        exec open('wrf4g.conf').read()
        
        dbc=vdb.vdb()
        rea_name=self.get_name()
        chunkd=dbc.select('Chunk,Realization','MIN(Chunk.id_chunk),MIN(Chunk.id)','id_rea=%s AND Realization.restart >= Chunk.sdate'%self.data['id'],verbose=self.verbose)
        [first_id_chunk,first_id]=chunkd[0].values()
        if nchunk == 0: 
            nchunk=self.last_chunk()

        for chunki in range(first_id,first_id+nchunk):
            chi=Chunk(data={'id':'%s'%chunki})
            chi.loadfromDB(['id'],chi.get_configuration_fields())
            arguments='%s %s %d %d %s %s'%(rea_name,self.data['id'] ,chi.data['id_chunk'],chi.data['id'],datetime2datewrf(chi.data['sdate']),datetime2datewrf(chi.data['edate']))
            job=gridway.job()
            job.create_template(rea_name + '__' + str(chi.data['id_chunk']),arguments,np=NP,req=REQUIREMENTS)
            if chunki == first_id:
                gw_id=job.submit()
            else:
                gw_id=job.submit(dep=gw_id)
            
            job_data={'gw_job': gw_id,'id_chunk': str(chi.data['id']), 'hash': create_hash()}
            job=Job(job_data,verbose=self.verbose)
            jid=job.create()
            job.set_status(1)
            
                
    def prepare_storage(self):          
        # Load the URL into the VCPURL class
        exec open('wrf4g.conf').read()
        reas=self.data['name'].split('__')
        rea_dir="%s/experiments/%s/%s" % (WRF4G_BASEPATH,reas[0],self.data['name'])
        for dir in ["output","restart","wpsout"]:
          repo="%s/%s" % (rea_dir,dir)
          list=vcp.VCPURL(repo)
          list.mkdir(verbose=self.verbose)    
        output=vcp.copy_file('namelist.input',rea_dir,verbose=self.verbose)       
                             
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
    
    def get_id_rea(self):
        id_rea=self.get_one_field(['id_rea'], ['id'])
        return id_rea     
        
    def prepare_storage(self):
        pass
   
class Job(Component):

    def get_reconfigurable_fields(self):
        return['resource','wn']

    def get_distinct_fields(self):
        return['gw_job','id_chunk']
    
    def stjob2stchunk(self):
        dst={1:1, 10: 2, 40: 4, 41: 3}
        return dst

    def set_status(self,st):
        self.data['status']=st
        oc=self.update_fields(['status'], ['id'])
        # If status involves any change in Chunk status, change the Chunk in DB
        dst=self.stjob2stchunk()
        if st in dst:
            id_rea=Chunk(data={'id': self.data['id_chunk']},verbose=self.verbose).set_status(dst[st])
            
        # Add an Event in the DB
        timestamp=datetime2datewrf(datetime.utcnow())
        event_data= {'id_job': self.data['id'], 'status': st, 'timestamp': timestamp }
        id_event=Events(data=event_data).create()
        return oc   
    
    def load_wn_conf(self,wn_gwres):
        wn_gwres=int(wn_gwres)
        dbc=vdb.vdb()
        cond=''
        for field in self.get_distinct_fields():
            cond="%s AND %s='%s'" %(cond,field,self.data[field])
        cond=cond[4:]
        jobd=dbc.select('Job','MAX(id),gw_restarted',cond,verbose=self.verbose)
        [max_id,db_gwres]=jobd[0].values()
        if db_gwres > wn_gwres:
            self.data['gw_restarted']=wn_gwres
            id=self.create()
        elif db_gwres == wn_gwres:
            self.data['id']=str(max_id)
            id=self.update()
        else:
            stderr.write('Error: This job should not be running this Chunk\n')
            exit(9)
        self.set_status(10)
        
    def get_hash(self):
        pass

class Events(Component):
    pass

     
   
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
        output=getattr(comp,function)(*args[3:])
    else:                  
        output=getattr(comp,function)()
    print output




   
