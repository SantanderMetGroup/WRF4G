from __future__     import with_statement
from datetime       import datetime
from os.path        import join , basename , dirname , exists , expandvars , isdir
from re             import search , match
from wrf4g          import vdblib , vcplib , gridwaylib , WRF4G_LOCATION , WRF4G_DEPLOYMENT_LOCATION , DB4G_CONF , GW_LOCATION , GW_BIN_LOCATION , GW_LIB_LOCATION , MYSQL_LOCATION
from wrf4g.utils    import VarEnv , datetime2datewrf , list2fields , create_hash

import os
import sys
import re
import shutil
import tarfile
import subprocess
import time
import signal
import getpass

try :
    sys.path.insert( 0 , GW_LIB_LOCATION  )
    from drm4g  import REMOTE_VOS_DIR 
except :
    pass

__version__  = '1.5.1'
__author__   = 'Carlos Blanco'
__revision__ = "$Id$"


dreastatus = { 
              0 : 'P' , 
              1 : 'W' , 
              2 : 'R' ,
              3 : 'F' ,
              4 : 'D' ,
              }

def JobStatus( dbc ):
    dstatus=dbc.select('Jobstatus','id,description')
    sta={}
    for entry in dstatus:
        sta[entry['id']]=entry['description']
    return sta

def getuserid(name,dbc):
    idp=dbc.select('User','id',"name='%s'"%name)
    if len(idp) == 0:
        id=dbc.insert('User',{'name': name})
    else:
        id = vdblib.list_query().one_field(idp)
    return id      

class Environment:
    def __init__(self, dbc):
        self.dbc = dbc
    
    def list_experiments(self):
        idp = self.dbc.select('Experiment','id','1=1')
        id  = vdblib.parse_one_list(idp,interpreter='python')
        return id
        
class Component:
    """
    Component CLASS
    """

    def __init__(self,data='',verbose=False,dryrun=False,reconfigure=False,dbc=False):
        self.verbose=verbose
        self.dryrun=dryrun
        self.reconfigure=reconfigure
        self.element=self.__class__.__name__
        self.data=data
        self.dbc=dbc   
            
    def describeDB(self):
        """    
        Returns a list with all the Component fields.
        """
        salida=self.dbc.describe(self.element)
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
        
        for field in fields:
            wheresta="%s AND %s='%s'" %(wheresta,field,self.data[field])
        wheresta=wheresta[4:]
        
        idp=self.dbc.select(self.element,'id',wheresta,verbose=self.verbose)
        id = vdblib.list_query().one_field(idp)
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
        for field in list_query:
            wheresta="%s AND %s='%s'" %(wheresta,field,self.data[field])
        wheresta=wheresta[4:]    
        dic=self.dbc.select(self.element,list2fields(fields),wheresta, verbose=self.verbose )
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
        id=self.dbc.insert(self.element,self.data,verbose=self.verbose)
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
        ddata={}
        for field in self.get_reconfigurable_fields():
            ddata[field]=self.data[field]
        
        condition='id=%s'%self.data['id']
        oc=self.dbc.update(self.element,ddata,condition,verbose=self.verbose)
        return oc
    
    def delete(self):
        """
        Delete a row of Component. It clears the DB and all the data related to it. 
        """
        if self.verbose: 
            sys.stderr.write("Deleting %s with id %s\n"%(self.element,self.data['id']))
        condition='id=%s'%self.data['id']
        o=self.dbc.delete_row(self.element,condition)
        return 0
    
    def get_one_field(self,val,cond):
        condition=''
        for field in cond:
            condition="%s AND %s='%s'" %(condition,field,self.data[field])
        condition=condition[4:]   
        
        dic=self.dbc.select(self.element,list2fields(val),condition, verbose=self.verbose )
        return dic[0][val[0]]

    def update_fields(self,val,cond):
        ddata={}
        for field in val:
            ddata[field]=self.data[field]
        
        condition=''
        for field in cond:
            condition="%s AND %s='%s'" %(condition,field,self.data[field])
        condition=condition[4:]
        
        oc=self.dbc.update(self.element,ddata,condition,verbose=self.verbose)
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
        if (self.element == "Experiment") and (len(self.data['name'])>512): 
            sys.stderr.write("Experiment name must be maximun 512 characters")
            sys.exit(9)
        if (self.element == "Experiment") and (len(self.data['name'])==0): 
            sys.stderr.write("Experiment name can not be null")    
            sys.exit(9)
        id=self.get_id(self.get_distinct_fields())
        if self.verbose == 1: 
            sys.stderr.write("FLAG Reconfigure=%d\nexp_id= %s\n"%(self.reconfigure,id))
        # Experiment exists in database
        if id > 0:
            self.data['id']=id
            id=self.get_id(self.get_configuration_fields())
            # Experiment is different that the one found in the database
            if id == -1:
                if self.reconfigure == False:
                    sys.stderr.write("Error: %s with the same name and different parameters already exists.\n" %self.element)
                    sys.stderr.write("If you want to overwrite some parameters, try the reconfigure option.\n")
                    sys.exit(9)
                else: 
                    id=self.get_id(self.get_no_reconfigurable_fields())
                    if id == -1:
                        sys.stderr.write("Error: %s with the same name and different configuration already exists\n"%self.element) 
                        sys.exit(9)
                    else: 
                        self.update()
                        self.prepare_storage()
            else:
                if self.reconfigure == False:
                    sys.stderr.write('%s already exists. \n'%self.element)
                    self.data['id']=-1
                    sys.exit(9)
                else:                
                    self.prepare_storage()
        else:
            if self.verbose: 
                sys.stderr.write('Creating %s\n'%self.element)
            if self.element == "Experiment":
                username=getpass.getuser()
                userid=getuserid(username,self.dbc)
                self.data['id_user']=userid
                
            self.data['id']=self.create()
            if not self.dryrun:
                self.prepare_storage()
                    
        return self.data['id']    

class Experiment(Component):
    """ 
    Experiment CLASS
    """
    def get_no_reconfigurable_fields(self):
        return ['id','multiple_dates','multiple_parameters','basepath']
    
    def get_configuration_fields(self):
        return ['id','sdate','edate','basepath','multiple_dates','multiple_parameters','multiparams_labels']
    
    def get_distinct_fields(self):
        return['name']
        
    def get_reconfigurable_fields(self):
        return['sdate','edate','multiparams_labels']
     
    def get_id_from_name(self):

        wheresta=''
        
        idp=self.dbc.select(self.element,'id',"name='%s'"%self.data['name'],verbose=self.verbose)
        id = vdblib.list_query().one_field(idp)
        self.data['id']=id
        if id !='': return id
        else: return -1

    def get_name(self):
        name=self.get_one_field(['name'], ['id'])
        return name
           
    def run(self,nrea=0,nchunk=0,priority=0,rerun=False,force=False,type_dep="afterany"):
        ncrea=0
        if rerun:
            rea_ids=self.get_realizations_id()
        else:
            rea_ids=self.get_unfinishedreas_id()
            
        if len(rea_ids) == 0:
            sys.stderr.write('There are not realizations to run.\n')
        
        if nrea >  0:
            rea_ids=rea_ids[0:nrea]

        for id_rea in rea_ids:
            rea=Realization(data={'id': str(id_rea)},verbose=self.verbose,dryrun=self.dryrun, dbc = self.dbc)
            st=rea.run(nchunk=nchunk,priority=priority,rerun=rerun,force=force,type_dep=type_dep)
            if st==1:
                ncrea=ncrea+1
                if ncrea==nrea:
                    break
     
    def get_basepath(self):
        cdate=self.get_one_field(['basepath'], ['id'])
        return cdate           
    
    def ps(self, number_of_characters):
        output=''
        if self.data['id'] < 0:
            sys.exit(19)
        rea_ids=self.get_realizations_id()
        dout=[]
        for id_rea in rea_ids:
            rea=Realization(data={'id': str(id_rea)},verbose=self.verbose,dryrun=self.dryrun, dbc = self.dbc)
            rea.ps(number_of_characters)
    
    def statistics(self):
        output=''
        if self.data['id'] < 0:
            sys.exit(19)
        rea_ids=self.get_realizations_id()
        dout=[]
        for id_rea in rea_ids:
            rea=Realization(data={'id': str(id_rea)},verbose=self.verbose,dryrun=self.dryrun, dbc = self.dbc)
            rea.statistics()
            
    
    def summarized_status(self, number_of_characters=20):
        prepared=len(self.get_prepared_reas_id())
        wait=len(self.get_wait_reas_id())
        run=len(self.get_run_reas_id())
        done=len(self.get_done_reas_id())
        fail=len(self.get_fail_reas_id())
        string_to_print = '%-'+ str(number_of_characters) + 's %-3d %-3d %-3d %-3d %-3d'
        print string_to_print % (self.get_name(),prepared,wait,run,done,fail)
                          
    def get_realizations_id(self):
        """    
        Query database and Returns a list with the realization
        ids of an experiment"""        
        
        idp=self.dbc.select('Realization','id','id_exp=%s'%self.data['id'],verbose=self.verbose)
        ids_rea = vdblib.list_query().one_field(idp,'python')
        return ids_rea 
    
    def get_unfinishedreas_id(self):
        """
        Return a list of ids of the realization of a experiment which haven't finished.
        """
        idp=self.dbc.select('Chunk,Realization','DISTINCT Realization.id','Chunk.status!=4 AND Realization.id=Chunk.id_rea AND Realization.id_exp=%s'%self.data['id'],verbose=self.verbose)
        ids_rea = vdblib.list_query().one_field(idp,'python')
        return ids_rea

    def get_run_reas_id(self):
        """
        Return a list of ids of the realization of a experiment which are running.
        """
        idp=self.dbc.select('Chunk,Realization','DISTINCT Realization.id','Chunk.status=2 AND Realization.id=Chunk.id_rea AND Realization.id_exp=%s AND Chunk.id_rea not in (select c2.id_rea From Chunk as c2 where c2.id_rea = Realization.id AND c2.status=3)'%self.data['id'],verbose=self.verbose)
        ids_rea = vdblib.list_query().one_field(idp,'python')
        return ids_rea      
    
    def get_fail_reas_id(self):
        """
        Return a list of ids of the realization of a experiment which have finished. Any of the Realization Chunks have the status 3.
        """
        idp=self.dbc.select('Chunk,Realization','DISTINCT Realization.id','Chunk.status=3 AND Realization.id=Chunk.id_rea AND Realization.id_exp=%s'%self.data['id'],verbose=self.verbose)
        ids_rea = vdblib.list_query().one_field(idp,'python')
        return ids_rea  

    def get_prepared_reas_id(self):
        """
        Return a list of ids of the realization of a experiment which are prepared. Every Chunk in the Realization has status 0.
        """
        # SELECT DISTINCT(Realization.id) From Chunk,Realization where Realization.id=Chunk.id_rea AND Realization.id_exp=2 and Chunk.status = 1 and Chunk.id_rea not in (select c2.id_rea From Chunk as c2 where c2.id_rea = Chunk.id_rea AND c2.status!=1)
        condition='Realization.id=Chunk.id_rea AND Realization.id_exp=%s and Chunk.status = 0 and Chunk.id_rea not in (select c2.id_rea From Chunk as c2 where c2.id_rea = Realization.id AND (c2.status!=0 and c2.status!=4))'%self.data['id']
        idp=self.dbc.select('Chunk,Realization','DISTINCT Realization.id',condition,verbose=self.verbose)
        ids_rea = vdblib.list_query().one_field(idp,'python')
        return ids_rea   

    def get_done_reas_id(self):
        """
        Return a list of ids of the realization of a experiment which have finished. Every Chunk in the Realization has status 4.
        """
        # SELECT DISTINCT(Realization.id) From Chunk,Realization where Realization.id=Chunk.id_rea AND Realization.id_exp=2 and Chunk.status = 1 and Chunk.id_rea not in (select c2.id_rea From Chunk as c2 where c2.id_rea = Chunk.id_rea AND c2.status!=1)
        condition='Realization.id=Chunk.id_rea AND Realization.id_exp=%s and Chunk.status = 4 and Chunk.id_rea not in (select c2.id_rea From Chunk as c2 where c2.id_rea = Realization.id AND c2.status!=4)'%self.data['id']
        idp=self.dbc.select('Chunk,Realization','DISTINCT Realization.id',condition,verbose=self.verbose)
        ids_rea = vdblib.list_query().one_field(idp,'python')
        return ids_rea

    def get_wait_reas_id(self):
        """
        Return a list of ids of the realization of a experiment which have finished.
        """
        idp=self.dbc.select('Chunk,Realization',
                       'DISTINCT Realization.id',
                       'Chunk.status=1 AND Realization.id=Chunk.id_rea AND Realization.id_exp=%s AND Chunk.id_rea not in (select c2.id_rea From Chunk as c2 where c2.id_rea = Realization.id AND (c2.status=2 OR c2.status=3 ) )'%self.data['id'],
                       verbose=self.verbose)
        ids_rea = vdblib.list_query().one_field(idp,'python')
        return ids_rea
    
    def prepare_storage(self):    
        exp_sub_dir = join( WRF4G_LOCATION , 'var' , 'submission' , self.data['name'] )
        if not isdir( exp_sub_dir ) :
            try: 
                os.makedirs( exp_sub_dir )
            except Exception :
                raise Exception( "Couldn't be created '%s' directory" % exp_sub_dir )
        current_path = os.getcwd()
        wrf4g_bundle = join ( exp_sub_dir , "WRF4G.tar.gz" )
        if exists( wrf4g_bundle  ):
            os.remove( wrf4g_bundle )
        tar = tarfile.open( wrf4g_bundle , "w:gz" )
        os.chdir( WRF4G_DEPLOYMENT_LOCATION )
        for dir in [ "bin", "lib" ]:
            tar.add( dir )
        os.chdir( current_path )
         
class Realization(Component):
    """ 
    Realization CLASS
    """
    
    def get_no_reconfigurable_fields(self):
        return ['id','id_exp','sdate','multiparams_label']
    
    def get_configuration_fields(self):
        return ['id','id_exp','sdate','edate','multiparams_label']
    
    def get_distinct_fields(self):
        return['id_exp','sdate','multiparams_label']
        
    def get_reconfigurable_fields(self):
        return['edate']
 
    def get_name(self):
        name=self.get_one_field(['name'], ['id'])
        return name
    
    def get_id_from_name(self):

        wheresta=''
        idp=self.dbc.select(self.element,'id',"name='%s'"%self.data['name'],verbose=self.verbose)
        id = vdblib.list_query().one_field(idp)
        self.data['id']=id
        if id !='': return id
        else: return -1
        
    def get_restart(self):
        restart=self.get_one_field(['restart'], ['id'])
        if restart != None:
           restart=datetime2datewrf(restart)
        return restart
        
    def set_restart(self, args):
        if type( args ) is tuple :
            restart_date = args[ 0 ]
        else :
            restart_date = args
        self.data['restart']=restart_date
        oc=self.update_fields(['restart'], ['id'])    
        return oc

    def get_cdate(self):
        cdate=self.get_one_field(['cdate'], ['id'])
        return cdate

    def get_exp_id(self):
        id_exp=self.get_one_field(['id_exp'], ['id'])
        return id_exp
   
    def set_cdate(self, args ):
        if type( args ) is tuple :
            cdate = args[ 0 ]
        else :
            cdate = args
        self.data['cdate']=cdate
        self.data['ctime']=timestamp=datetime2datewrf(datetime.utcnow())
        oc=self.update_fields(['cdate','ctime'], ['id'])    
        return oc
    
    def has_finished(self):
        lchunk=self.last_chunk()
        status=Chunk(data={'id': '%s'%lchunk}, dbc = self.dbc).get_status()
        if status == 4:
            finished=True
        else:
            finished=False
        return finished

    def get_init_status(self):
        
        dst=self.dbc.select('Chunk','status','id_rea=%s AND id_chunk=1'%self.data['id'] )
        st=vdblib.parse_one_field(dst)
        return st
    
    def current_chunk_status(self):
        status=0
        nchunks=self.number_of_chunks()
        failed=self.dbc.select('Chunk','MIN(id_chunk)','id_rea=%s AND status=3'%self.data['id'] )
        id_chunk=vdblib.parse_one_field(failed)
        
        if id_chunk:     
            status=3
        else:
            did=self.dbc.select('Chunk','MAX(id_chunk)','id_rea=%s AND status=4'%self.data['id'])
            id_chunk=vdblib.parse_one_field(did)
            if id_chunk == None:
                id_chunk=0
            if id_chunk == nchunks:
                status=4
            else:
                id_chunk=id_chunk + 1
        
        ch=Chunk(data={'id_chunk':id_chunk,'id_rea':self.data['id']}, dbc = self.dbc)
        id=ch.get_one_field(['id'], ['id_chunk','id_rea'])
        if status !=4 and status !=3:
            status=ch.get_one_field(['status'], ['id_chunk','id_rea'])
        return (id,id_chunk,int(status))    
        
    def has_failed(self):
        nchunkd=self.dbc.select('Chunk','id','id_rea=%s AND status=3'%self.data['id'] )
        if nchunkd != ():     
            return True
        else:
            return False 
    
    def number_of_chunks(self):
        nchunkd=self.dbc.select('Chunk','COUNT(Chunk.id_chunk)','id_rea=%s'%self.data['id'])
        nchunk=vdblib.parse_one_field(nchunkd)
        return nchunk
        
    def last_chunk(self):        
        nchunkd=self.dbc.select('Chunk','MAX(Chunk.id)','id_rea=%s'%self.data['id'])
        nchunk=vdblib.parse_one_field(nchunkd)
        return nchunk 
    
    def first_chunk(self):        
        nchunkd=self.dbc.select('Chunk','MIN(Chunk.id)','id_rea=%s'%self.data['id'])
        nchunk=vdblib.parse_one_field(nchunkd)
        return nchunk 
       
    def run(self,nchunk=0,priority=0,rerun=False,force=False,repeatchunk=0,type_dep="afterany"):
        exp_name = self.get_exp_name()
        rea_name = self.get_name()       
      
        chunk_status=self.current_chunk_status()
        first_id=chunk_status[0]
        first_id_chunk=chunk_status[1]

        if rerun:
            if repeatchunk is 0 :
                first_id=first_id-first_id_chunk+1     
                first_id_chunk=1
                self.set_restart(None)
                cdate=self.get_one_field(['sdate'], ['id'])
                self.set_cdate(cdate)
            else :
                first_id_chunk=repeatchunk
                chunkd=self.dbc.select('Chunk','id,sdate','id_rea=%s AND id_chunk=%d'%(self.data['id'],repeatchunk),verbose=self.verbose)
                first_id=chunkd[0]['id']
                sdate=chunkd[0]['sdate']              
                self.set_restart(sdate)
                self.set_cdate(sdate)
        else:
            if chunk_status[2] == 4:
                sys.stderr.write('Realization %s already finished.\n'%rea_name)
                return 1
            if not force and ( chunk_status[2] == 2 or chunk_status[2] == 1) :
                sys.stderr.write('Realization %s is still submitting. Use --force if you really want to submit the realization.\n'%rea_name)
                return 1
        if repeatchunk is 0:
            lchunk=self.last_chunk()
            if nchunk != 0 and lchunk-first_id >= nchunk: 
                lchunk=first_id+nchunk-1
        else:
            lchunk=first_id

        for chunki in range(first_id,lchunk+1):
            chi=Chunk(data={'id':'%s'%chunki}, dbc = self.dbc )
            chi.loadfromDB(['id'],chi.get_configuration_fields())
            arguments='%s %s %s %d %d %s %s'%(self.get_exp_name(),
                                              rea_name,self.data['id'],
                                              chi.data['id_chunk'],
                                              chi.data['id'],
                                              datetime2datewrf(chi.data['sdate']),
                                              datetime2datewrf(chi.data['edate'])
                                              )
            
            if chunki == first_id: 
                sys.stderr.write('Submitting realization: "%s"\n'%(rea_name))
            sys.stderr.write('\tSubmitting Chunk %d:\t%s\t%s\n'  % (chi.data['id_chunk'],
                                                                   datetime2datewrf(chi.data['sdate']),
                                                                   datetime2datewrf(chi.data['edate'])
                                                                   )
                             )
    
            if not self.dryrun :
                rea_sub_dir   = join( WRF4G_LOCATION , 'var' , 'submission' , exp_name , rea_name )
                os.chdir( rea_sub_dir )
                WRF4G_package = join( WRF4G_LOCATION , 'var' , 'submission' , exp_name , 'WRF4G.tar.gz' ) 
                if not exists(  WRF4G_package ) :
                    raise Exception( "'%s' file does not exist" % WRF4G_package )
                sandbox  = "file://%s,"                  % ( WRF4G_package )
                sandbox += "file://%s/db.conf,"          % ( rea_sub_dir )
                sandbox += "file://%s/resources.wrf4g,"  % ( rea_sub_dir )
                sandbox += "file://%s/experiment.wrf4g," % ( rea_sub_dir )
                sandbox += "file://%s/namelist.input"    % ( rea_sub_dir )
                input_files = join( rea_sub_dir , 'input_files.tar.gz' )
                if exists( input_files ) :
                    sandbox += ",file://%s" % ( input_files )
                run_vars = VarEnv( join( rea_sub_dir , 'resources.wrf4g' )  )
                gw_job   = gridwaylib.job()
                gw_job.create_template( 
                                       rea_name + '__' + str( chi.data['id_chunk'] ) ,
                                       arguments ,
                                       np           = int ( run_vars.get_variable( 'NP', default = '1') ) ,
                                       req          = run_vars.get_variable( 'REQUIREMENTS' ) ,
                                       environ      = run_vars.get_variable( 'ENVIRONMENT' ) ,
                                       inputsandbox = sandbox ,
                                       verbose      = self.verbose
                                       )
                if chunki == first_id:
                    gw_id = gw_job.submit(priority=priority)
                else:
                    gw_id = gw_job.submit(priority=priority,dep=gw_id,type_dep=type_dep)
                job_data = { 'gw_job' : gw_id , 
                            'id_chunk': str(chi.data['id']), 
                            'hash'    : create_hash(),
                            }
                job = Job(job_data,verbose=self.verbose,dbc = self.dbc)
                jid = job.create()
                job.set_status('1')
                self.dbc.commit()
        return 0

    def get_exp_name(self):
         if 'id' in self.data.keys():
           self.data['name']=self.get_name()
         else:
            self.data['id']=self.get_id(['name'])
         cexp=Experiment(data={'id': self.get_exp_id()}, dbc = self.dbc)
         exp_name=cexp.get_name()
         return exp_name  
        
    def ps(self, number_of_characters):
        if 'id' in self.data.keys():
            self.data['name']=self.get_name()
        else:
            self.data['id']=self.get_id(['name'])
            if self.data['id']== -1:
                sys.stderr.write('Realization with name %s does not exist\n'%self.data['name'])
                sys.exit(1)            
        drea=self.dbc.select('Realization','name,restart,sdate,cdate,edate','id=%s'%self.data['id'])
        dout=drea[0]
        nchunks=self.number_of_chunks()
        status=self.get_init_status()
        
        if status == 0:
            djob={'gw_job': '-', 'status': 0, 'wn': '-', 'resource': '-', 'exitcode': None,'nchunks':'0/%d'%nchunks}
        else:
            (id_chunk,current_chunk,status)=self.current_chunk_status()     
            if status == 0:       
                djob={'gw_job': '-', 'status': 0, 'wn': '-', 'resource': '-', 'exitcode': None,'nchunks':'%s/%d'%(current_chunk,nchunks)}
            else:
                ch=Chunk(data={'id': id_chunk},  dbc = self.dbc) 
                lastjob=ch.get_last_job()
                j=Job(data={'id':lastjob},dbc = self.dbc)            
                djob=j.get_info()
                if djob['status'] < 10:
                    djob={'gw_job': djob['gw_job'], 'status': djob['status'], 'wn': '-', 'resource': '-', 'exitcode': None,'nchunks':'0/%d'%nchunks}
                djob['nchunks']='%d/%d'%(current_chunk,nchunks)
        dout.update(djob)
        dout['rea_status']=status            
        djobstatus=JobStatus( self.dbc )
        if dout['exitcode']==None:
            exitcode='-'
        else:
            exitcode=str(dout['exitcode']) 
    
        runt   = int(dout['cdate'].strftime("%s"))-int(dout['sdate'].strftime("%s"))    
        totalt = int(dout['edate'].strftime("%s"))-int(dout['sdate'].strftime("%s"))
        per = ( runt * 100.0) / totalt
        if dout['status'] < 10 and dout['status'] > 0 and dout['rea_status'] != 4: 
            dout['rea_status']=1  
        string_to_print = '%-'+ str(number_of_characters) + 's %-5s %-2s %-15s %-10s %-10s %-13s %2s %2.2f'
        print string_to_print  % (dout['name'][0:number_of_characters],
                                    dout['gw_job'],
                                    dreastatus[dout['rea_status']],
                                    dout['nchunks'],
                                    dout['resource'][0:10],
                                    dout['wn'][0:10],
                                    djobstatus[dout['status']],
                                    exitcode,
                                    per)    
                           
    def statistics(self):
        if 'id' in self.data.keys():
            self.data['name']=self.get_name()
        else:
            self.data['id']=self.get_id(['name'])
            if self.data['id']== -1:
                sys.stderr.write('Realization with name %s does not exist\n'%self.data['name'])
                sys.exit(1)

        last_chunk=self.current_chunk_status()

        if last_chunk[2] == 0 or last_chunk[2] == 1 :
            lastc=last_chunk[0]
        else :
            lastc=last_chunk[0]+1
        ci=1
        for id_chunk in range(self.first_chunk(),lastc):
            ch=Chunk(data={'id': id_chunk}, dbc = self.dbc)
            j=Job(data={'id':ch.get_last_job()},dbc = self.dbc)
            jobi=j.list_events()
            line="%s;%d;%d;%d;%s;%s;"%(self.get_name(),ci,jobi['gw_job'],jobi['exitcode'],jobi['resource'],jobi['wn'])
            lestados=JobStatus( self.dbc )
            es=lestados.keys()
            es.sort()

            for st in es:
                if jobi['tiempos'].has_key(st):
                    line+=(str(jobi['tiempos'][st])) + ";"
                else:
                    line+=";"
            
            print line
            ci=ci+1

    def prepare_storage(self): 
        files = [ 
                 join( WRF4G_LOCATION , 'etc' , 'db.conf' ) ,
                 "resources.wrf4g" ,
                 "experiment.wrf4g" ,
                 "namelist.input" ,
                 ]        

        def _file_exist( file ) :    
            if not exists ( file ) :
                raise Exception( "'%s' is not available" % file_path )
        [  _file_exist( file ) for file in files ] 
        
        rea_name           = self.data['name']        
        exp_name           = self.data['name'].split('__')[0]
        rea_submission_dir = join( WRF4G_LOCATION , 'var' , 'submission' , exp_name , rea_name )
        if not isdir( rea_submission_dir ) :
            try :
                os.makedirs( rea_submission_dir )
            except Exception :
                raise Exception( "Couldn't be created '%s' directory" % rea_submission_dir ) 
        if exists( 'input_files' ):
            current_path = os.getcwd()
            tar = tarfile.open( 'input_files.tar.gz' , "w:gz" )
            os.chdir( 'input_files')
            for elem in os.listdir('.') :
                tar.add( elem )
            os.chdir( current_path )
            dst_file = join( rea_submission_dir , 'input_files.tar.gz' )
            if exists( dst_file ):
                os.remove( dst_file )
            shutil.move( 'input_files.tar.gz' , dst_file )
            
        def _copy( file ):
            shutil.copy( expandvars( file ) , rea_submission_dir )
        [ _copy( file ) for file in files ]         
                
    def prepare_remote_storage(self , args ):
        """
        Create a remote tree directory of a Realization
        
        Realization
           *  output                    
           *  restart
           *  realout
           *  log          
        """
        try :
            remote_realization_path = args[ 0 ]
            vcp_remote_path = vcplib.VCPURL( dirname( remote_realization_path ) )
            if not vcp_remote_path.exists( verbose = self.verbose ) :
                vcp_remote_path.mkdir( verbose = self.verbose )
            for dir in [ "output" , "restart" , "realout" , "log" ]:
                vcp_repo = vcplib.VCPURL( "%s/%s/" % ( remote_realization_path , dir ) )
                if not vcp_repo.exists( verbose = self.verbose ) :
                    vcp_repo.mkdir( verbose = self.verbose )
            return 0
        except Exception, err :
            sys.stderr.write( 'Error creating the remote repository: %s\n' % str( err ) )
            return 1
        
    def copy_configuration_files(self , args ):
        """
        Copy configuration files from the WN to the out path such as :
            * experiment.wrf4g
            * resources.wrf4g
            * db.conf
            * namelist.input
        """
        # We have to add an overwriting option 
        try :
            remote_realization_path = args[ 0 ]
            for configuration_file in [ "db.conf" , "experiment.wrf4g" ,  "resources.wrf4g" ]:
                vcplib.copy_file( configuration_file, join( remote_realization_path , configuration_file  ) , verbose = self.verbose )
            vcplib.copy_file( "namelist.input" , join ( remote_realization_path , "namelist.input" ) , verbose = self.verbose )
            return 0
        except Exception, err :
            sys.stderr.write( 'Error coping configurations files: %s\n' % str( err ) )
            return 1  
                              
    def is_finished(self,id_rea):
        
        max_id = self.dbc.select('Chunk','MAX(id)','id_rea=%s'%id_rea,verbose=self.verbose)
        status = self.dbc.select('Chunk','status','id=%s'%vdblib.parse_one_field(max_id),verbose=self.verbose)
        if status == '4':
            return 1
        else:
            return 0

    def stop_running_chunks(self):
        import gridwaylib
        condition="Chunk.id_rea=%s and Job.id_chunk=Chunk.id AND (Chunk.status=1 OR Chunk.status=2) GROUP BY Chunk.id"%self.data['id']
        output=self.dbc.select('Chunk,Job','Chunk.id,MAX(Job.id)','%s'%condition,verbose=self.verbose)
        chunk_id = job_id = []
        for couple in output:
            chunk_id.append(couple['id'])
            j=Job(data={'id':couple['MAX(Job.id)']},dbc = self.dbc)
            job_id.append(j.get_gw_job())
            
        task=gridwaylib.job()
        to=task.kill(job_id)
        condition="id_rea=%s AND (status=1 OR status=2)"%self.data['id']
        data={'status':0}
        output=self.dbc.update('Chunk',data,'%s'%condition,verbose=self.verbose)

    def change_priority(self, priority):
        import gridwaylib
        condition="Chunk.id_rea=%s and Job.id_chunk=Chunk.id AND (Chunk.status=1 OR Chunk.status=2) GROUP BY Chunk.id"%self.data['id']
        output=self.dbc.select('Chunk,Job','Chunk.id,MAX(Job.id)','%s'%condition,verbose=self.verbose)
        chunk_id = job_id = []
        for couple in output:
            chunk_id.append(couple['id'])
            j=Job(data={'id':couple['MAX(Job.id)']},dbc = self.dbc)
            job_id.append(j.get_gw_job())
        task=gridwaylib.job()
        to=task.change_priority(priority,job_id)
                  
class Chunk(Component):
    """ 
    Chunk CLASS
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
   
    def set_wps(self, args):
        self.data['wps'] = args[ 0 ] 
        oc=self.update_fields(['wps'], ['id'])
        return oc     
   
    def set_status(self,st):
        self.data['status']=st
        oc=self.update_fields(['status'], ['id'])
        return oc   
    
    def get_status(self):
        status=self.get_one_field(['status'], ['id'])
        return status   
    
    def get_id_rea(self):
        id_rea=self.get_one_field(['id_rea'], ['id'])
        return id_rea     

    def get_id_chunk(self):
        id_chunk=self.get_one_field(['id_chunk'], ['id'])
        return id_chunk 
        
    def prepare_storage(self):
        pass
   
    def get_last_job(self):
        last_job=self.dbc.select('Job','MAX(Job.id)','Job.id_chunk=%s'%self.data['id'])
        lj=vdblib.parse_one_field(last_job)
        return int(lj)
    
class Job(Component):

    def get_reconfigurable_fields(self):
        return['resource','wn']

    def get_distinct_fields(self):
        return['gw_job','id_chunk']
    
    def stjob2stchunk(self):
        dst={'1':1, '10': 2, '40': 4, '41': 3}
        return dst
        
    def set_status(self, args ):
        st = args[ 0 ]
        self.data['status']=st
        oc=self.update_fields(['status'], ['id'])
        # If status involves any change in Chunk status, change the Chunk in DB
        dst=self.stjob2stchunk()
        
        if st in dst:
            Chunk(data={'id': self.get_id_chunk()},verbose=self.verbose, dbc = self.dbc).set_status(dst[st])
            
        # Add an Event in the DB
        timestamp=datetime2datewrf(datetime.utcnow())
        event_data= {'id_job': self.data['id'], 'status': st, 'timestamp': timestamp }
        id_event=Events(data=event_data, dbc = self.dbc).create()
        return oc   

    def get_status(self):
        status=self.get_one_field(['status'], ['id'])
        return status
    
    def get_info(self):
        jobd=self.dbc.select('Job','gw_job,status,exitcode,resource,wn','id=%s'%self.data['id'],verbose=self.verbose)
        return jobd[0]
            
    def set_exitcode(self, args ):
        exitcode = args[ 0 ]
        self.data['exitcode']=exitcode
        oc=self.update_fields(['exitcode'], ['id'])    
        return oc 
    
    def get_id_chunk(self):
        id_chunk=self.get_one_field(['id_chunk'], ['id'])
        return id_chunk
    
    def get_gw_job(self):
        gw_job=self.get_one_field(['gw_job'], ['id'])
        return gw_job

    def load_wn_conf(self, args ):
        wn_gwres=int( args[ 0 ] )
        cond=''
        for field in self.get_distinct_fields():
            cond="%s AND %s='%s'" %(cond,field,self.data[field])
        cond=cond[4:]
        ch=Chunk(data={'id': self.data['id_chunk']}, dbc = self.dbc)
        nchunk=int(ch.get_id_chunk())
        if nchunk > 1:            
            lch=Chunk(data={'id': str(int(self.data['id_chunk'])-1)}, dbc = self.dbc)
            lstatus=int(lch.get_status())
            if lstatus!=4:
                sys.stderr.write('Error: The previous Chunk did not finished correctly\n')
                sys.exit(93)
        last_job=int(ch.get_last_job())
        chunk_status=int(ch.get_status())
        if chunk_status==4:
            print "%d"%last_job
            sys.stderr.write('Error: Chunk %s already finished.\n'%data['id_chunk'])
            sys.exit(91) 

        # In this select statement we have the restriction of gw_job.
        jobd=self.dbc.select('Job','MAX(id),gw_restarted',cond,verbose=self.verbose)
        [max_id,db_gwres]=jobd[0].values()

        # If last_job is bigger than this job's id, then this job should not run.
        if last_job > int(max_id):
            sys.stderr.write('Error: This job should not be running this Chunk\n')
            sys.exit(92)

        if db_gwres < wn_gwres:
            self.data['gw_restarted']=wn_gwres
            id=self.create()
            self.data['id']=id
        elif db_gwres == wn_gwres:
            self.data['id']=str(max_id)
            id=self.update()
        else:
            sys.stderr.write('Error: This job should not be running this Chunk (restarted id)\n')
            sys.exit(92)
        self.set_status('10')
        return self.data['id']
        
    def get_hash(self):
        pass
    
    def list_events(self,evid=0):
        lev=self.dbc.select("Events","status,timestamp","id_job=%s"%self.data['id'])
        linfo=self.get_info()
        lstatus=JobStatus( self.dbc )
        i=0
        ltiempos={}
        for event in lev[:-1]:
            t=lev[i+1]['timestamp']-event['timestamp']
            ltiempos[event['status']]=t.seconds
            i=i+1
        linfo['tiempos']=ltiempos       
        return linfo

    def expvar(self,args):
        """
        Exports the environment variables of a resource file.
        """
        try:
            list_files = args[0:]
            resource_name  = os.environ.get('GW_HOSTNAME')
            for file in list_files :
                resource_env   = VarEnv( file )
                section_to_use = 'DEFAULT' 
                for section in resource_env.sections() :
                    if ':' in section :
                        _section = section.split( ':' , 1 )[ 1 ]
                    else :
                        _section = section
                    if resource_name.startswith( _section ) : 
                        section_to_use = section
                        break
                list_values = resource_env.items( section_to_use  )
                dict_values = dict( ( key , val ) for key, val in list_values ) 
                _KEYCRE = re.compile(r"\$\{([^}]+)\}")
                def update_list():
                    for key , val in  list_values  :
                        if "${" in val :
                            vars =_KEYCRE.findall( val )
                            for var in vars :
                                if var in dict_values :
                                    dict_values[ key ] = _KEYCRE.sub( dict_values[ var ] , val , count = 1 )
                [ update_list() for key , val in dict_values.items() if "${" in val ]
                with open( file , 'w' ) as f :
                    [ f.write( "export %s=%s\n" % ( key , val ) ) for key, val in dict_values.items() ]
            return 0
        except Exception, err:
            sys.stderr.write( str( err ) + '\n' )
            sys.exit( -1 )

class Events(Component):
    pass
   

class FrameWork( object ):
    
    def __init__( self ):
        db4g_vars       = VarEnv( DB4G_CONF ) 
        self.local_db   = db4g_vars.get_variable( 'WRF4G_DB_LOCAL' , 'Database' )
        self.mysql_port = db4g_vars.get_variable( 'WRF4G_DB_PORT' , 'Database' )
        self.gwd_pid    = join( GW_LOCATION    , 'var' , 'gwd.pid' )
        self.mysql_pid  = join( WRF4G_LOCATION , 'var' , 'mysql',  'mysql.pid' )
        self.mysql_sock = join( WRF4G_LOCATION , 'var' , 'mysql' , 'mysql.sock' )
        self.mysql_log  = join( WRF4G_LOCATION , 'var' , 'log'   , 'mysql.log' )
        
    def _process_is_runnig( self , pid_file ):
        with open( pid_file , 'r') as f:
            pid = f.readline()
        try:
            os.kill( int( pid.strip() ) , 0 )
        except :
            return False
        else:
            return True
  
    def _port_is_free( self , port ):
        import socket
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        if sock.connect_ex( ( '127.0.0.1', int( port ) ) ) is 0 :
            return False
        else :
            return True
  
    def status_drm4g( self ):
        if not exists( self.gwd_pid ) :
            print "DRM4G (GridWay) is stopped"
        elif self._process_is_runnig( self.gwd_pid ) :
            print "DRM4G (GridWay) is running"
        else :
            print "DRM4G (GridWay) is stopped"

    def status_database( self ):
        if not exists( self.mysql_pid ) :
            print "WRF4G_DB (MySQL) is not running"
        elif  self._process_is_runnig( self.mysql_pid ) :
            print "WRF4G_DB (MySQL) is running"
        else :
            print "WRF4G_DB (MySQL) is stopped"
  
    def start_drm4g( self ):
        print "Starting DRM4G .... "
        if not exists( self.gwd_pid ) or ( exists( self.gwd_pid ) and not self._process_is_runnig( self.gwd_pid ) ) :
            lock = join( GW_LOCATION , 'var' '/.lock' )
            if exists( lock ) : os.remove( lock )
            os.environ[ 'PATH' ] = '%s:%s' % ( GW_BIN_LOCATION , os.getenv( 'PATH' ) )
            exec_cmd = subprocess.Popen( join( GW_BIN_LOCATION , 'gwd' ) , 
                                         shell=True , 
                                         stdout=subprocess.PIPE ,
                                         stderr=subprocess.PIPE
                                         )
            out , err = exec_cmd.communicate()
            if err :
                print err 
            else :
                print "OK"
        else :
            print "WARNING: DRM4G is already running."
    
    def start_database( self ):        
        if self.local_db == '1' :
            print "Starting WRF4G_DB (MySQL) ... "
            if not self._port_is_free( self.mysql_port ) and not self._process_is_runnig( self.mysql_pid ):
                raise Exception( "WARNING: Another process is listening on port %s."
                  "Change WRF4G_DB_PORT in '%s' file to start MySQL on a different port." % ( self.mysql_port , DB4G_CONF )
                  )
            elif not exists( self.mysql_pid ) or ( exists( self.mysql_pid ) and not self._process_is_runnig( self.mysql_pid ) ) :
                mysql_options = "--no-defaults --port=%s --socket=%s --log-error=%s --pid-file=%s" % ( self.mysql_port ,
                                                                                                     self.mysql_sock ,
                                                                                                     self.mysql_log ,
                                                                                                     self.mysql_pid 
                                                                                                     )                         
                cmd = "cd %s ; nohup ./bin/mysqld_safe %s &>/dev/null &" % ( MYSQL_LOCATION , mysql_options )
                exec_cmd = subprocess.Popen( cmd  , 
                                             shell=True , 
                                             stdout=subprocess.PIPE,
                                             stderr=subprocess.PIPE
                                            )
                time.sleep( 1.0 )
                if not exists( self.mysql_pid ) or self._port_is_free( self.mysql_port ) :
                    print "ERROR: MySQL did not start, check '%s' for more information " % self.mysql_log 
                else :
                    print "OK"
            else :
                print "WARNING: MySQL is already running"
        else :
            print "You are using a remote WRF4G_DB (MySQL)"
                
    def stop_drm4g( self ):
        print "Stopping DRM4G .... "
        cmd_kill = "%s -k" % join( GW_BIN_LOCATION , "gwd" )
        exec_cmd = subprocess.Popen(  cmd_kill , 
                                      shell=True , 
                                      stdout=subprocess.PIPE,
                                      stderr=subprocess.PIPE,
                                      )
        out , err =  exec_cmd.communicate()
        if err :
            print err 
        else :
            print "OK"
            
    def stop_database( self ):        
        if self.local_db == '1' :
            print "Stopping WRF4G_DB (MySQL) ... "
            if not exists( self.mysql_pid ) or ( exists( self.mysql_pid ) and not self._process_is_runnig( self.mysql_pid ) ) :
                raise Exception( "WARNING: MySQL is already stopped." )
            elif exists( self.mysql_pid ) and self._process_is_runnig( self.mysql_pid ) :
                with open( self.mysql_pid , 'r') as f:
                    pid = f.readline().strip()
                exec_cmd = subprocess.Popen( "ps h -p %s -o ppid" % pid , 
                                             shell=True , 
                                             stdout=subprocess.PIPE,
                                             stderr=subprocess.PIPE,
                                             )
                mysql_ppid ,  err = exec_cmd.communicate()
                if err :
                    raise Exception( str( err ) )
                try :
                    os.kill( int( mysql_ppid ), signal.SIGKILL )
                    os.kill( int( pid ) , signal.SIGKILL )
                    print "OK"
                except Exception , err :
                    print "ERROR: stopping MySQL: %s" % str( err )
            else :
                print "WARNING: MySQL is already stopped."
        else :
            print "You are using a remote WRF4G_DB (MySQL)"
    
    def clear_drm4g( self ):
        self.stop_drm4g()
        print "Clearing DRM4G .... "
        cmd_kill = "%s -k" % join( GW_BIN_LOCATION , "gwd -c" )
        exec_cmd = subprocess.Popen(  cmd_kill , 
                                      shell=True , 
                                      stdout=subprocess.PIPE,
                                      stderr=subprocess.PIPE,
                                      )
        out , err =  exec_cmd.communicate()
        if err :
            print out , err 
        else :
            print "OK"
            
    def stop( self ):
        self.stop_drm4g()
        self.stop_database()  
      
    def start( self ):
        self.start_drm4g()
        self.start_database()       
        
    def status( self ):
        self.status_drm4g()
        self.status_database()       

class Proxy( object ):
    
    def __init__( self , resource , communicator , cred_lifetime , proxy_lifetime ):
        self.resource       = resource  
        self.communicator   = communicator
        self.cred_lifetime  = cred_lifetime
        self.proxy_lifetime = proxy_lifetime
        
    def upload( self ):
        print "\tCreating '%s' directory to store the proxy ... " % REMOTE_VOS_DIR
        cmd = "mkdir -p %s" % REMOTE_VOS_DIR
        print "\tExecuting command ... ", cmd 
        out, err = self.communicator.execCommand( cmd )
        if not err :
            message      = '\tInsert your GRID pass: '
            grid_passwd  = getpass.getpass(message)
        
            message      = '\tInsert MyProxy password: '
            proxy_passwd = getpass.getpass(message)
        
            if self.resource.has_key( 'myproxy_server' ) :
                cmd = "MYPROXY_SERVER=%s myproxy-init -S --cred_lifetime %d --proxy_lifetime %d" % (
                                                                   self.resource[ 'myproxy_server' ] ,
                                                                   self.cred_lifetime ,
                                                                   self.proxy_lifetime
                                                                   )
            else :
                cmd = "myproxy-init -S --cred_lifetime %d --proxy_lifetime %d" % (
                                                                                  self.cred_lifetime ,
                                                                                  self.proxy_lifetime
                                                                                  )
            print "\tExecuting command ... ", cmd 
            out , err = self.communicator.execCommand( cmd , input = '\n'.join( [ grid_passwd, proxy_passwd ] ) )
            print "\t", out , err
        else :
            print "\t", err
            
    def check( self ):
        if self.resource.has_key( 'myproxy_server' ) :
            cmd = "MYPROXY_SERVER=%s myproxy-info" % self.resource[ 'myproxy_server' ] 
        else :
            cmd = "myproxy-info"
        print "\tExecuting command ... ", cmd 
        out, err = self.communicator.execCommand( cmd )
        print "\t", out , err    
    
    def download( self ):
        message      = '\tInsert MyProxy password: '
        proxy_passwd = getpass.getpass(message)
        if self.resource.has_key( 'myproxy_server' ) :
            cmd = "X509_USER_PROXY=%s/%s MYPROXY_SERVER=%s myproxy-logon -S --proxy_lifetime %d" % (
                                                                                   REMOTE_VOS_DIR ,
                                                                                   self.resource[ 'myproxy_server' ] ,
                                                                                   self.resource[ 'myproxy_server' ] ,
                                                                                   self.cred_lifetime
                                                                                   ) 
        else :
            cmd = "X509_USER_PROXY=%s/${MYPROXY_SERVER} myproxy-logon -S --proxy_lifetime %d" % ( 
                                                                                                 REMOTE_VOS_DIR ,
                                                                                                 self.cred_lifetime
                                                                                                 )
        print "\tExecuting command ... ", cmd 
        out, err = self.communicator.execCommand( cmd , input = proxy_passwd )
        print "\t", out , err
    
    def destroy( self ):
        if self.resource.has_key( 'myproxy_server' ) :
            cmd = "MYPROXY_SERVER=%s myproxy-destroy" %  self.resource[ 'myproxy_server' ]                          
        else :
            cmd = "myproxy-destroy" % REMOTE_VOS_DIR 
        print "\tExecuting command ... ", cmd
        out , err = self.communicator.execCommand( cmd )
        print "\t", out , err
        
class Resource( object ):
    
    def __init__( self , config ):
        self.config = config
        
    def check_frontends( self ) :
        """
        Check if all frontends are reachable.
        """
        self.check_resources( )
        communicators = self.config.make_communicators()
        for resname, communicator in sorted( communicators.iteritems() ) :
            print "\t--> Resource '%s' ... " % resname
            try :
                communicator.connect()
            except Exception , err :
                print "\t\tThe front-end %s is not reachable" % communicator.frontend 
                print "\t\t" , err
            print "\t\tThe front-end %s is reachable" % communicator.frontend
            
    def edit_resources( self ) :
        """
        Edit reosurces file.
        """
        editor = os.environ.get('EDITOR', None)
        if not editor : 
            editor = 'vi'
        os.system( "%s %s" % ( editor , join( WRF4G_LOCATION , 'etc' , 'resources.conf' ) ) )


    def list_resources( self ) :
        """
        Check if the resource.conf file has been configured well and list the resources available.
        """
        self.check_resources( )
        print """\tName                          State
        ---------------------------------------------"""
        for resname, resdict in sorted( self.config.resources.iteritems() ) :
            if resdict[ 'enable' ] == 'True' :
                state = 'enabled'
            else :
                state = 'disabled'
            print "\t%-30.30s%s" % ( resname , state )
                    
    def resource_features( self ) :
        """
        List the features of a given resource.
        """
        self.check_resources( )
        for resname, resdict in sorted( self.config.resources.iteritems() ) :
            print "\t--> Resource '%s' ... " % resname
            for key , val in sorted( resdict.iteritems() ) :
                print "\t\t--> '%s' : '%s' " % ( key , val )        
    

    def check_resources( self ) :
        """
        Check if the resource.conf file has been configured well.
        """
        self.config.load()
        errors = self.config.check()
        if errors :
            message = '\n'.join( errors )
            raise Exception( "Please, review your configuration file\n%s" % message )
        

class Host( object ):
        
    def list( self , hid ) :
        """
        List the hosts and their features.
        """
        if hid is None :
            cmd_option = 'gwhost'
        else :
            cmd_option = 'gwhost ' + hid
        exec_cmd = subprocess.Popen( join ( GW_BIN_LOCATION , cmd_option ) , 
                                     shell=True , 
                                     stdout=subprocess.PIPE,
                                     stderr=subprocess.PIPE,
                                     )
        out, err = exec_cmd.communicate()
        if err :
            print out , err
        else :
            print out
