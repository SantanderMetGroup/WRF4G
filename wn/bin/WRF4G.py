#!/usr/bin/python

from sys import stderr,exit, path
path.reverse()
import sys
import inspect
import vdb
import vcp
from datetime import datetime
from optparse import OptionParser
import os



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

#===============================================================================
# def create_hash():
#    import md5
#    import random
#    rand=random.randint(1,60000)
#    text=str(datetime.utcnow()) + str(rand)
#    ha=md5.new()
#    ha.update(text)
#    return ha.hexdigest()
#===============================================================================
def create_hash():
    import random
    rand=random.randint(1,60000000)
    text=str(rand)
    return text

def JobStatus():
    dstatus=dbc.select('Jobstatus','id,description')
    sta={}
    for entry in dstatus:
        sta[entry['id']]=entry['description']
    return sta

def load_default_values():
    global NP,ENVIRONMENT,REQUIREMENTS,DB_HOST,DB_PORT,DB_USER,DB_PASSWD
    NP=1
    REQUIREMENTS=''
    ENVIRONMENT=''
    DB_HOST="ui01.macc.unican.es"
    DB_PORT=13306
    DB_USER="gridway"
    DB_PASSWD="ui01"
    DB_WRF4G="WRF4GDB"

def format_output(dout,ext=False):
    dreastatus={0:'P',1:'W',2:'R',3: 'F',4:'D'}
    djobstatus=JobStatus()
    if dout['exitcode']==None:
        exitcode='-'
    else:
        exitcode=str(dout['exitcode']) 

    runt=(int(dout['cdate'].strftime("%s"))-int(dout['sdate'].strftime("%s")))*100.    
    totalt=int(dout['edate'].strftime("%s"))-int(dout['sdate'].strftime("%s"))
    per=runt/totalt
    if dout['status'] < 10 and dout['status']>0 and dout['rea_status'] != 4:  dout['rea_status']=1  
    
    print '%-18s %-5s %-2s %-6s %-10s %-10s %-13s %2s %2.2f'%(dout['name'][0:17],dout['gw_job'],dreastatus[dout['rea_status']],dout['nchunks'],dout['resource'][0:10],dout['wn'][0:10],djobstatus[dout['status']],exitcode,per)
    if ext==True:
        print "%10s %10s %10s"%(dout['sdate'],dout['restart'],dout['edate'])    

class Component:
    """  Component CLASS
    """

    def __init__(self,data='',verbose=False,reconfigure=False,dryrun=False,rerun=False):
        #val={'no': False, 'yes': True}
        self.verbose=verbose
        self.reconfigure=reconfigure
        self.dryrun=dryrun
        self.rerun=rerun
        self.element=self.__class__.__name__
        self.data=data
        self.allfields=['','','','']        
        #for field in data.keys():            
        #    setattr(self,field,data[field])
            
    def describeDB(self):
        """    
        Returns a list with all the Component fields.
        """
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
     
     for field in list_query:
         wheresta="%s AND %s='%s'" %(wheresta,field,self.data[field])
     wheresta=wheresta[4:]    
     #dic=dbc.select(self.element,list2fields(fields), wheresta, verbose=1 )
     dic=dbc.select(self.element,list2fields(fields),wheresta, verbose=self.verbose )
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
        
        
        ddata={}
        for field in self.get_reconfigurable_fields():
            ddata[field]=self.data[field]
        
        condition='id=%s'%self.data['id']
        oc=dbc.update(self.element,ddata,condition,verbose=self.verbose)
        return oc
    
    def delete(self):
        """
        Delete a row of Component. It clears the DB and all the data related to it. 
        """
        if self.verbose: stderr.write("Deleting %s with id %s\n"%(self.element,self.data['id']))
        
        condition='id=%s'%self.data['id']
        o=dbc.delete_row(self.element,condition)
        return 0
    
    def get_one_field(self,val,cond):
        
        
        condition=''
        for field in cond:
          condition="%s AND %s='%s'" %(condition,field,self.data[field])
        condition=condition[4:]   
        
        dic=dbc.select(self.element,list2fields(val),condition, verbose=self.verbose )
        return dic[0][val[0]]

    def update_fields(self,val,cond):
        
        
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
                stderr.write('%s already exists. \n'%self.element)
                self.data['id']=-1
        else:
            if self.verbose: stderr.write('Creating %s\n'%self.element)
            self.data['id']=self.create()
            if not self.dryrun:
                self.prepare_storage()
                    
        return self.data['id']    

class Experiment(Component):
    
    """  Experiment CLASS
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
        
        idp=dbc.select(self.element,'id',"name='%s'"%self.data['name'],verbose=self.verbose)
        id = vdb.list_query().one_field(idp)
        if id !='': return id
        else: return -1

    def get_name(self):
        name=self.get_one_field(['name'], ['id'])
        return name
           
    def run(self,nrea=0,nchunk=0,priority=0):
        ncrea=0
        rea_ids=self.get_realizations_id()
        if len(rea_ids) == 0:
            stderr.write('There are not realizations to run.\n')
        
        for id_rea in rea_ids:
            rea=Realization(data={'id': str(id_rea)},verbose=self.verbose,dryrun=self.dryrun)
            st=rea.run(nchunk=nchunk)
            if st==1:
                ncrea=ncrea+1
                if ncrea==nrea:
                    break
     
    def get_basepath(self):
        cdate=self.get_one_field(['basepath'], ['id'])
        return cdate           
    
    def ps(self):
        self.data['id']=self.get_id_from_name()
        if self.data['id'] < 0:     sys.exit(19)
        rea_ids=self.get_realizations_id()
        dout=[]
        print '%-18s %-3s %-4s %-6s %-10s %-10s %-13s %2s %3s'%('Realization','GW','Stat','Chunks','Comp.Res','WN','Run.Sta','ext','%')        
        for id_rea in rea_ids:
            rea=Realization(data={'id': str(id_rea)},verbose=self.verbose,dryrun=self.dryrun)
            dout=rea.ps()
            format_output(dout)
        return ''
                      
    def get_realizations_id(self):
        """    
        Query database and Returns a list with the realization
        ids of an experiment"""        
        
        idp=dbc.select('Realization','id','id_exp=%s'%self.data['id'],verbose=self.verbose)
        ids_rea = vdb.list_query().one_field(idp,'python')
        return ids_rea 
    
    def prepare_storage(self):          
        RESOURCES_WRF4G=os.environ.get('RESOURCES_WRF4G')
        if RESOURCES_WRF4G == None:
            sys.stderr.write('RESOURCES_WRF4G is not defined. Please define it and try again\n')
            sys.exit(1)
        exec open(RESOURCES_WRF4G).read()     
        # Load the URL into the VCPURL class
        exp_dir="%s/%s" %(WRF4G_BASEPATH, self.data['name'])
        list=vcp.VCPURL(exp_dir)
        list.mkdir(verbose=self.verbose)
        if os.path.exists('wrf4g_files'):
            import tarfile
            os.chdir('wrf4g_files')
            tFile = tarfile.open('../wrf4g_files.tar.gz', 'w:gz')
            for file in os.listdir('.'):
                tFile.add(file)
            tFile.close()
            os.chdir('..')
            output=vcp.copy_file('wrf4g_files.tar.gz',exp_dir,verbose=self.verbose)    
            os.remove('wrf4g_files.tar.gz')  
            
        output=vcp.copy_file(DB4G_CONF,exp_dir,verbose=self.verbose)
        output=vcp.copy_file(RESOURCES_WRF4G,exp_dir,verbose=self.verbose) 
        output=vcp.copy_file('experiment.wrf4g',exp_dir,verbose=self.verbose) 
         
class Realization(Component):
    """ Realization CLASS
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
    
    def get_restart(self):
        restart=self.get_one_field(['restart'], ['id'])
        if restart != None:
           restart=datetime2datewrf(restart)
        return restart
        
    def set_restart(self,restart_date):
        self.data['restart']=restart_date
        oc=self.update_fields(['restart'], ['id'])    
        return oc

    def get_cdate(self):
        cdate=self.get_one_field(['cdate'], ['id'])
        return cdate

    def get_exp_id(self):
        id_exp=self.get_one_field(['id_exp'], ['id'])
        return id_exp
   
    def set_cdate(self,cdate):
        self.data['cdate']=cdate
        self.data['ctime']=timestamp=datetime2datewrf(datetime.utcnow())
        oc=self.update_fields(['cdate','ctime'], ['id'])    
        return oc
    
    def has_finished(self):
        lchunk=self.last_chunk()
        status=Chunk(data={'id': '%s'%lchunk}).get_status()
        if status == 4:
            finished=True
        else:
            finished=False
        return finished
    

    def get_init_status(self):
        
        dst=dbc.select('Chunk','status','id_rea=%s AND id_chunk=1'%self.data['id'] )
        st=vdb.parse_one_field(dst)
        return st
    
    def current_chunk_status(self):
        status=0
        nchunks=self.number_of_chunks()
        failed=dbc.select('Chunk','MIN(id_chunk)','id_rea=%s AND status=3'%self.data['id'] )
        id_chunk=vdb.parse_one_field(failed)
        
        if id_chunk:     
            status=3
        else:
            did=dbc.select('Chunk','MAX(id_chunk)','id_rea=%s AND status=4'%self.data['id'])
            id_chunk=vdb.parse_one_field(did)
            if id_chunk == None:
                id_chunk=0
            if id_chunk == nchunks:
                status=4
            else:
                id_chunk=id_chunk + 1
        
        ch=Chunk(data={'id_chunk':id_chunk,'id_rea':self.data['id']})
        id=ch.get_one_field(['id'], ['id_chunk','id_rea'])
        if status !=4 and status !=3:
            status=ch.get_one_field(['status'], ['id_chunk','id_rea'])
        return (id,id_chunk,int(status))    
        
    def has_failed(self):
        
        nchunkd=dbc.select('Chunk','id','id_rea=%s AND status=3'%self.data['id'] )
        if nchunkd != ():     
            return True
        else:
            return False 
    
    def number_of_chunks(self):
        
        nchunkd=dbc.select('Chunk','COUNT(Chunk.id_chunk)','id_rea=%s'%self.data['id'])
        nchunk=vdb.parse_one_field(nchunkd)
        return nchunk
        
    def last_chunk(self):        
        nchunkd=dbc.select('Chunk','MAX(Chunk.id)','id_rea=%s'%self.data['id'])
        nchunk=vdb.parse_one_field(nchunkd)
        return nchunk 
         
    def run(self,nchunk=0,priority=0):
        import gridway   
        self.prepared=0

        if 'id' in self.data.keys():
           self.data['name']=self.get_name()
           rea_name=self.data['name']
        else:
            rea_name=self.data['name']
            self.data['id']=self.get_id(['name'])
            if self.data['id']== -1:
                sys.stderr.write('Realization with name %s does not exists \n'%rea_name)
                sys.exit(1)    
        
        #restart=self.get_restart()
        
        chunk_status=self.current_chunk_status()
        
        first_id=chunk_status[0]
        first_id_chunk=chunk_status[1]
        
        if self.rerun:
            first_id=first_id-first_id_chunk+1     
            first_id_chunk=1
        else:
            if chunk_status[2] == 4:
                stderr.write('Realization %s already finished.\n'%rea_name)
                return 1
            elif chunk_status[2] == 2:
                stderr.write('Realization %s is still running.\n'%rea_name)
                return 1
        """
        if restart == None:
            chunkd=dbc.select('Chunk','MIN(Chunk.id_chunk),MIN(Chunk.id)','id_rea=%s'%self.data['id'],verbose=self.verbose)
        else:
            chunkd=dbc.select('Chunk,Realization','Chunk.id_chunk,Chunk.id','Chunk.id_rea=%s AND Chunk.id_rea=Realization.id AND Realization.restart >=Chunk.sdate AND Realization.restart<Chunk.edate'%self.data['id'],verbose=self.verbose)
            if chunkd== ():
                stderr.write('DB inconsistency.')
                return 2
        [first_id_chunk,first_id]=chunkd[0].values()
        """
        
        lchunk=self.last_chunk()
        if nchunk != 0 and lchunk-first_id > nchunk: 
            lchunk=first_id+int(nchunk)-1

        for chunki in range(first_id,lchunk+1):
            chi=Chunk(data={'id':'%s'%chunki})
            chi.loadfromDB(['id'],chi.get_configuration_fields())
            arguments='%s %s %s %d %d %s %s'%(self.get_exp_name(),rea_name,self.data['id'] ,chi.data['id_chunk'],chi.data['id'],datetime2datewrf(chi.data['sdate']),datetime2datewrf(chi.data['edate']))
            
            if chunki == first_id: 
                stderr.write('Submitting realization: "%s"\n'%(rea_name))
            stderr.write('\tSubmitting Chunk %d:\t%s\t%s\n'%(chi.data['id_chunk'],datetime2datewrf(chi.data['sdate']),datetime2datewrf(chi.data['edate'])))
    
            if self.dryrun == False:      
                self.prepare_gridway()   
                exec open('resources.wrf4g').read()     
                job=gridway.job()
                sandbox='file://%s/etc/templates/WRF4G.sh,file://%s/bin/vcp,file://%s/etc/db4g.conf,resources.wrf4g'%(WRF4G_LOCATION,WRF4G_LOCATION,WRF4G_LOCATION)
                job.create_template(rea_name + '__' + str(chi.data['id_chunk']),arguments,np=NP,req=REQUIREMENTS,environ=ENVIRONMENT,inputsandbox=sandbox)
                if chunki == first_id:
                    gw_id=job.submit()
                else:
                    gw_id=job.submit(dep=gw_id)
                job_data={'gw_job': gw_id,'id_chunk': str(chi.data['id']), 'hash': create_hash()}
                job=Job(job_data,verbose=self.verbose)
                jid=job.create()
                job.set_status('1')
                dbc.commit()
        return 0

    def get_exp_name(self):
         if 'id' in self.data.keys():
           self.data['name']=self.get_name()
         else:
            self.data['id']=self.get_id(['name'])
         cexp=Experiment(data={'id': self.get_exp_id()})
         exp_name=cexp.get_name()
         return exp_name
        
    def prepare_gridway(self):
        if self.prepared == 0: 
            rea_name=self.data['name']
            exp_id=self.get_exp_id()
            exp_name=self.get_exp_name()
            cexp=Experiment(data={'id': exp_id})
            WRF4G_BASEPATH=cexp.get_basepath()
            subdir= '%s/%s/%s/%s'%(WRF4G_LOCATION,'.submission',exp_name,rea_name)       
            if not os.path.isdir(subdir):
                os.makedirs(subdir)
            os.chdir(subdir)
            rea_input='%s/%s/%s'%(WRF4G_BASEPATH,exp_name,rea_name)
            rea_vcp=vcp.VCPURL(rea_input)
            orea=rea_vcp.ls('resources.wrf4g')
            if 'resources.wrf4g' in orea:
                output=vcp.copy_file('%s/%s'%(rea_input,'resources.wrf4g'),'.',verbose=self.verbose)
            else:
                output=vcp.copy_file('%s/%s/%s'%(WRF4G_BASEPATH,exp_name,'resources.wrf4g'),'.',verbose=self.verbose)           
            self.prepared=1   
        
    def ps(self):
        only_rea=0
        if 'id' in self.data.keys():
           self.data['name']=self.get_name()
        else:
            self.data['id']=self.get_id(['name'])
            only_rea=1
            if self.data['id']== -1:
                sys.stderr.write('Realization with name %s does not exists \n'%rea_name)
                sys.exit(1)            
        drea=dbc.select('Realization','name,restart,sdate,cdate,edate','id=%s'%self.data['id'])
        dout=drea[0]
        nchunks=self.number_of_chunks()
        status=self.get_init_status()
        
        if status == 0:
            djob={'gw_job': '-', 'status': 0, 'wn': '-', 'resource': '-', 'exitcode': None,'nchunks':'0/%d'%nchunks}
        else:
            (id_chunk,current_chunk,status)=self.current_chunk_status()            
            #(id_chunk,status)=self.current_chunk_status()                
            ch=Chunk(data={'id': id_chunk}) 
            lastjob=ch.get_last_job()
            j=Job(data={'id':lastjob})            
            djob=j.get_info()
            if djob['status'] < 10:
                djob={'gw_job': djob['gw_job'], 'status': djob['status'], 'wn': '-', 'resource': '-', 'exitcode': None,'nchunks':'0/%d'%nchunks}
            djob['nchunks']='%d/%d'%(current_chunk,nchunks)
        dout.update(djob)
        dout['rea_status']=status        
        #dout={'gw_job': 4L, 'status': 40L, 'sdate': datetime.datetime(1983, 8, 25, 12, 0), 'resource': 'mycomputer', 'name': 'testc1', 'wn': 'sipc18', 'nchunks': '3/3', 'cdate': datetime.datetime(1983, 8, 25, 12, 0), 'exitcode': 0L, 'edate': datetime.datetime(1983, 8, 27, 0, 0), 'restart': datetime.datetime(1983, 8, 27, 0, 0), 'rea_status': 4}]
        if only_rea == 1 :
            print '%-18s %-3s %-4s %-6s %-10s %-10s %-13s %2s %3s'%('Realization','GW','Stat','Chunks','Comp.Res','WN','Run.Sta','ext','%')
            format_output(dout)
            return ""
        else:
            return dout
                           
    def prepare_storage(self):          
        RESOURCES_WRF4G=os.environ.get('RESOURCES_WRF4G')
        if RESOURCES_WRF4G == None:
            sys.stderr.write('RESOURCES_WRF4G is not defined. Please define it and try again\n')
            sys.exit(1)
        exec open(RESOURCES_WRF4G).read()        
        reas=self.data['name'].split('__')
        rea_dir="%s/%s/%s" % (WRF4G_BASEPATH,reas[0],self.data['name'])
        for dir in ["output","restart","realout","log"]:
          repo="%s/%s" % (rea_dir,dir)
          list=vcp.VCPURL(repo)
          list.mkdir(verbose=self.verbose)
        output=vcp.copy_file('namelist.input',rea_dir,verbose=self.verbose) 
        #os.remove('namelist.input') 
        #os.remove('namelist.input.base')
                             
    def is_finished(self,id_rea):
        
        max_id=dbc.select('Chunk','MAX(id)','id_rea=%s'%id_rea,verbose=self.verbose)
        status=dbc.select('Chunk','status','id=%s'%vdb.parse_one_field(max_id),verbose=self.verbose)
        if status == '4':
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
    
    def get_status(self):
        status=self.get_one_field(['status'], ['id'])
        return status   
    
    def get_id_rea(self):
        id_rea=self.get_one_field(['id_rea'], ['id'])
        return id_rea     
        
    def prepare_storage(self):
        pass
   
    def get_last_job(self):
        last_job=dbc.select('Job,Chunk','MAX(Job.id)','Chunk.id=%s AND Job.id_chunk=Chunk.id'%self.data['id'])
        lj=vdb.parse_one_field(last_job)
        return lj
    
    
class Job(Component):

    def get_reconfigurable_fields(self):
        return['resource','wn']

    def get_distinct_fields(self):
        return['gw_job','id_chunk']
    
    def stjob2stchunk(self):
        dst={'1':1, '10': 2, '40': 4, '41': 3}
        return dst
        
    def set_status(self,st):
        self.data['status']=st
        oc=self.update_fields(['status'], ['id'])
        # If status involves any change in Chunk status, change the Chunk in DB
        dst=self.stjob2stchunk()
        
        if st in dst:
            Chunk(data={'id': self.get_id_chunk()},verbose=self.verbose).set_status(dst[st])
            
        # Add an Event in the DB
        timestamp=datetime2datewrf(datetime.utcnow())
        event_data= {'id_job': self.data['id'], 'status': st, 'timestamp': timestamp }
        id_event=Events(data=event_data).create()
        return oc   

    def get_status(self):
        status=self.get_one_field(['status'], ['id'])
        return status
    
    def get_info(self):
        jobd=dbc.select('Job','gw_job,status,exitcode,resource,wn','id=%s'%self.data['id'],verbose=self.verbose)
        return jobd[0]
            
    def set_exitcode(self,exitcode):
        self.data['exitcode']=exitcode
        oc=self.update_fields(['exitcode'], ['id'])    
        return oc 
    
    def get_id_chunk(self):
        id_chunk=self.get_one_field(['id_chunk'], ['id'])
        return id_chunk

    def load_wn_conf(self,wn_gwres):
        wn_gwres=int(wn_gwres)
        cond=''
        for field in self.get_distinct_fields():
            cond="%s AND %s='%s'" %(cond,field,self.data[field])
        cond=cond[4:]
        ch=Chunk(data={'id': self.data['id_chunk']})
        last_job=int(ch.get_last_job())
        chunk_status=int(ch.get_status())
        if chunk_status==4:
            print "%d"%last_job
            stderr.write('Error: Chunk %s already finished.\n'%data['id_chunk'])
            exit(91) 

        # In this select statement we have the restriction of gw_job.
        jobd=dbc.select('Job','MAX(id),gw_restarted',cond,verbose=self.verbose)
        [max_id,db_gwres]=jobd[0].values()

        # If last_job is bigger than this job's id, then this job should not run.
        if last_job > max_id:
            stderr.write('Error: This job should not be running this Chunk\n')
            exit(92)

        if db_gwres < wn_gwres:
            self.data['gw_restarted']=wn_gwres
            id=self.create()
            self.data['id']=id
        elif db_gwres == wn_gwres:
            self.data['id']=str(max_id)
            id=self.update()
        else:
            stderr.write('Error: This job should not be running this Chunk (restarted id)\n')
            exit(92)
        self.set_status('10')
        return self.data['id']
        
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
    parser.add_option("-n", "--dry-run",action="store_true", dest="dryrun", default=False,help="Perform a trial run with no changes made")
    parser.add_option("-R", "--rerun",action="store_true", dest="rerun", default=False,help="Rerun an experiment or realization although it has been finished.")
    (options, args) = parser.parse_args()
    
    if len(args) < 2:
        parser.error("Incorrect number of arguments")
        exit(1)
        
    class_name=args[0]
    function=args[1]
    load_default_values()
    DB4G_CONF=os.environ.get('DB4G_CONF')
    if DB4G_CONF == None:
        sys.stderr.write('DB4G_CONF is not defined. Please define it and try again\n')
        sys.exit(1)
    exec open(DB4G_CONF).read()
    
    WRF4G_LOCATION=os.environ.get('WRF4G_LOCATION')
    if DB4G_CONF == None:
        sys.stderr.writpathgridwaye('WRF4G_LOCATION is not defined. Please define it and try again\n')
        sys.exit(1)   
   
    data=''
    dbc=vdb.vdb(host=DB_HOST, user=DB_USER, db=DB_WRF4G, port=DB_PORT, passwd=DB_PASSWD)
    if len(args) > 2:   data=pairs2dict(args[2])         
    inst="%s(data=%s,verbose=options.verbose,reconfigure=options.reconfigure,dryrun=options.dryrun,rerun=options.rerun)"%(class_name,data)
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
    
    if options.dryrun:
        dbc.rollback()
    else:
        dbc.commit()
    dbc.close()





   
