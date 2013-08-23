#!/usr/bin/env python

import inspect
import os
import sys
from datetime import datetime
from os.path import join, basename, dirname
from re import search, match
import vdblib
import vcplib 

WRF4G_LOCATION=os.environ.get('WRF4G_LOCATION')
GW_LOCATION ='%s/opt/drm4g_gridway'% WRF4G_LOCATION

#Configure WRF4GF_DB
DB4G_CONF=os.environ.get('DB4G_CONF')
try:
    exec open(DB4G_CONF).read() 
    dbc=vdblib.vdb(host=WRF4G_DB_HOST,
               user=WRF4G_DB_USER,
               db=WRF4G_DB_DATABASE,
               port=WRF4G_DB_PORT,
               passwd=WRF4G_DB_PASSWD)
except Exception, e:
    sys.stderr.write('Caught exception: %s\n' % (str(e)))

class wrffile :
    """
    This class manage the restart and output files and the dates they represent.
    It recieves a file name with one of the following shapes: wrfrst_d01_1991-01-01_12:00:00 or
    wrfrst_d01_19910101T120000Z and it return the date of the file, the name,...
    """

    def __init__(self, url, edate=None):
        """
        Change the name of the file in the repository (Change date to the iso format
        and add .nc at the end of the name
        """
        # wrfrst_d01_1991-01-01_12:00:00
        if edate:
            self.edate = datewrf2datetime(edate)

        g = search("(.*)(\d{4}-\d{2}-\d{2}_\d{2}:\d{2}:\d{2})", url)
        if g:
            base_file, date_file = g.groups()
            self.date = datewrf2datetime(date_file)
        else :
            # wrfrst_d01_19910101T120000Z.nc
            g = search("(.*)(\d{8}T\d{6}Z)", url)
            if not g:
                out="File name is not well formed"
                raise Exception(out)
            else :
                base_file, date_file = g.groups()
                self.date = dateiso2datetime(date_file)
        self.file_name = basename(base_file)
        self.dir_name = dirname(base_file)

    def date_wrf(self):
        return datetime2datewrf(self.date)

    def date_iso(self):
        return datetime2dateiso(self.date)

    def file_name_wrf(self):
        return self.file_name + datetime2datewrf(self.date)

    def file_name_iso(self):
        return "%s%s.nc" % (self.file_name,datetime2dateiso(self.date))

    def file_name_out_iso(self):
        return "%s%s_%s.nc" % (self.file_name, datetime2dateiso(self.date), datetime2dateiso(self.edate))


############################## FUNCTIONS FOR MANAGE DATES ################################
def datewrf2datetime (datewrf):
    g = match("(\d{4})-(\d{2})-(\d{2})_(\d{2}):(\d{2}):(\d{2})", datewrf)
    if not g :
        raise Exception("Date is not well formed")
    date_tuple = g.groups()
    date_object = datetime(*tuple(map(int, date_tuple)))
    return date_object

def dateiso2datetime (dateiso):
    g = match("(\d{4})(\d{2})(\d{2})T(\d{2})(\d{2})(\d{2})Z", dateiso)
    if not g :
        raise Exception("Date is not well formed")
    date_tuple = g.groups()
    date_object = datetime(*tuple(map(int, date_tuple)))
    return date_object

def datetime2datewrf (date_object):
    return date_object.strftime("%Y-%m-%d_%H:%M:%S")

def datetime2dateiso (date_object):
    return date_object.strftime("%Y%m%dT%H%M%SZ")

############################################################################################

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
    global NP,ENVIRONMENT,REQUIREMENTS,WRF4G_DB_HOST,WRF4G_DB_PORT,WRF4G_DB_USER,WRF4G_DB_PASSWD
    NP=1
    REQUIREMENTS=''
    ENVIRONMENT=''
    
load_default_values()

def format_output(dout,number_of_characters, ext=False):
    dreastatus={0:'P',1:'W',2:'R',3: 'F',4:'D'}
    djobstatus=JobStatus()
    if dout['exitcode']==None:
        exitcode='-'
    else:
        exitcode=str(dout['exitcode']) 

    runt=(int(dout['cdate'].strftime("%s"))-int(dout['sdate'].strftime("%s")))*100.    
    totalt=int(dout['edate'].strftime("%s"))-int(dout['sdate'].strftime("%s"))
    per=runt/totalt
    if dout['status'] < 10 and dout['status'] > 0 and dout['rea_status'] != 4: 
        dout['rea_status']=1  
    string_to_print = '%-'+ str(number_of_characters) + 's %-5s %-2s %-15s %-10s %-10s %-13s %2s %2.2f'
    return  string_to_print  % (dout['name'][0:number_of_characters],
                                dout['gw_job'],
                                dreastatus[dout['rea_status']],
                                dout['nchunks'],
                                dout['resource'][0:10],
                                dout['wn'][0:10],
                                djobstatus[dout['status']],
                                exitcode,
                                per)
        
def getuserid(name,dbc):
    idp=dbc.select('User','id',"name='%s'"%name)
    if len(idp) == 0:
        id=dbc.insert('User',{'name': name})
    else:
        id = vdblib.list_query().one_field(idp)
    return id      

class Environment:
    def __init__(self):
        pass
    
    def list_experiments(self):
        idp=dbc.select('Experiment','id','1=1')
        id = vdblib.parse_one_list(idp,interpreter='python')
        return id
        
class Component:
    """
    Component CLASS
    """

    def __init__(self,data='',verbose=False,dryrun=False,reconfigure=False):
        self.verbose=verbose
        self.dryrun=dryrun
        self.reconfigure=reconfigure
        self.element=self.__class__.__name__
        self.data=data   
            
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
        if self.verbose: 
            sys.stderr.write("Deleting %s with id %s\n"%(self.element,self.data['id']))
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
                else:                
                    self.prepare_storage()
        else:
            if self.verbose: 
                sys.stderr.write('Creating %s\n'%self.element)
            if self.element == "Experiment":
                import getpass
                username=getpass.getuser()
                userid=getuserid(username,dbc)
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
        
        idp=dbc.select(self.element,'id',"name='%s'"%self.data['name'],verbose=self.verbose)
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
            rea=Realization(data={'id': str(id_rea)},verbose=self.verbose,dryrun=self.dryrun)
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
            rea=Realization(data={'id': str(id_rea)},verbose=self.verbose,dryrun=self.dryrun)
            rea.ps(number_of_characters)
    
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
        
        idp=dbc.select('Realization','id','id_exp=%s'%self.data['id'],verbose=self.verbose)
        ids_rea = vdblib.list_query().one_field(idp,'python')
        return ids_rea 
    
    def get_unfinishedreas_id(self):
        """
        Return a list of ids of the realization of a experiment which haven't finished.
        """
        idp=dbc.select('Chunk,Realization','DISTINCT Realization.id','Chunk.status!=4 AND Realization.id=Chunk.id_rea AND Realization.id_exp=%s'%self.data['id'],verbose=self.verbose)
        ids_rea = vdblib.list_query().one_field(idp,'python')
        return ids_rea

    def get_run_reas_id(self):
        """
        Return a list of ids of the realization of a experiment which are running.
        """
        idp=dbc.select('Chunk,Realization','DISTINCT Realization.id','Chunk.status=2 AND Realization.id=Chunk.id_rea AND Realization.id_exp=%s AND Chunk.id_rea not in (select c2.id_rea From Chunk as c2 where c2.id_rea = Realization.id AND c2.status=3)'%self.data['id'],verbose=self.verbose)
        ids_rea = vdblib.list_query().one_field(idp,'python')
        return ids_rea      
    
    def get_fail_reas_id(self):
        """
        Return a list of ids of the realization of a experiment which have finished. Any of the Realization Chunks have the status 3.
        """
        idp=dbc.select('Chunk,Realization','DISTINCT Realization.id','Chunk.status=3 AND Realization.id=Chunk.id_rea AND Realization.id_exp=%s'%self.data['id'],verbose=self.verbose)
        ids_rea = vdblib.list_query().one_field(idp,'python')
        return ids_rea  

    def get_prepared_reas_id(self):
        """
        Return a list of ids of the realization of a experiment which are prepared. Every Chunk in the Realization has status 0.
        """
        # SELECT DISTINCT(Realization.id) From Chunk,Realization where Realization.id=Chunk.id_rea AND Realization.id_exp=2 and Chunk.status = 1 and Chunk.id_rea not in (select c2.id_rea From Chunk as c2 where c2.id_rea = Chunk.id_rea AND c2.status!=1)
        condition='Realization.id=Chunk.id_rea AND Realization.id_exp=%s and Chunk.status = 0 and Chunk.id_rea not in (select c2.id_rea From Chunk as c2 where c2.id_rea = Realization.id AND (c2.status!=0 and c2.status!=4))'%self.data['id']
        idp=dbc.select('Chunk,Realization','DISTINCT Realization.id',condition,verbose=self.verbose)
        ids_rea = vdblib.list_query().one_field(idp,'python')
        return ids_rea   

    def get_done_reas_id(self):
        """
        Return a list of ids of the realization of a experiment which have finished. Every Chunk in the Realization has status 4.
        """
        # SELECT DISTINCT(Realization.id) From Chunk,Realization where Realization.id=Chunk.id_rea AND Realization.id_exp=2 and Chunk.status = 1 and Chunk.id_rea not in (select c2.id_rea From Chunk as c2 where c2.id_rea = Chunk.id_rea AND c2.status!=1)
        condition='Realization.id=Chunk.id_rea AND Realization.id_exp=%s and Chunk.status = 4 and Chunk.id_rea not in (select c2.id_rea From Chunk as c2 where c2.id_rea = Realization.id AND c2.status!=4)'%self.data['id']
        idp=dbc.select('Chunk,Realization','DISTINCT Realization.id',condition,verbose=self.verbose)
        ids_rea = vdblib.list_query().one_field(idp,'python')
        return ids_rea

    def get_wait_reas_id(self):
        """
        Return a list of ids of the realization of a experiment which have finished.
        """
        idp=dbc.select('Chunk,Realization',
                       'DISTINCT Realization.id',
                       'Chunk.status=1 AND Realization.id=Chunk.id_rea AND Realization.id_exp=%s AND Chunk.id_rea not in (select c2.id_rea From Chunk as c2 where c2.id_rea = Realization.id AND (c2.status=2 OR c2.status=3 ) )'%self.data['id'],
                       verbose=self.verbose)
        ids_rea = vdblib.list_query().one_field(idp,'python')
        return ids_rea  
    
    def prepare_storage(self):          
        RESOURCES_WRF4G=os.environ.get('RESOURCES_WRF4G')
        if RESOURCES_WRF4G == None:
            sys.stderr.write('RESOURCES_WRF4G is not defined. Please define it and try again\n')
            sys.exit(1)
        exec open(RESOURCES_WRF4G).read()     
        # Load the URL into the VCPURL class
        exp_dir="%s/%s/" %(WRF4G_BASEPATH, self.data['name'])
        vcplib.VCPURL(exp_dir).mkdir(verbose=self.verbose)
        if os.path.exists('wrf4g_files'):
            import tarfile
            tFile = tarfile.open('wrf4g_files.tar.gz', 'w:gz')
            tFile.add(file)
            tFile.close()
            vcplib.copy_file('wrf4g_files.tar.gz',exp_dir,verbose=self.verbose)    
            os.remove('wrf4g_files.tar.gz')  
        vcplib.copy_file(DB4G_CONF,exp_dir,verbose=self.verbose)
        vcplib.copy_file(RESOURCES_WRF4G,exp_dir,verbose=self.verbose) 
        vcplib.copy_file('experiment.wrf4g',exp_dir,verbose=self.verbose)
        if os.path.exists('prolog.wrf4g'):
            vcplib.copy_file('prolog.wrf4g',exp_dir,verbose=self.verbose)  
         
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
        idp=dbc.select(self.element,'id',"name='%s'"%self.data['name'],verbose=self.verbose)
        id = vdblib.list_query().one_field(idp)
        self.data['id']=id
        if id !='': return id
        else: return -1
        
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
        st=vdblib.parse_one_field(dst)
        return st
    
    def current_chunk_status(self):
        status=0
        nchunks=self.number_of_chunks()
        failed=dbc.select('Chunk','MIN(id_chunk)','id_rea=%s AND status=3'%self.data['id'] )
        id_chunk=vdblib.parse_one_field(failed)
        
        if id_chunk:     
            status=3
        else:
            did=dbc.select('Chunk','MAX(id_chunk)','id_rea=%s AND status=4'%self.data['id'])
            id_chunk=vdblib.parse_one_field(did)
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
        nchunk=vdblib.parse_one_field(nchunkd)
        return nchunk
        
    def last_chunk(self):        
        nchunkd=dbc.select('Chunk','MAX(Chunk.id)','id_rea=%s'%self.data['id'])
        nchunk=vdblib.parse_one_field(nchunkd)
        return nchunk 
         
    def run(self,nchunk=0,priority=0,rerun=False,force=False,repeatchunk=0,type_dep="afterany"):
        import gridwaylib   
        
        self.prepared=0
        rea_name=self.get_name()       
      
        chunk_status=self.current_chunk_status()
        first_id=chunk_status[0]
        first_id_chunk=chunk_status[1]

        if rerun:
            if repeatchunk != 0:
               first_id_chunk=repeatchunk
               chunkd=dbc.select('Chunk','id,sdate','id_rea=%s AND id_chunk=%d'%(self.data['id'],repeatchunk),
                                 verbose=self.verbose)
               first_id=chunkd[0]['id']
               sdate=chunkd[0]['sdate']              
               self.set_restart(sdate)
               self.set_cdate(sdate)
            else:
               first_id=first_id-first_id_chunk+1     
               first_id_chunk=1
               self.set_restart(None)
               cdate=self.get_one_field(['sdate'], ['id'])
               self.set_cdate(cdate)
        else:
            if chunk_status[2] == 4:
                sys.stderr.write('Realization %s already finished.\n'%rea_name)
                return 1
            if not force and ( chunk_status[2] == 2 or chunk_status[2] == 1) :
                sys.stderr.write('Realization %s is still submitting. Use --force if you really want to submit the realization.\n'%rea_name)
                return 1
        if repeatchunk == 0:
            lchunk=self.last_chunk()
            if nchunk != 0 and lchunk-first_id >= nchunk: 
                lchunk=first_id+nchunk-1
        else:
            lchunk=first_id

        for chunki in range(first_id,lchunk+1):
            chi=Chunk(data={'id':'%s'%chunki})
            chi.loadfromDB(['id'],chi.get_configuration_fields())
            arguments='%s %s %s %d %d %s %s'%(self.get_exp_name(),
                                              rea_name,self.data['id'],
                                              chi.data['id_chunk'],
                                              chi.data['id'],
                                              datetime2datewrf(chi.data['sdate']),
                                              datetime2datewrf(chi.data['edate']))
            
            if chunki == first_id: 
                sys.stderr.write('Submitting realization: "%s"\n'%(rea_name))
            sys.stderr.write('\tSubmitting Chunk %d:\t%s\t%s\n' % (chi.data['id_chunk'],
                                                                   datetime2datewrf(chi.data['sdate']),
                                                                   datetime2datewrf(chi.data['edate'])))
    
            if self.dryrun == False:      
                self.prepare_gridway()   
                exec open('resources.wrf4g').read()     
                job=gridwaylib.job()
                sandbox='file://%s/etc/templates/WRF4G.sh,file://%s/etc/templates/WRF4G-%s.tar.gz,file://%s/etc/db4g.conf,resources.wrf4g'%(WRF4G_LOCATION,WRF4G_LOCATION,WRF4G_VERSION,WRF4G_LOCATION)
                job.create_template(rea_name + '__' + str(chi.data['id_chunk']),arguments,np=NP,req=REQUIREMENTS,environ=ENVIRONMENT,inputsandbox=sandbox,verbose=self.verbose)
                if chunki == first_id:
                    gw_id=job.submit(priority=priority)
                else:
                    gw_id=job.submit(priority=priority,dep=gw_id,type_dep=type_dep)
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
            subdir='%s/%s/%s/%s/'%(WRF4G_LOCATION,'.submission',exp_name,rea_name)       
            if not os.path.isdir(subdir):
                os.makedirs(subdir)
            os.chdir(subdir)
            rea_input='%s/%s/%s/'%(WRF4G_BASEPATH,exp_name,rea_name)
            if 'resources.wrf4g' in vcplib.VCPURL(rea_input).ls('resources.wrf4g'):
                vcplib.copy_file(join(rea_input,'resources.wrf4g'),'.',verbose=self.verbose)
            else:
                vcplib.copy_file(join(WRF4G_BASEPATH,exp_name,'resources.wrf4g'),'.',verbose=self.verbose)           
            self.prepared=1   
        
    def ps(self, number_of_characters):
        if 'id' in self.data.keys():
            self.data['name']=self.get_name()
        else:
            self.data['id']=self.get_id(['name'])
            if self.data['id']== -1:
                sys.stderr.write('Realization with name %s does not exist\n'%self.data['name'])
                sys.exit(1)            
        drea=dbc.select('Realization','name,restart,sdate,cdate,edate','id=%s'%self.data['id'])
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
                ch=Chunk(data={'id': id_chunk}) 
                lastjob=ch.get_last_job()
                j=Job(data={'id':lastjob})            
                djob=j.get_info()
                if djob['status'] < 10:
                    djob={'gw_job': djob['gw_job'], 'status': djob['status'], 'wn': '-', 'resource': '-', 'exitcode': None,'nchunks':'0/%d'%nchunks}
                djob['nchunks']='%d/%d'%(current_chunk,nchunks)
        dout.update(djob)
        dout['rea_status']=status        
        print format_output(dout, number_of_characters)
                           
    def prepare_storage(self):          
        RESOURCES_WRF4G=os.environ.get('RESOURCES_WRF4G')
        if RESOURCES_WRF4G == None:
            sys.stderr.write('RESOURCES_WRF4G is not defined. Please define it and try again\n')
            sys.exit(1)
        exec open(RESOURCES_WRF4G).read()        
        reas=self.data['name'].split('__')
        rea_dir="%s/%s/%s/" % (WRF4G_BASEPATH,reas[0],self.data['name'])
        vcplib.VCPURL(rea_dir).mkdir(verbose=self.verbose)
        vcplib.copy_file('namelist.input',rea_dir,verbose=self.verbose)
        for dir in ["output","restart","realout","log"]:
          vcplib.VCPURL(join(rea_dir,dir)).mkdir(verbose=self.verbose)
                             
    def is_finished(self,id_rea):
        
        max_id=dbc.select('Chunk','MAX(id)','id_rea=%s'%id_rea,verbose=self.verbose)
        status=dbc.select('Chunk','status','id=%s'%vdblib.parse_one_field(max_id),verbose=self.verbose)
        if status == '4':
            return 1
        else:
            return 0

    def stop_running_chunks(self):
        import gridwaylib
        condition="Chunk.id_rea=%s and Job.id_chunk=Chunk.id AND (Chunk.status=1 OR Chunk.status=2) GROUP BY Chunk.id"%self.data['id']
        output=dbc.select('Chunk,Job','Chunk.id,MAX(Job.id)','%s'%condition,verbose=self.verbose)
        chunk_id=[]
        job_id=[]
        for couple in output:
            chunk_id.append(couple['id'])
            j=Job(data={'id':couple['MAX(Job.id)']})
            job_id.append(j.get_gw_job())
            
        task=gridwaylib.job()
        to=task.kill(job_id)
        condition="id_rea=%s AND (status=1 OR status=2)"%self.data['id']
        data={'status':0}
        output=dbc.update('Chunk',data,'%s'%condition,verbose=self.verbose)

    def change_priority(self, priority):
        import gridwaylib
        condition="Chunk.id_rea=%s and Job.id_chunk=Chunk.id AND (Chunk.status=1 OR Chunk.status=2) GROUP BY Chunk.id"%self.data['id']
        output=dbc.select('Chunk,Job','Chunk.id,MAX(Job.id)','%s'%condition,verbose=self.verbose)
        chunk_id=[]
        job_id=[]
        for couple in output:
            chunk_id.append(couple['id'])
            j=Job(data={'id':couple['MAX(Job.id)']})
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

    def get_id_chunk(self):
        id_chunk=self.get_one_field(['id_chunk'], ['id'])
        return id_chunk 
        
    def prepare_storage(self):
        pass
   
    def get_last_job(self):
        last_job=dbc.select('Job','MAX(Job.id)','Job.id_chunk=%s'%self.data['id'])
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
    
    def get_gw_job(self):
        gw_job=self.get_one_field(['gw_job'], ['id'])
        return gw_job

    def load_wn_conf(self,wn_gwres):
        wn_gwres=int(wn_gwres)
        cond=''
        for field in self.get_distinct_fields():
            cond="%s AND %s='%s'" %(cond,field,self.data[field])
        cond=cond[4:]
        ch=Chunk(data={'id': self.data['id_chunk']})
        nchunk=int(ch.get_id_chunk())
        if nchunk > 1:            
            lch=Chunk(data={'id': str(int(self.data['id_chunk'])-1)})
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
        jobd=dbc.select('Job','MAX(id),gw_restarted',cond,verbose=self.verbose)
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

class Events(Component):
    pass




   

