__version__  = '2.0.0'
__author__   = 'Carlos Blanco'
__revision__ = "$Id$"


import logging
import os
import re
import tarfile
import shutil
import wrf4g.tools.gridwaylib

from sqlalchemy         import func
from sqlalchemy.orm.exc import NoResultFound
from os.path            import exists, expandvars, expanduser, isdir
from wrf4g              import WRF4G_DIR, WRF4G_DEPLOYMENT_DIR, logger
from wrf4g.db           import ExperimentModel, RealizationModel, ChunkModel, JobModel, JobStatusModel
from wrf4g.utils        import datetime2datewrf
from wrf4g.tools.vcplib import VCPURL

# Chunk and job status
CHUNK_STATUS = {
                 'PREPARED'  : 0,
                 'SUBMITTED' : 1,
                 'RUNNING'   : 2,
                 'FAILED'    : 3,
                 'FINISHED'  : 4,
                 'PENDING'   : 5,
               }

JOB_STATUS  =   {
                 'PREPARED'     : 0,
                 'SUBMITTED'    : 1,
                 'RUNNING'      : 2,
                 'PREPARING_WN' : 10,
                 'DOWN_BIN'     : 11,
                 'DOWN_RESTART' : 12,
                 'DOWN_WPS'     : 20,
                 'DOWN_BOUND'   : 21,
                 'UNGRIB'       : 22,
                 'METGRID'      : 23,
                 'REAL'         : 24,
                 'UPLIAD_WPS'   : 25,
                 'ICBCPROCESOR' : 26,
                 'WRF'          : 29,
                 'FINISHED'     : 40,
                 'FAILED'       : 41,
                }
             
class Experiment( ExperimentModel ):
    """ Experiment 
        Methods:
                run 
                delete
                prepare_storage
                prepare
                status
    """
    # Sqlalchemy session
    session = None

    def run(self, rerun=False, dryrun=False):
        """
        Run the realizations of this experiment
        n_chunk is the number of chunks to run. 
        If there are not n_chunk, run every chunk since the last one finished 
        """
        #Check if the experiment have some realization to run
        #list of realizations of the experiment
        q_realizations = self.session.query( Realization ).\
                         filter( Realization.exp_id == self.id )
        if not ( q_realizations.all() ):
            logger.warn( 'There are not realizations to run.' )
        else:
            #if there are realizations to run
            for rea in q_realizations :
                #Run every realization
                rea()
                rea.session = self.session
                rea.run( rerun, dryrun )
        
    def prepare_storage(self):
        """
        Prepare storage
        """
        exp_dir = join( WRF4G_DIR , 'var' , 'submission' , self.name )
        if not isdir( exp_dir ) :
            try:
                logger.debug( "Creating '%s' directory" % exp_dir ) 
                os.makedirs( exp_dir )
            except Exception :
                raise Exception( "Couldn't be created '%s' directory" % exp_dir )
        current_path = os.getcwd()
        wrf4g_package = join ( exp_dir , "WRF4G.tar.gz" )
        if exists( wrf4g_package  ):
            logger.debug( "Removing '%s' package" % wrf4g_package ) 
            os.remove( wrf4g_package )
        tar = tarfile.open( wrf4g_package , "w:gz" )
        os.chdir( WRF4G_DEPLOYMENT_DIR )
        logger.debug( "Creating '%s' package" % wrf4g_package )
        for dir in [ "bin", "lib" ]:
            tar.add( dir )
        os.chdir( current_path )
        
    def prepare(self, reconfigure=False, dryrun=False):
        """ 
        Prepare experiment
        Check if experiment exists in database, If it not exists, insert it and return id
            
        If it exist and  it is the same configuration that the one found in the database, return -1
        If it exists and  parameters of configuration do not match , check if it is a reconfigure run.
        If reconfigure is False=> err retunr -1
        If reconfigure is True, Check if there is an experiment with the same no reconfigurable field. 
        If there is not an experiment with the same no reconfigurable fields => error, return -1
        If there is an experiment with the same reconfigurable fields, update data and return id
        """
        #check if the experiment exists in database
        #It means if there are a realization with the same distinct field: name
        try :
            exp = self.session.query( Experiment ).\
                  filter( Experiment.name == self.name ).one()
            return exp.id
        except Exception:
            #If the experiment does not exist, insert it
            logger.debug( 'Creating experiment' )
            #Insert experiment
            self.session.add(self)
            #Check storage
            if not dryrun :
                try :
                    self.prepare_storage( )
                except :
                    return -1
            return self.id # return id
        else:
            #If the experiment exists
            if not reconfigure :
                logger.error("ERROR: Experiment with the same name already exists.")
                logger.error("If you want to overwrite some paramenters, try the --reconfigure option.")
                return -1 
            else:
                #If reconfigure is True
                #Check if there is an experiment with the same no reconfigurable fields:
                #name,sdate,multiple_dates,multiple_parameters,basepath
                try:
                    exp2=self.session.query.filter( Experiment.name  == self.name,
                                                    Experiment.sdate == self.sdate,
                                                    Experiment.multiple_dates == self.multiple_dates,
                                                    Experiment.multiple_parameters == self.multiple_parameters
                                                  ).one()
                except Exception:
                    #if exp2 does not exist (no experiment with the same no reconfigurable fields)
                    logger.error("ERROR: Experiment with the same name and no reconfigurable fields already exists.")
                    return -1 
                else:
                    #if exp2 exits,it means an experiment with the same no reconfigurable fields
                    self.id = exp2.id
                    logger.debug('Updating experiment')
                    try :
                        self.prepare_storage()
                    except :
                        return -1
                    return self.id
    
    def status(self, long_format=False, rea_pattern=False ):
        """ 
        Show information about realizations of the experiment for example:
        
        Realization Status   Chunks     Comp.Res      Run.Sta     ext      %
        testc       Done     3/3        mycomputer   Finished       0 100.00
        
        * Realization: Realization name. It is taken from the field experiment_name in experiment.wrf4g.
        * Status: It can be take the following values: Prepared, Submitted, Running, Failed and Done).
        * Chunks [Chunk running/Total Chunks]: A realization is split into chunks. Each chunk is sent as a job.
        * Computer resource: Resource (cluster) where the job is running.
        * Run.Sta: Job status in the WN (Downloading data, running ungrib, real, wrf, ...)
        * ext: Exit Code. If exit code is different from 0, there has been an error. 
        * % : percentage of simulation finished.
        """
        #list of realization of the experiment
        q_realizations = self.session.query( Realization ).\
                         filter( Realization.exp_id == self.id )
        if rea_pattern :
            q_realizations  = q_realizations.\
                              filter( Realization.name.like( rea_pattern.replace('*','%') ) )
        #Header of the information
        if not ( q_realizations.all() ):
            logger.info( 'There are not realizations to check.' )
        if long_format :
            logger.info( '%-21s %-8s %-10s %-10s %-13s %2s %4s 20% 20% 20%'% (
                                   'Realization','Status','Chunks','Comp.Res','Run.Sta','ext','%','sdate', 'rdate', 'edate')
                       )
        else :
            logger.info( '%-21s %-8s %-10s %-10s %-13s %2s %4s'% (
                                   'Realization','Status','Chunks','Comp.Res','Run.Sta','ext','%')
                       )
        for rea in q_realizations :
            #Print information of each realization
            rea().status( long_format )
    
    def list(self, long_format=False, rea_pattern=False):
        """
        List the the experiment avaible
        """
        #List of experiments
        q_experiments = self.session.query( Experiment )
        if rea_pattern :
            q_experiments  = q_experiments.\
                             filter( Experiment.name.like( rea_pattern.replace('*','%') ) )
        if not long_format :
            # Header
            logger.info( "Name" )
            for e in q_experiment :
                logger.info( e.name )
        else :
            # Header
            logger.info( "%20s %20s %20s %12s %10s %-30s" % ( 
                         "Name", "Start Date" , "End Date", "Mult Paramts", "Mult Dates", "Mult Labels")  )
            for e in q_experiment :
                logger.info("%20.20s %20.20s %20.20s %12.12s %10.10s %-30.30s" ( 
                         e.name, datetime2datewrf(e.sdate), datetime2datewrf(e.edate), e.mult_parameters, e.mult_dates, e.mult_labels  )

    @staticmethod 
    def start( name, template, dir ):
        validate_name( name )
        if not template in [ 'default', 'single', 'physics' ] :
            raise Exception( "'%s' template does not exist" % template )
        exp_dir = expandvars( expanduser( dir ) )
        if not exists( exp_dir ):
            raise Exception("'%s' does not exist" % exp_dir )
        exp_dir_config = join( exp_dir, name )
        if exists( exp_dir_config ):
            raise Exception("'%s' already exists" % exp_dir_config )
        logger.debug( "Creating '%s' directory" % exp_dir_config )
        shutil.copytree( join( WRF4G_DIR , 'etc' , 'templates' , 'experiments',  template ),
                         exp_dir_config )
        for file in [ 'resources.wrf4g' , 'experiment.wrf4g' ] :
            dest_path = join( exp_dir_config , file )
            with open( dest_path , 'r') as f :
                data = ''.join( f.readlines( ) )
            data_updated = data % {
                                   'WRF4G_EXPERIMENT_HOME' : exp_dir_config ,
                                   'WRF4G_DIR_LOCATION'    : WRF4G_DEPLOYMENT_DIR ,
                                   'exp_name'              : name ,
                               }
            with open( dest_path , 'w') as f :
                f.writelines( data_updated ) 

    def stop(self, dryrun=False):
        """
        Delete jobs which status is running or submitted 
        """
        #list of realization of the experiment
        q_realizations = self.session.query( Realization ).\
                         filter( Realization.exp_id == self.id ) 
        if not ( q_realizations.all() ):
            logger.info( 'There are not realizations to stop.' )
        else :
            logger.info( 'Stopping Experiment %s' % self.name )
            for rea in q_realizations :  
                rea.session = self.session
                rea().stop( dryrun )

    def delete(self, dryrun=False):
        """
        Delete the experiment ,its realizations and chunks 
        """
        # save the name to use it later
        exp_name = self.name 
        if not dryrun :
            local_exp_dir = join( WRF4G_DIR , 'var' , 'submission' , exp_name )
            logger.debug( "Deleting '%s' directory" % local_exp_dir )
            shutil.rmtree( local_exp_dir )
        self.session.delete( self )
        logger.info( "'%s' experiment has been deleted from the database" % exp_name )
    
class Realization( RealizationModel ):
    """A class to mange WRF4G realizations
        Methods:
                has_finished
                run (n_chunk)
                set_restart(restart)
                set_cdate(cdate)
                prepare_storage
                prepare
                ps
                status
    """
    # Sqlalchemy session
    session = None

    def get_status(self):
        """ 
        Check the status of the realization 
        """
        #Search status of the last chunk of the realization
        id_last_chunk = self.session.query( Chunk ).\
                        filter( Chunk.rea_id == self.id ).\
                        order_by( Chunk.id )[-1].id
        return self.session.query( Chunk ).\
                        get( id_last_chunk ).status
    
    def run (self, first_chunk_run=0, last_chunk_run=0, rerun=False, dryrun=False):
        """ 
        Run n_chunk of the realization.
        If n_chunk=0 run every chunk of the realization which haven't finished yet
        else run (n_chunk) chunks since the last one finished
        """
        #Check the status of the realization
        rea_status = self.get_status()
        if rea_status == CHUNK_STATUS[ 'FINISHED' ] and not rerun :
            logger.warn( "Realization '%s' already finished." % self.name )
        elif ( rea_status == CHUNK_STATUS[ 'SUBMITTED' ] or rea_status == CHUNK_STATUS[ 'RUNNING' ] ) not rerun :
            logger.warn( "Realization '%s' has been submitted." % self.name )
        elif first_chunk_run < 0 :
            logger.error( "ERROR: The first chunk to run is '%d'." % first_chunk_run ) 
        elif last_chunk_run  < 0 :
            logger.error( "ERROR: The last chunk to run is '%d'." % last_chunk_run )
        elif last_chunk_run > first_chunk_run :
            logger.error( "ERROR: The last chunk to run is greater than the fist one." )
        else :
            # search first chunk to run 
            if rerun and first_chunk_run == 1 :
                id_first_chunk_run = first_chunk_run
                ch = self.session.filter( Chunk.chunk_id == first_chunk_run,
                                          Chunk.rea_id   == self.id )
                self.restart = ch.sdate
                self.cdate   = ch.sdate
            elif rerun and first_chunk_run != 1 :
                id_first_chunk_run = first_chunk_run
                self.restart = None
                self.cdate   = self.sdate
            else :
                #search first chunk to run
                if not self.restart : # run every chunks of the realization
                    id_first_chunk_run = 1
                else:
                    #search chunk with edate>restart and sdate<restart
                    try:
                        first_chunk_run    = self.session.query( Chunk ).filter( Chunk.rea_id == self.id ).\
                                             filter( and_( Chunk.sdate <= self.restart, Chunk.edate >= self.restart ) ).one()
                        id_first_chunk_run = first_chunk_run.id
                    except :
                        raise Exception( 'There are not chunks to run.' )
            #search last chunk to run
            if not last_chunk_run :
                #run every chunk
                #Search last chunk of the realization
                id_last_chunk_run = self.session.query( Chunk ).\
                                    filter( Chunk.rea_id == self.id ).\
                                    order_by( Chunk.id )[-1].id
            else:
                #search last chunk
                id_last_chunk_run = last_chunk_run
            
            #Search chunks to run
            q_chunks = self.session( Chunk ).\
                       filter( and_( Chunk.id >= id_first_chunk_run, Chunk.id <= id_last_chunk_run ) )
            #run chunks
            for chunk in q_chunks :
                ch         = chunk()
                ch.session = self.session
                ch.run( dryrun )
            
    def prepare_storage(self):
        """
        Prepare the files needed to submit the realization 
        """
        files_to_copy = [
                         join( WRF4G_DIR , 'etc' , 'db.conf' ) ,
                         "resources.wrf4g" ,
                         "experiment.wrf4g" ,
                         "namelist.input" ,
                         ]
        rea_submission_dir = join( WRF4G_DIR , 'var' , 'submission' , self.exp_id.name , self.name )
        if not isdir( rea_submission_dir ) :
            try :
                os.makedirs( rea_submission_dir )
            except Exception :
                raise Exception( "Couldn't be created '%s' directory" % rea_submission_dir )
        if exists( 'wrf4g_files' ):
            current_path = os.getcwd()
            tar = tarfile.open( 'wrf4g_files.tar.gz' , "w:gz" )
            os.chdir( 'wrf4g_files')
            for elem in os.listdir('.') :
                tar.add( elem )
            os.chdir( current_path )
            dst_file = join( rea_submission_dir , 'wrf4g_files.tar.gz' )
            if exists( dst_file ):
                os.remove( dst_file )
            shutil.move( 'wrf4g_files.tar.gz' , dst_file )
        for file in files_to_copy :
            if not exists ( expandvars( file ) ) :
                raise Exception( "'%s' is not available" % file )
            else :
                shutil.copy( expandvars( file ) , rea_submission_dir )
        
    def prepare(self, reconfigure=False, dryrun=False):
        """ 
        Prepare realization
        Check if realization exists in database, If it not exists, insert it and return id
        
        If it exist and  it is the same configuration that the one found in the database, return -1
        If it exists and  parameters of configuration do not match , check if it is a reconfigure run.
        If reconfigure is False=> err return -1
        If reconfigure is True, Check if there is a realization with the same no reconfigurable field. 
        If there is not a realization with the same no reconfigurable fields => error, return -1
        If there is a realization with the same reconfigurable fields, update data and return id
        """
        #check if the realization exists in database
        #It means if there are a realization with the same distinct fields id_exp, sdate,multiparams_labels
        try:
            rea = self.session.query( Realization ).\
                  filter.(Realization.name == self.name).one()
        except Exception:
            #If the realization does not exist, insert it
            logger.debug('Creating Realization')
            #Insert realization
            self.session.add(self)
            #Check storage
            if not dryrun :
                try :
                    self.prepare_storage( )
                except :
                    return -1
            return self.id # return id
        else:
            #If the realization exists
            #Check reconfigure
            if not reconfigure :
                logger.error("Error: Realization with the same name already exists." )
                logger.error("If you want to overwrite some paramenters, try the reconfigure option." ) 
                return -1
            else:
                #Check if there is a realization with the same no reconfigurable fields:
                #id,id_exp,sdate,multiple_parameters
                try:
                    rea2 = self.session.filter( Realization.name  == self.name,
                                                Realization.sdate == self.sdate,
                                                Realization.multiparams_labels == self.multiparams_labels).one()
                except Exception:
                    #if rea2 does not exist (no realization with the same no reconfigurable fields)
                    logger.error("ERROR: Realization with the same name and no reconfigurable fields already exists") 
                    return -1
                else: 
                    #if exp2 exits,it means a realization with the same no reconfigurable fields
                    self.id = rea2.id
                    logger.debug('Updating realization')
                    if not dryrun :
                        try :
                            self.prepare_storage()
                        except :
                            return -1
                    return self.id
    
    def status(self, long_format=False):
        """ 
        Show information about the realization for example:
            Realization Status   Chunks     Comp.Res   WN         Run.Sta       ext   %
            testc       Done     3/3        mycomputer sipc18     Finished       0 100.00
            
        * Realization: Realization name. It is taken from the field experiment_name in experiment.wrf4g.
        * Status: It can be take the following values: Prepared, Submitted, Running, Failed and Done).
        * Chunks [Chunk running/Total Chunks]: A realization is split into chunks. Each chunk is sent as a job.
        * Computer resource: Resource (cluster) where the job is running.
        * Run.Sta: Job status in the WN (Downloading data, running ungrib, real, wrf, ...)
        * ext: Exit Code. If exit code is different from 0, there has been an error.
        * % : percentage of simulation finished.
        """
        #List of chunks of the realization
        q_chunks = self.session.query( Chunk ).\
                   filter( Chunk.rea_id == self.id )
        #Number of chunks
        number_chunks = q_chunks.count()
        #Look for: job_status,rea_status,wn,resource,exitcode,nchunks.
        #These parameters depend on the status of the first chunk:
        #Status of the first chunk 
        status_first_ch = q_chunks.\
                          filter( Chunk.chunk_id == 1).one().status
       
        if status_first_ch == CHUNK_STATUS[ 'PREPARED' ] or status_first_ch == CHUNK_STATUS[ 'SUBMITTED' ] :
            #Parameters are:
            job_status = 0 if status_first_ch == CHUNK_STATUS[ 'PREPARED' ] else 1
            rea_status = job_status
            resource   = '-'
            exitcode   = None
            nchunks    ='0/%d' % number_chunks
        else:
            #Status of the first chunk is different of 0 or 1
            #Select status of the realization (rea_status) and the current chunk(ch_current)
            #Check if there are any chunk with failed status 
            #Look for id_chunk of the first chunk failed
            try :
                id_chunk_min_failed = q_chunks.\
                                      filter( Chunk.status == CHUNK_STATUS[ 'FAILED' ] ).\
                                      order_by( Chunk.chunk_id )[ 0 ].id
            except :
                try : 
                    #There are not any chunk failed
                    #Check if there are any chunk.status=4 and select the last one
                    id_chunk_max_finished = q_chunks.\
                                            filter( Chunk.status == CHUNK_STATUS[ 'FINISHED' ]).\
                                            order_by( Chunk.chunk_id )[ -1 ].id 
                except :
                    #If there are not any chunk with status = finished => is the first chunk of the realization
                    ch_current = q_chunks.\
                                 filter( Chunk.chunk_id == 1 )
                    rea_status = 2
                else:
                    #There are any chunk whose status is finished
                    #check if the realization is finished
                    if id_chunk_max_finished == number_chunks:
                        rea_status = 4 
                        ch_current = q_chunks.\
                                     filter( Chunk.chunk_id == id_chunk_max_finished)
                    else:
                        #Realization has not finished yet
                        rea_status = 2
                        ch_current = q_chunks.\
                                     filter( Chunk.chunk_id == id_chunk_max_finished + 1 )
            else:
                #First chunk with status = failed
                ch_current = q_chunks.\
                             filter( Chunk.chunk_id == id_chunk_min_failed )
                rea_status = 3
                
            #Select parameters:job_status, rea_status,wn,resource,exitcode,nchunks
            #Last job of current chunk
            id_last_job = self.session.query( Job ).\
                          filter( Job.chunk_id == ch_current.id).\
                          order_by( Job.id )[ -1 ].id
            last_job    = self.session.query( Job ).\
                          filter( Job.id == id_last_job )
            #Parameters if last_job.status is prepared(0) or submitted(1)
            job_st_0 = self.session.query( JobStatusModel ).get( 0 )
            job_st_1 = self.session.query( JobStatusModel ).get( 1 )
            if last_job.status == job_st_0 or last_job.status == job_st_1 :
                #Parameters
                job_status = last_job.status.id
                rea_status = job_status
                resource   = '-'
                exitcode   = None
                nchunks    = '0/%d' % number_chunks
            else:
                #Last job does not have status prepared or submitted
                job_status = last_job.status.id
                resource   = last_job.resource
                exitcode   = last_job.exitcode
                nchunks    = '%d/%d' % (last_job.id_chunk.id_chunk, number_chunks)
                
        #FORMAT OUTPUT
        dreastatus={0:'Prepared',1:'Submit',2:'Running',3: 'Failed',4:'Done',5:'Pending'}
        #Format exitcode
        if exitcode :
            exitcode = str(exitcode)
        else:
            exitcode = '-'
        #Format chunks run / chunks total
        runt   =  int( self.cdate.strftime("%s") ) - int( self.sdate.strftime("%s") ) 
        totalt =  int( self.edate.strftime("%s") ) - int( self.sdate.strftime("%s") )
        #Percentage
        per = runt * 100.0/ totalt
        #Rea_status
        if job_status < 10 :
            rea_status = 5
        #Job status
        job_status_description = self.session.query( JobStatusModel).get( job_status ).description
        #Print output
        if long_format :
            logger.info( "%-21s %-8s %-10s %-10s %-13s %2s %4.2f %20s %20s %20s" % (
                    self.name[0:20],dreastatus[rea_status],nchunks,resource[0:10],job_status_description,exitcode,per,
                    self.sdate, self.restart, self.edate )
                    )
        else :
            logger.info( "%-21s %-8s %-10s %-10s %-13s %2s %4.2f" % (
                    self.name[0:20],dreastatus[rea_status],nchunks,resource[0:10],job_status_description,exitcode,per)
  
    def stop(self, dryrun=False):
        """
        Delete chunks which status is running or submitted 
        """
        q_chunks = self.session( Chunk ).\
                   filter( Chunk.rea_id == self.id ).\
                   filter( or_( Chunk.status == CHUNK_STATUS['SUBMITTED'], Chunk.status == CHUNK_STATUS['RUNNING'] ) )        
        if not ( q_chunks.all() ):
            logger.info( 'There are not chunks to stop.' )
        else :
            logger.info('\tStopping Realization %s' % self.name )
            for chunk in q_chunks :
                ch         = chunk()
                ch.session = self.session
                ch.stop( dryrun )
    
class Chunk( ChunkModel ):
    """ 
    Chunk
        Methods:
                get_wps
                set_wps
                set_status
                run
                prepare
    """
    # Sqlalchemy session
    session = None
 
    #METHODS
    def run (self, dryrun=False):
        """ 
        Run a chunk is run a drm4g job
        """
        #check if is the first chunk of the realization
        if self.chunk_id == 1 :
            logger.info('Submitting Realization: "%s" with restart %s' % (self.rea_id.name, self.rea_id.restart ) )
        #print data of chunks
        logger.info('\t\tSubmitting Chunk %d:\t%s\t%s' % ( self.chunk_id, 
                                                          datetime2datewrf(self.sdate), 
                                                          datetime2datewrf(self.edate) ) 
                   )
        if not dryrun :
            #Send a gridway's job and save data in table Job
            job               = Job()  #create an object "job"
            job.session       = self.session
            job.chunk_id      = self
            job.run(first_chunk_rea=self.chunk_id) #run job
            self.session.add(job)
        
    def prepare(self,reconfigure=False,dryrun=False):
        """ 
        Prepare chunk
        Check if chunk exists in database, If it not exists, insert it and return id
        
        If it exist and  it is the same configuration that the one found in the database, return -1
        If it exists and  parameters of configuration do not match , check if it is a reconfigure run.
        If reconfigure is False=> err return -1
        If reconfigure is True, Check if there is a chunk with the same no reconfigurable field. 
        If there is not a chunk with the same no reconfigurable fields => error, return -1
        If there is a chunk with the same reconfigurable fields, update data and return id
        """
        #check if the chunk exists in database
        #It means if there are a chunk with the same distinct fields
        # id_rea,id_chunk
        try:
            ch = self.session.query( Chunk.id       == self.id,
                                     Chunk.rea_id   == self.rea_id,
                                     Chunk.chunk_id == self.chunk_id
                                    ).one()
        except Exception:
            #If the chunk does not exist, insert it
            logger.debug('Creating chunk.')
            #Insert chunk
            self.session.add(self)
            return self.id # return id
        else:
            #If the chunk exists
            #Check reconfigure: if reconfigure is False =>err,exitif reconfigure == False:if reconfigure == False:
            if not reconfigure :
                logger.error( "ERROR Chunk with the same name and different parameters already exists.")
                logger.error( "If you want to overwrite some paramenters, try the reconfigure option.") 
                return -1
            else:
                #Check if there is a chunk with the same no reconfigurable fields:
                #id,id_rea,id_chunk,sdate
                try:
                    ch = self.session.( Chunk.id       == self.id,
                                        Chunk.rea_id   == self.rea_id,
                                        Chunk.chunk_id == self.chunk_id,
                                        Chunk.sdate    == self.sdate).one()
                except Exception:
                    #if ch2 does not exist (no chunk with the same no reconfigurable fields)
                    logger.error("ERROR: Chunk with the same name and no reconfigurable fields already exists")
                    return -1
                else:
                    #if ch2 exits,it means a chunk with the same no reconfigurable fields
                    self.id = ch.id
                    logger.debug('Updating chunk.')
                    return self.id

    def stop(self, dryrun=False):
        """
        Delete jobs
        """
        logger.info('\t\tDeleting Chunk %d:\t%s\t%s' % ( self.chunk_id,
                                                         datetime2datewrf(self.sdate),
                                                         datetime2datewrf(self.edate) 
                                                       )
        q_jobs = self.session.query( Job ).\
                 filter( Job.chunk_id.id == self.id ).\
                 filter( and_( Job.jobst_status <= JobStatusModel.get(1) , Job.jobst_status >= JobStatusModel.get(13) ) )
        if not ( q_jobs.all() ):
            logger.info( 'There are not jobs to stop.' )
        else :
            for job in q_jobs :
                job.session = self.session
                job().stop( dryrun )

        
class Job( JobModel ):
    """ Job
        Methods:
                run (first_chunk_rea)
                set_status(st)
                set_exitcode(exitcode)
                load_wn_conf(wn_gwres)
    """
    # Sqlalchemy session
    session = None
    
    def set_status(self, status):
        """ 
        Save the status of the job and if it is a jobstatus of the dictionary CHUNK_STATUS,
        change the status of the Chunk.
        Add an event. 
        """
        #Save job's status
        self.jobst_status  = status
        #if is an status of the dictionary CHUNK_STATUS
        if status in CHUNK_STATUS.values() : 
            self.id_chunk.status = status
        #Add event
        events              = Events()
        events.job_id       = self
        events.jobst_status = status
        events.timestamp    = datetime.utcnow()
        self.session.add( events )
        
    def run(self,first_chunk_rea):
        """ 
        Send a drm4g's job and save data in table Job
        id_chunk is an instance of Chunk
        """
        if not self.gw_restarted :
            self.gw_restarted = 0
        # create gridway's job
        job      = wrf4g.tools.gridwaylib.Job()
        rea_id   = self.chunk_id.rea_id.id
        rea_name = self.chunk_id.rea_id.name
        exp_name = self.chunk_id.rea_id.exp_id.name
        # create template
        rea_dir = join( WRF4G_DIR , 'var' , 'submission' , exp_name, rea_name )
        wrf4g_package = join( WRF4G_DIR , 'var' , 'submission' , exp_name, 'WRF4G.tar.gz' )
        if not exists(  wrf4g_package ) :
            raise Exception( "'%s' file does not exist" % wrf4g_package )
        sandbox  = "file://%(rea_dir)s/%(wrf4g_package)s,"                 
        sandbox += "file://%(rea_dir)s/db.conf,"          
        sandbox += "file://%(re_dir)s/resources.wrf4g,"  
        sandbox += "file://%(rea_dir)s/experiment.wrf4g," 
        sandbox += "file://%(rea_dir)s/namelist.input"  
        sandbox % { "rea_dir" : rea_dir , "wrf4g_package" : wrf4g_package } 
        input_files = join( rea_dir , 'wrf4g_files.tar.gz' )
        if exists( input_files ) :
            sandbox += ",file://%s" % ( input_files )
        var_resources = VarEnv( join( rea_dir , 'resources.wrf4g' )  )
        arguments='%s %s %d %d %s %s'%(rea_name,
                                       rea_id,
                                       self.chunk_id.chunk_id,
                                       self.chunk_id.id,
                                       datetime2datewrf(self.chunk_id.sdate),
                                       datetime2datewrf(self.chunk_id.edate)
                                      )
        job.create_template( join( rea_dir,  rea_name + '__' + str( self.chunk_id.chunk_id ) ),
                            arguments,
                            np           = int( var_resources.get_variable( 'NP', default = '1') ),
                            req          = var_resources.get_variable( 'REQUIREMENTS' ) ,
                            environ      = var_resources.get_variable( 'ENVIRONMENT' ) ,
                            inputsandbox = sandbox
                            )
        #submit job
        # if the first chunk of the realization
        if first_chunk_rea == 1 :
            self.gw_job = job.submit()
        else:
            #if the chunk is not the first of the realization, gwsubmit has an argument, gw_job of the job before
            id_chunk_before = self.chunk_id.id - 1
            id_job_before   = self.session.query( Job ).\
                              filter( self.chunk == id_chunk_before).\
                              order_by( Chunk.id )[-1].id
            gw_job_before   = self.session.query( Job ).get( id_job_before ).gw_job
            self.gw_job     = job.submit( dep = gw_job_before )
        #Insert values (gw_job, id_chunk, status) into table Job
        self.set_status( self.session.query( JobStatusModel ).\
             get( JOB_STATUS[ 'SUBMITTED' ] ) ) 
   
    def stop(self, dryrun=False):
        """
        Delete a job
        """
        logger.info('\t\t\tDeleting Job %d' % self.gw_job ) 
        if not dryrun :
            wrf4g.tools.gridwaylib.Job().kill( self.gw_job )
            self.set_status( self.session.query( JobStatusModel ).\
                 get( JOB_STATUS[ 'PREPARED' ] ) ) 
         
    def load_wn_conf(self,wn_gwres):
        """
        Load configuration
        """
        wn_gwres=int(wn_gwres)
        #Last job with the same distinct fields  (gw_job and id_chunk)
        try :
            last_job = self.session.query( Job.gw_job   == self.gw_job, 
                                           Job.id_chunk == self.chunk_id.id).\
                                           group_by( Job.id ).all()[-1]
        except :
            logger.error('ERROR: Could not find a the last job')
            return -1
        if last_job.gw_restarted < wn_gwres:
            self.gw_restarted = wn_gwres
        elif last_job.gw_restarted == wn_gwres:
            self.id = last_job.id
        else:
            #if last_job.gw_restarted>wn_gwres
            logger.error('ERROR: This job should not be running this Chunk')
            return -1
        self.set_status( self.session.query( JobStatusModel ).\
            get( JOB_STATUS[ 'PREPARING_WN' ] ) ) 
        return self.id
    
    @staticmethod
    def create_rea_storage(self):
        """
        Create a remote tree directory for the realization
        and copy configure files.
        
        realization/
           * experiment.wrf4g
           * resources.wrf4g
           * db.conf
           * namelist.input
           * -- output/                    
           * -- restart/
           * -- realout/
           * -- log/         
        """
        rea_dir = join( 
                       os.getenv("WRF4G_BASEPATH" ),
                       os.getenv("WRF4G_EXPERIMENT" ),
                       os.getenv("WRF4G_REALIZATION" )
                       )
                                            
        dir_to_create = [ 
                       dirname( dirname( re_dir ) ) ,
                       dirname( rea_dir ) ,
                       rea_dir,
                       "%s/%s/" % ( rea_dir, "output" ) ,
                       "%s/%s/" % ( rea_dir, "restart" ) ,
                       "%s/%s/" % ( rea_dir, "reaload" ) ,
                       "%s/%s/" % ( rea_dir, "log" ) 
                       ]
        # create directories
        for dir in  dir_to_create :
            vcp_dir = VCPURL( dir )
            if not vcp_dir.exists( ) :
                vcp_dir.mkdir( )

        # copy experiment files
        for conf_file in [ "db.conf", "experiment.wrf4g", "resources.wrf4g", "namelist.input" ] :
            vcplib.copy_file( conf_file, join( rea_dir , conf_file  ) )

    @staticmethod
    def export_varibles(self):
        """
        Exports the environment variables of experiment.wrfg and resources.wrf4g files.
        """
        list_files = [ 'experiment.wrfg', 'resources.wrf4g' ]  
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
