__version__  = '2.0.0'
__author__   = 'Carlos Blanco'
__revision__ = "$Id$"


import logging
import os
import re
import tarfile
import shutil
import datetime
import wrf4g.tools.gridwaylib

from sqlalchemy         import func
from sqlalchemy.orm.exc import NoResultFound
from os.path            import exists, expandvars, expanduser, isdir
from datetime           import datetime 
from wrf4g              import WRF4G_DIR, WRF4G_DEPLOYMENT_DIR, logger
from wrf4g.db           import ExperimentModel, RealizationModel, ChunkModel, JobModel, JobStatusModel
from wrf4g.utils        import datetime2datewrf, exec_cmd

from wrf4g.tools.vcplib import VCPURL

# Realization, Chunk and Job status
REA_STATUS = {
                 'PREPARED'     : 0,
                 'SUBMITTED'    : 1,
                 'RUNNING'      : 2,
                 'PENDING'      : 3,
                 'FAILED'       : 4,
                 'FINISHED'     : 5,
               }

CHUNK_STATUS = {
                 'PREPARED'     : 0,
                 'SUBMITTED'    : 1,
                 'RUNNING'      : 2,
                 'PENDING'      : 3,
                 'FAILED'       : 4,
                 'FINISHED'     : 5,
               }

JOB_STATUS  =   {
                 'PREPARED'     : 0,
                 'SUBMITTED'    : 1,
                 'RUNNING'      : 2,
                 'PENDING'      : 3,
                 'FAILED'       : 4,
                 'FINISHED'     : 5,
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
                }
             
class Experiment( ExperimentModel ):
    """ 
    Manage WRF4G experiments
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
                rea.session = self.session
                rea().run( rerun, dryrun )

    def _create_bundle(self):
        """
        Create a bundle with the necessary software to run WRF on WNs
        """
        exp_dir = join( WRF4G_DIR , 'var' , 'submission' , self.name )
        if not isdir( exp_dir ) :
            try:
                logger.debug( "Creating '%s' directory" % exp_dir ) 
                os.makedirs( exp_dir )
            except Exception :
                raise Exception( "Couldn't be created '%s' directory" % exp_dir )
        wrf4g_package = join ( exp_dir , "WRF4G.tar.gz" )
        if exists( wrf4g_package  ):
            logger.debug( "Removing '%s' package" % wrf4g_package ) 
            os.remove( wrf4g_package )
        current_path = os.getcwd()
        tar = tarfile.open( wrf4g_package , "w:gz" )
        os.chdir( WRF4G_DEPLOYMENT_DIR )
        logger.debug( "Creating '%s' package" % wrf4g_package )
        for dir in [ "bin", "lib" ]:
            tar.add( dir )
        os.chdir( current_path )
        
    def insert_db(self, reconfigure=False, dryrun=False):
        """ 
        Check if experiment exists in database, If it not exists, insert it.
            
        If it exist and  it is the same configuration that the one found in the database.
        If it exists and  parameters of configuration do not match , check if it is a reconfigure run.
        If reconfigure is False=> err
        If reconfigure is True, Check if there is an experiment with the same no reconfigurable field. 
        If there is not an experiment with the same no reconfigurable fields => error.
        If there is an experiment with the same reconfigurable fields, update data.
        """
        #check if the experiment exists in database
        #It means if there are a realization with the same distinct field: name
        try :
            exp = self.session.query( Experiment ).\
                  filter( Experiment.name == self.name ).one()
        except Exception:
            #If the experiment does not exist, insert it
            logger.debug( 'Creating experiment on the database...' )
            #Insert experiment
            self.session.add(self)
            #Check storage
            if not dryrun :
                self.create_bundle()
        else:
            #If the experiment exists
            if not reconfigure :
                raise Exception("ERROR: Experiment with the same name already exists."
                                "If you want to overwrite some paramenters, try the --reconfigure option.")
            else:
                #If reconfigure is True
                #Check if there is an experiment with the same no reconfigurable fields:
                #name,sdate,multiple_dates,multiple_parameters,basepath
                try:
                    exp2=self.session.query.filter( Experiment.name  == self.name,
                                                    Experiment.sdate == self.sdate,
                                                    Experiment.mult_dates == self.mult_dates,
                                                    Experiment.mult_parameters == self.mult_parameters
                                                  ).one()
                except Exception:
                    #if exp2 does not exist (no experiment with the same no reconfigurable fields)
                    raise Exception("ERROR: Experiment with the same name and no reconfigurable fields already exists.")
                else:
                    #if exp2 exits,it means an experiment with the same no reconfigurable fields
                    self.id = exp2.id
                    logger.debug('Updating experiment on the database...')
                    self._create_bundle()

    def create(self, reconfigure=False, dryrun=False,  directory='./' )
        """
        Create and prepare all realizations and chunks needed to submit a WRF4G experiment 
        """
        # Sanity check 
        exp_dir = expandvars( expanduser( directory ) )
        if not exists( exp_dir ):
            raise Exception("'%s' does not exist" % exp_dir )
        exp_file = join( exp_dir, 'experiment.wrf4g' )
        if not exists( exp_file ):
            raise Exception("'%s' does not exist" % exp_fileg ) 
        exp_features = VarEnv( exp_file )
        # Defining experiment variables
        self.name     = exp_features.get_variable( 'experiment_name' )
        self.basepath = directory
        self.sdate    = datetime.strptime(exp_features.get_variable( 'start_date' ), "%Y-%m-%d_%H:%M:%S")
        self.edate    = datetime.strptime(exp_features.get_variable( 'end_date' ), "%Y-%m-%d_%H:%M:%S"),
        self.csize    = exp_features.get_variable( 'chunk_size_h' , ( sdate - edate ).total_seconds()/3600 ) 
        self.mult_parameters = 1 if exp_features.has_section( 'multiple_parameters' ) else 0
        self.mult_dates      = 1 if exp_features.has_section( 'multiple_dates' ) else 0
        if self.mult_parameters :
            self.mult_labels = exp_features.get_variable( 'combinations', 'parameters' ).\
                          replace(' ', '').replace('\n', '/')
        else :
            self.mult_labels = ''
        if self.mult_dates :
            self.smul_length_h = exp_features.get_variable( 'simulation_length_h', 'multiple_dates' ) 
            if not self.smul_length_h :
                raise Exception("'simulation_length_h' variable is madatory for 'multiple_dates' section" % var)
            self.smul_interval_h = exp_features.get_variable( 'simulation_interval_h', 'multiple_dates' )
            if not self.smul_interval_h :
                raise Exception( "'simulation_interval_h' variable is madatory for 'multiple_dates' section" )
        # Insert the experiment on the database
        self.insert_db( reconfigure = reconfigure, dryrun = dryrun)
        if not dryrun :
            # Modify the namelist with the parameters available in experiment.wrf4g
            logger.info( "Preparing namelist..." )
            if exp_features.has_section( 'NI' ) :
                restart_interval = exp_features.get_variable( 'restart_interval' , 'NI', chunk_size_h * 60 )
            else :
                restart_interval = chunk_size_h * 60
            namelist_template = join( WRF4G_DIR , 'etc', 'templates', 'namelist', 'namelist.input-%s' % 
                                     exp_features.get_variable( 'namelist_version' ) )
            namelist_input    = join( directory, 'namelist.input')
            try :
                logger.debug("Copying '%s' to '%s'" % (namelist_template, namelist_input) )
                shutil.copyfile(namelist_template, namelist_input )
            except :
                raise Exception( "There is not a namelist template for WRF %s (File namelist.input does not exist)" % namelist_template )
            logger.debug( "Updating parameter 'max_dom' in the namelist" )
            exec_cmd( "fortnml -wof %s -s max_dom %s" % ( namelist_input, exp_features.get_variable( 'max_dom' ) ) )
            if exp_features.has_section( 'NI' ) :  
                for key, val in exp_features.items( 'NI' ) :
                    logger.debug( "Updating parameter '%s' in the namelist" % key )
                    exec_cmd( "fortnml -wof %s -s %s -- %s" % (namelist_input, key, val) )
            if exp_features.has_section( 'NIM' ) :
                for key, val in exp_features.items( 'NI' ) :
                    logger.debug( "Updating parameter '%s' in the namelist" % key )
                    exec_cmd( "fortnml -wof %s -s %s -- %s" % (namelist_input, key, ' '.join( val.split(',')) ) )
            if exp_features.has_section( 'NIN' ) :
                for key, val in exp_features.items( 'NIN' ) :
                    logger.debug( "Updating parameter '%s' in the namelist" % key )
                    exec_cmd( "fortnml -wof %s -m %s -- %s" % (namelist_input, key, val) )
        if exp_features.has_section( 'multiple_parameters' ) :
           # If there is a multiple_parameter
           for rea_label in mult_labels.split('/') :
               rea_tag, combinations = rea_label.split('|')
               logger.info( "--->Realization: multiparams=%s %s %s" % (rea_label, sdate, edate) )
               if not dryrun :
                   l_variables    = exp_features.get_variable('variables', 'multiple_parameters' ).replare(' ', '').split(',')
                   l_combinations = combinations.split(',')
                   l_nitems       = exp_features.get_variable('nitems', 'multiple_parameters' ).replare(' ', '').split(',')
                   for var, comb, nitem in zip( l_variables, l_combinations, l_nitems ) :
                       # Update the namelist per each combination
                       logger.debug( "Updating parameter '%s' in the namelist" % var )
                       if ':' in comb :
                           exec_cmd( "fortnml -wof %s -s %s -- %s" % (namelist_input, var, ' '.join( comb.split(':') ) ) )
                       else :
                           exec_cmd( "fortnml -wof %s -s %s -n %s -- %s" % (namelist_input, var, nitem, comb ) )
               self.cycle_time( "%s__%s " % ( name, rea_tag ), reconfigure, dryrun)
        else :
           logger.info( "---> Single params run" )
           self.cycle_time( name, reconfigure, dryrun )
    
    def cycle_time(self, rea_name, reconfigure=False, dryrun=False ) :
        """
        Check if the experiment has multiple_dates 
        """
        if self.mult_dates :
            self.cycle_hindcasts( rea_name, reconfigure, dryrun )
        else :
            logger.info( "---> Continuous run" )
            # Create realization
            rea = Realization( name       = rea_name,
                               exp_id     = self.id,
                               sdate      = self.sdate,
                               edate      = self.edate 
                               cdate      = self.sdate,
                               status     = REA_STATUS[ 'PREPARED' ]
                               mult_label = self.mult_label )
            rea.session = self.session
            # Insert data on the database
            rea.insert( reconfigure )
            # Check storage
            if not dryrun :
                rea.prepare_sub_files()
            rea.cycle_chunks( reconfigure )
 
    def cycle_hindcasts(self, rea_name, reconfigure=False, dryrun=False ) :
        """
        Create chunks the needed for a realization 
        """
        logger.info( "\n---> cycle_hindcasts: %s %s %s %s %s" % ( 
                      rea_name, self.id, self.sdate , self.edate, self.mult_label ) )
        rea_sdate = self.sdate
        while chunk_sdate <= ( self.edate - timedelta( hours = self.smul_interval_h ) ) :
            rea_edate = rea_sdate + timedelta( hours = self.smul_interval_h )
            # Create realization
            rea = Realization( name       = "%s__%s_%s" % ( rea_name, rea_sdate, rea_edate),
                               exp_id     = self.id,
                               sdate      = rea_sdate,
                               edate      = rea_edate,
                               cdate      = rea_sdate,
                               status     = REA_STATUS[ 'PREPARED' ]
                               mult_label = self.mult_label )
            rea.session = self.session
            # Insert data on the database
            rea.insert( reconfigure )
            # Check storage
            if not dryrun :
                rea.prepare_sub_files()
            rea.cycle_chunks( reconfigure )
            rea_sdate = rea_edate
    
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
            rea.session = self.session
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
                         e.name, datetime2datewrf(e.sdate), datetime2datewrf(e.edate), e.mult_parameters, e.mult_dates, e.mult_labels )

    @staticmethod 
    def create_files(name, template, directory):
        """
        Create the files needed to establish a WRF4G experiment
        """
        validate_name( name )
        if not template in [ 'default', 'single', 'physics' ] :
            raise Exception( "'%s' template does not exist" % template )
        exp_dir = expandvars( expanduser( directory ) )
        if not exists( exp_dir ):
            raise Exception("'%s' does not exist" % exp_dir )
        exp_dir_config = join( exp_dir, name )
        if exists( exp_dir_config ):
            raise Exception("'%s' already exists" % exp_dir_config )
        logger.debug( "Creating '%s' directory" % exp_dir_config )
        shutil.copytree( join( WRF4G_DIR , 'etc' , 'templates' , 'experiments',  template ),
                         exp_dir_config )
        dest_path = join( exp_dir_config , 'experiment.wrf4g' )
        with open( dest_path , 'r') as f :
            data = ''.join( f.readlines( ) )
        data_updated = data % {
                               'WRF4G_EXPERIMENT_HOME' : exp_dir_config ,
                               'WRF4G_DIR_LOCATION'    : WRF4G_DEPLOYMENT_DIR ,
                               'exp_name'              : name ,
                               }
        with open(dest_path, 'w') as f :
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
    """
    A class to mange WRF4G realizations
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
    
    def run (self, first_chunk_run=1, last_chunk_run=1, rerun=False, dryrun=False):
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
            
    def prepare_sub_files(self):
        """
        Prepare the files needed to submit the realization 
        """
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
        for file in [ join( WRF4G_DIR, 'etc', 'db.conf' ), "experiment.wrf4g", "namelist.input" ] :
            if not exists ( expandvars( file ) ) :
                raise Exception( "'%s' is not available" % file )
            else :
                shutil.copy( expandvars( file ) , rea_submission_dir )
        
    def insert(self, reconfigure=False ):
        """ 
        Check if realization exists in database, If it not exists, insert it 
        
        If it exist and  it is the same configuration that the one found in the database
        If it exists and  parameters of configuration do not match , check if it is a reconfigure run.
        If reconfigure is False=> err 
        If reconfigure is True, Check if there is a realization with the same no reconfigurable field. 
        If there is not a realization with the same no reconfigurable fields => error
        If there is a realization with the same reconfigurable fields, update data
        """
        #check if the realization exists in database
        #It means if there are a realization with the same distinct fields id_exp, sdate,multiparams_labels
        try:
            rea = self.session.query( Realization ).\
                  filter.(Realization.name == self.name).one()
        except Exception:
            #If the realization does not exist, insert it
            logger.debug('Creating realization on the database...')
            #Insert realization
            self.session.add(self)
        else:
            #If the realization exists
            #Check reconfigure
            if not reconfigure :
                raise Exception("Error: Realization with the same name already exists."
                      "If you want to overwrite some paramenters, try the reconfigure option." )
            else:
                #Check if there is a realization with the same no reconfigurable fields:
                #id,id_exp,sdate,multiple_parameters
                try:
                    rea2 = self.session.filter( Realization.name  == self.name,
                                                Realization.sdate == self.sdate,
                                                Realization.mult_labels == self.mult_labels).one()
                except Exception:
                    #if rea2 does not exist (no realization with the same no reconfigurable fields)
                    raise Exception("ERROR: Realization with the same name and no reconfigurable fields already exists") 
                else: 
                    #if exp2 exits,it means a realization with the same no reconfigurable fields
                    logger.debug('Updating realization on the database...')
                    self.id = rea2.id
   
    def cycle_chunks(self, reconfigure= False ):
        """
        Create chunks the needed for a realization 
        """
        logger.info( "\t---> cycle_chunks: %s %s %s" % ( self.name, self.sdate , self.edate ) )
        chunk_id = 1
        chunk_sdate = self.sdate
        while chunk_sdate <= ( self.edate - timedelta( hours = self.csize ) ) :
            chunk_edate = chunk_sdate + timedelta( hours = self.csize )
            logger.info( "\t\t---> chunk %d: %s %s %s" %( chunk_id, self.name, chunk_sdate, chunk_edate ) )
            # Create Chunk
            ch = Chunk( rea_id   = self.id, 
                        chunk_id = chunk_id, 
                        sdate    = chunk_sdate, 
                        edate    = chunk_edate,
                        wps      = 0 ,
                        status   = CHUNK_STATUS[ 'PREPARED' ])
            ch.session = self.session
            # Insert on the database
            ch.insert( reconfigure )
            chunk_sdate = chunk_edate 
            chunk_id    = chunk_id + 1
 
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
            job_status = REA_STATUS[ 'PREPARED' ] if status_first_ch == CHUNK_STATUS[ 'PREPARED' ] else REA_STATUS[ 'SUBMITTED' ]
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
                                      order_by( Chunk.chunk_id ).one().id
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
                    rea_status = REA_STATUS[ 'RUNNING' ]

                else:
                    #There are any chunk whose status is finished
                    #check if the realization is finished
                    if id_chunk_max_finished == number_chunks:
                        rea_status = REA_STATUS[ 'FINISHED' ] 
                        ch_current = q_chunks.\
                                     filter( Chunk.chunk_id == id_chunk_max_finished)
                    else:
                        #Realization has not finished yet
                        rea_status = REA_STATUS[ 'RUNNING' ]
                        ch_current = q_chunks.\
                                     filter( Chunk.chunk_id == id_chunk_max_finished + 1 )
            else:
                #First chunk with status = failed
                ch_current = q_chunks.\
                             filter( Chunk.chunk_id == id_chunk_min_failed )
                rea_status =  REA_STATUS[ 'FALIED' ]
                
            #Select parameters:job_status, rea_status,wn,resource,exitcode,nchunks
            #Last job of current chunk
            id_last_job = self.session.query( Job ).\
                          filter( Job.chunk_id == ch_current.id).\
                          order_by( Job.id )[ -1 ].id
            last_job    = self.session.query( Job ).\
                          filter( Job.id == id_last_job )
            if last_job.status == JOB_STATUS[ 'PREPARED' ] or last_job.status == JOB_STATUS[ 'SUBMITTED'] :
                #Parameters
                job_status = last_job.status.id
                rea_status = job_status
                resource   = '-'
                exitcode   = None
                nchunks    = '0/%d' % number_chunks
            else:
                #Last job does not have status prepared or submitted
                job_status = last_job.status.id
                rea_status = job_status
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
    A class to manage WRF4G chunks
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
        
    def insert(self, reconfigure=False ):
        """ 
        Check if chunk exists in database, If it not exists, insert it.
        
        If it exist and  it is the same configuration that the one found in the database.
        If it exists and  parameters of configuration do not match , check if it is a reconfigure run.
        If reconfigure is False=> err.
        If reconfigure is True, Check if there is a chunk with the same no reconfigurable field. 
        If there is not a chunk with the same no reconfigurable fields => error,.
        If there is a chunk with the same reconfigurable fields, update data.
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
            logger.debug('Creating chunk on the database')
            #Insert chunk
            self.session.add(self)
        else:
            #If the chunk exists
            #Check reconfigure: if reconfigure is False =>err,exitif reconfigure == False:if reconfigure == False:
            if not reconfigure :
                raise Exception( "ERROR Chunk with the same name and different parameters already exists."
                                 "If you want to overwrite some paramenters, try the reconfigure option.") 
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
                    raise Exception("ERROR: Chunk with the same name and no reconfigurable fields already exists")
                else:
                    #if ch2 exits,it means a chunk with the same no reconfigurable fields
                    logger.debug('Updating chunk on the database')
                    self.id = ch.id

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
        rea_dir  = join( WRF4G_DIR , 'var' , 'submission' , exp_name, rea_name )
        if not exists(  wrf4g_package ) : raise Exception( "'%s' file does not exist" % wrf4g_package )
        # files to add for the inputsandbox 
        inputsandbox  = "file://%(rea_dir)s/%(wrf4g_package)s,"                 
        inputsandbox += "file://%(rea_dir)s/db.conf,"          
        inputsandbox += "file://%(rea_dir)s/resources.wrf4g,"  
        inputsandbox += "file://%(rea_dir)s/experiment.wrf4g," 
        inputsandbox += "file://%(rea_dir)s/namelist.input"  
        inputsandbox % { "rea_dir"       : rea_dir , 
                         "wrf4g_package" : join( WRF4G_DIR , 'var' , 'submission' , exp_name, 'WRF4G.tar.gz' ) } 
        input_files = join( rea_dir , 'wrf4g_files.tar.gz' )
        if exists( input_files ) :
            inputsandbox += ",file://%s" % ( input_files )
        # files to add for the outputsandbox
        outputsandbox = "log_%s_$GW_JOB_ID_$GW_RESTARTED_.tar.gz" % ( self.chunk_id.chunk_id )
        var_resources = VarEnv( join( rea_dir , 'resources.wrf4g' )  )
        arguments = '%s %s %d %d %s %s' % (
                                         rea_name,
                                         rea_id,
                                         self.chunk_id.chunk_id,
                                         self.chunk_id.id,
                                         datetime2datewrf(self.chunk_id.sdate),
                                         datetime2datewrf(self.chunk_id.edate)
                                        )
        job.create_template( 
                            name          = join(rea_dir, rea_name + '__' + str( self.chunk_id.chunk_id ) ),
                            arguments     = arguments,
                            np            = int( var_resources.get_variable( 'NP', default = '1') ),
                            req           = var_resources.get_variable( 'REQUIREMENTS' ),
                            environ       = var_resources.get_variable( 'ENVIRONMENT' ),
                            inputsandbox  = inputsandbox,
                            outputsandbox = outputsandbox
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
         
    def should_run(self, gwres):
        """
        Check if the job should run
        """
        #Last job with the same distinct field gw_job 
        try :
            last_job = self.session.query( Job.gw_job == self.gw_job).\  
                                           order_by( Job.id ).all()[-1]
        except :
            logger.debug('ERROR: Could not find the last job')
            return False
        if last_job.gw_restarted < gwres:
            self.gw_restarted = gwres
        elif last_job.gw_restarted == gwres:
            self.id = last_job.id
        else:
            #if last_job.gw_restarted>gwres
            logger.debug('ERROR: This job should not be running this Chunk')
            return False
        return True
    
    @staticmethod
    def create_rea_storage(self, rea_dir):
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
        for conf_file in [ "db.conf", "experiment.wrf4g", "namelist.input" ] :
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
            for key, val in dict_values.items() :
               os.environ[ key ] = val
         os.system( 'bash' ) 
            
