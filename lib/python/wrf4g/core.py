__version__  = '2.0.0'
__author__   = 'Carlos Blanco'
__revision__ = "$Id$"


import logging
import os
import re
import tarfile
import shutil
import datetime

from sqlalchemy             import func
from sqlalchemy.orm.exc     import NoResultFound
from os.path                import exists, expandvars, expanduser, isdir, join
from datetime               import datetime, timedelta 
from wrf4g                  import WRF4G_DIR, WRF4G_DEPLOYMENT_DIR, logger
from wrf4g.db               import ExperimentModel, RealizationModel, ChunkModel, JobModel
from wrf4g.utils            import datetime2datewrf, exec_cmd, Calendar, validate_name
from wrf4g.tools.vcplib     import VCPURL
from wrf4g.tools.gridwaylib import Job as GWJob


# Realization, Chunk and Job status
REA_STATUS   = ( 'PREPARED', 'SUBMITTED', 'RUNNING',
                 'PENDING', 'FAILED', 'FINISHED'
                )

CHUNK_STATUS = ( 'PREPARED' , 'SUBMITTED', 'RUNNING',      
                 'PENDING', 'FAILED', 'FINISHED'
                )

JOB_STATUS   = ( 'PREPARED', 'SUBMITTED', 'RUNNING', 'PENDING', 'CANCEL',
                 'FAILED', 'FINISHED', 'PREPARING_WN', 'DOWN_BIN',
                 'DOWN_RESTART', 'DOWN_WPS', 'DOWN_BOUND', 'UNGRIB',
                 'METGRID', 'REAL', 'UPLOAD_WPS', 'ICBCPROCESOR', 'WRF'
                )

JOB_ERROR = { 
              'EXPERIMENT_FILE' : 1,
              'LOCAL_PATH' : 2,
              'LOG_PATH' : 3,
              'JOB_SHOULD_NOT_RUN' : 4,
              'COPY_RST_FILE' : 5,
              'RESTART_MISMATCH' : 6,
              'COPY_NAMELIST_WPS' : 7,
              'COPY_REAL_FILE' : 8,
              'COPY_WPS' : 9,
              'PREPROCESSOR_FAILED' : 10,
              'UNGRIB_FAILED' : 11,
              'METGRID_FAILED' : 12,
              'REAL_FAILED' : 13,
              'COPY_UPLOAD_WPS' : 14,
              'WRF_FAILED' : 15,
              'POSTPROCESSOR_FAILED' : 16,
              'COPY_OUTPUT_FILE' : 17,
              'COPY_NODES':18,
              }
             
class Experiment( ExperimentModel ):
    """ 
    Manage WRF4G experiments

    """

    def run(self, rerun = False, dryrun = False ):
        """
        Run the realizations of this experiment
        n_chunk is the number of chunks to run. 
        If there are not n_chunk, run every chunk since the last one finished 
        """
        #Check if the experiment have some realization to run
        #list of realizations of the experiment
        q_realizations = self.rea.query.all()
        if not ( q_realizations ):
            logger.warn( 'There are not realizations to run.' )
        else:
            #if there are realizations to run
            for rea in q_realizations :
                logger.info('Submitting Realization: "%s" with restart %s' % (rea.name, rea.restart ) )
                #Run every realization
                rea().run( rerun, dryrun )

    def check_db(self, new_realization ):
        """ 
        Check if there is a realization with the same no reconfigurable field. 
        If there is not a realization with the same no reconfigurable fields => error
        If there is a realization with the same reconfigurable fields, update data
        """
        #Check if there is a realization with the same no reconfigurable fields:
        #id,id_exp,sdate,multiple_parameters
        try:
            rea = self.realization.filter( self.realization.name        == new_realization.name,
                                           self.realization.sdate       == new_realization.sdate,
                                           self.realization.mult_labels == new_realization.mult_labels
                                           ).one()
        except Exception:
            #if rea does not exist (no realization with the same no reconfigurable fields)
            raise Exception("ERROR: Realization with the same name and no reconfigurable fields already exists") 
        else: 
            #if rea exits,it means a realization with the same no reconfigurable fields
            logger.debug('Updating realization on the database...')
            rea = new_realization 

    def _create_wrf4g_bundle(self):
        """
        Create a bundle with the necessary software to run WRF on WNs
        """
        looger.debug( "Create a WRF4G software bundle to use on the WN..." )
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

    def read_exp_file(self, directory = './' ):
        """
        Read the experiment configuration from experiment.wrf4g file.
        """
        # Sanity check 
        exp_dir = expandvars( expanduser( directory ) )
        if not exists( exp_dir ):
            raise Exception("'%s' does not exist" % exp_dir )
        exp_file = join( exp_dir, 'experiment.wrf4g' )
        if not exists( exp_file ):
            raise Exception("'%s' does not exist" % exp_file ) 
        logger.debug( "Reading '%s' file" % exp_file )
        exp_features  = VarEnv( exp_file )
        # Defining experiment variables
        name            = exp_features.get_variable( 'experiment_name' )
        home_dir        = directory
        calendar        = exp_features.get_variable( 'calendar', 'standard' )
        sdate           = datetime.strptime(exp_features.get_variable( 'start_date' ), "%Y-%m-%d_%H:%M:%S")
        edate           = datetime.strptime(exp_features.get_variable( 'end_date' ), "%Y-%m-%d_%H:%M:%S"),
        csize           = exp_features.get_variable( 'chunk_size_h' , ( sdate - edate ).total_seconds()/3600 ) 
        np              = int( exp_features.get_variable('np', '1') )
        environ         = exp_features.get_variable( 'environ', '')
        description     = exp_features.get_variable( 'description', '')
        requirements    = exp_features.get_variable( 'requirements', '')
        mult_parameters = 1 if exp_features.has_section( 'multiple_parameters' ) else 0
        mult_dates      = 1 if exp_features.has_section( 'multiple_dates' )      else 0
        if mult_parameters :
            mult_labels = exp_features.get_variable( 'combinations', 'multiple_parameters' ).\
                                replace(' ', '').replace('\n', '/')
            mult_vars   = exp_features.get_variable( 'variables', 'multiple_parameters', '' ).\
                                replare(' ', '')
            mult_nitems = exp_features.get_variable( 'nitems', 'multiple_parameters', '' ).\
                                replare(' ', '')
        else :
            mult_labels = ''
        if mult_dates :
            smul_length_h = int( exp_features.get_variable( 'simulation_length_h', 'multiple_dates' ) )
            if not smul_length_h :
                raise Exception( "'simulation_length_h' variable is madatory for 'multiple_dates' section." )
            smul_interval_h = int( exp_features.get_variable( 'simulation_interval_h', 'multiple_dates' ) )
            if not smul_interval_h :
                raise Exception( "'simulation_interval_h' variable is madatory for 'multiple_dates' section." )
        namelist_version = exp_features.get_variable( 'namelist_version' )
        if not namelist_version :
            raise Exception( "'namelist_version' variable is madatory." )
        if exp_features.has_section( 'NI' ) :
            restart_interval = int( exp_features.get_variable( 'restart_interval' , 'NI', csize * 60 ) )
        else :
            restart_interval = csize * 60
        max_dom = int( exp_features.get_variable( 'max_dom' ) )
        if not max_dom :
            raise Exception( "'max_dom' variable is madatory." )
        NI  = exp_features.items( 'NI' )  if exp_features.has_section( 'NI'  ) else []
        NIN = exp_features.items( 'NIN' ) if exp_features.has_section( 'NIN' ) else []
        NIM = exp_features.items( 'NIM' ) if exp_features.has_section( 'NIM' ) else []
        # Return all variables  
        return ( name, home_dir, calendar, sdate, edate, csize, np, environ, description, requirements,
                 mult_parameters, mult_labels, mult_vars, mult_nitems, mult_dates, smul_length_h, 
                 smul_interval_h, namelist_version, restart_interval, max_dom, NI, NIM, NIN )

    def create(self, update = False, dryrun = False,  directory = './' ):
        """
        Create and prepare all realizations and chunks needed to submit a WRF4G experiment 
        """
        ( name, home_dir, calendar, sdate, edate, csize, np, environ, description, requirements,
          mult_parameters, mult_labels, mult_vars, mult_nitems, mult_dates, smul_length_h, 
          smul_interval_h, namelist_version, restart_interval, max_dom, NI, NIM, NIN  )  = self.read_exp_file( directory )
        if update and ( name == self.name and sdate == self.sdate and \
                mult_dates == self.mult_dates and mult_parameters == self.mult_parameters ) :
            raise Exception("ERROR: Experiment with the same name and no reconfigurable fields already exists.")
        # Update experiment variables
        self.name = name, self.home_dir = home_dir, self.calendar = calendar
        self.sdate = sdate, self.edate = edate, self.csize = csize, self.np = np
        self.environ = environ, self.description = description, self.requirements = requirements
        self.namelist_version = namelist_version, self.restart_interval = restart_interval
        self.max_dom = max_dom, self.mult_parameters = mult_parameters
        self.mult_labels = mult_labels, self.mult_vars = mult_vars, self.mult_nitems = mult_nitems
        self.mult_dates = mult_dates, self.smul_length_h = smul_length_h, self.smul_interval_h = smul_interval_h
        if not dryrun :
            # Create a WRF4G software bundle to use on the WN
            self._create_wrf4g_bundle()
            # Modify the namelist with the parameters available in experiment.wrf4g
            logger.info( "Preparing namelist..." )
            namelist_template = join( WRF4G_DIR , 'etc', 'templates', 'namelist', 'namelist.input-%s' % 
                                     self.namelist_version )
            namelist_input    = join( directory, 'namelist.input' )
            try :
                logger.debug("Copying '%s' to '%s'" % (namelist_template, namelist_input) )
                shutil.copyfile(namelist_template, namelist_input )
            except :
                raise Exception( "There is not a namelist template for WRF '%s'"
                                 " (File namelist.input does not exist)" % namelist_template )
            logger.debug( "Updating parameter 'max_dom' in the namelist" )
            exec_cmd( "fortnml -wof %s -s max_dom %d" % ( namelist_input, self.max_dom ) )
            for key, val in NI :
                logger.debug( "Updating parameter '%s' in the namelist" % key )
                exec_cmd( "fortnml -wof %s -s %s %s" % (namelist_input, key, val) )
            for key, val in NIM :
                logger.debug( "Updating parameter '%s' in the namelist" % key )
                exec_cmd( "fortnml -wof %s -s %s %s" % (namelist_input, key, val ) )
            for key, val in NIN :
                logger.debug( "Updating parameter '%s' in the namelist" % key )
                exec_cmd( "fortnml -wof %s -m %s %s" % (namelist_input, key, val) )
        if self.mult_dates :
           # If there is a multiple_parameter
           for rea_label in self.mult_labels.split('/') :
               rea_tag, combinations = rea_label.split('|')
               logger.info( "---> Realization: multiparams=%s %s %s" % (rea_tag, sdate, edate) )
               if not dryrun :
                   l_variables    = self.mult_vars.split(',')
                   l_combinations = combinations.split(',')
                   l_nitems       = self.mult_nitems.split(',')
                   for var, comb, nitem in zip( l_variables, l_combinations, l_nitems ) :
                       # Update the namelist per each combination
                       logger.debug( "Updating parameter '%s' in the namelist" % var )
                       if ':' in comb :
                           exec_cmd( "fortnml -wof %s -s %s -- %s" % (namelist_input, var, ' '.join( comb.split(':') ) ) )
                       else :
                           exec_cmd( "fortnml -wof %s -s %s -n %s -- %s" % (namelist_input, var, nitem, comb ) )
               self.cycle_time( "%s__%s" % ( name, rea_tag ), rea_tag, reconfigure, dryrun)
        else :
           logger.info( "---> Single params run" )
           self.cycle_time( name, self.mult_labels, reconfigure, dryrun )
    
    def cycle_time(self, rea_name, label, update = False, dryrun = False ) :
        """
        Check if the experiment has multiple_dates 
        """
        if self.mult_dates :
            self.cycle_hindcasts( rea_name, label, update, dryrun )
        else :
            logger.info( "---> Continuous run" )
            # Create realization
            rea = Realization( 
                            name          = rea_name,
                            sdate         = self.sdate,
                            edate         = self.edate, 
                            cdate         = self.sdate,
                            status        = 'PREPARED',
                            current_chunk = 1,
                            mult_label    = label 
                            )
            if update :
                # Check realization on the database
                self.check_db( rea )
            else :
                # Add realization to the experiment 
                self.realization.append( rea )
            # Check storage
            if not dryrun :
                rea._prepare_sub_files()
            rea.cycle_chunks( update )
 
    def cycle_hindcasts(self, rea_name, label, update = False, dryrun = False ) :
        """
        Create chunks the needed for a realization 
        """
        logger.info( "\n---> cycle_hindcasts: %s %s %s %s %s" % ( 
                      rea_name, self.id, self.sdate , self.edate, self.mult_label ) )
        # Define which calendar is going to be used
        exp_calendar = Calendar(self.calendar)
        rea_sdate    = self.sdate
        while rea_sdate <= ( exp_calendar.sub_hours(self.edate,  self.smul_interval_h ) ) :
            rea_edate = exp_calendar.add_hours(rea_sdate, self.smul_interval_h )
            # Create realization
            rea = Realization( 
                                name          = "%s__%s_%s" % ( rea_name, rea_sdate, rea_edate),
                                sdate         = rea_sdate,
                                edate         = rea_edate,
                                cdate         = rea_sdate,
                                status        = 'PREPARED',
                                current_chunk = 1,
                                mult_label    = label 
                                )
            if update :
                # Check realization on the database
                self.check_db( rea )
            else :
                # Add realization to the experiment 
                self.realization.append( rea )
            # Check storage
            if not dryrun :
                rea._prepare_sub_files()
            rea.cycle_chunks( update )
            rea_sdate = rea_edate
    
    def status(self, long_format = False, rea_pattern = False ):
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
        if not rea_pattern:
            q_realizations = self.realization.query.all()
        else :
            q_realizations = self.realization.query.\
                filter( self.realization.name.like( rea_pattern.replace('*','%') ) ).all()
        #Header of the information
        if not ( q_realizations ):
            logger.info( 'There are not realizations to check.' )
        if long_format :
            logger.info( '%-21s %-8.8s %-10s %-10s %-13s %2s %4s 20% 20% 20%'% (
                                   'Realization','Status','Chunks','Comp.Res','Run.Sta','ext','%','sdate', 'rdate', 'edate')
                       )
        else :
            logger.info( '%-21s %-8s %-10s %-10s %-13s %2s %4s'% (
                                   'Realization','Status','Chunks','Comp.Res','Run.Sta','ext','%')
                       )
        for rea in q_realizations :
            #Print information of each realization
            rea().status( long_format )
    
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

    def stop(self, dryrun = False ):
        """
        Delete jobs which status is running or submitted 
        """
        #list of realization of the experiment
        q_realizations = self.rea.query.all()  
        if not ( q_realizations ):
            logger.info( 'There are not realizations to stop.' )
        else :
            logger.info( 'Stopping Experiment %s' % self.name )
            for rea in q_realizations :  
                rea().stop( dryrun )

    def delete(self, dryrun = False ):
        """
        Delete the experiment ,its realizations and chunks 
        """
        if not dryrun :
            local_exp_dir = join( WRF4G_DIR , 'var' , 'submission' , self.name )
            logger.debug( "Deleting '%s' directory" % local_exp_dir )
            shutil.rmtree( local_exp_dir )
    
class Realization( RealizationModel ):
    """
    A class to mange WRF4G realizations
    Methods:
            run
            stop
    """

    def run(self, first_chunk_run=1, last_chunk_run=1, rerun=False, dryrun=False):
        """ 
        Run n_chunk of the realization.
        If n_chunk=0 run every chunk of the realization which haven't finished yet
        else run (n_chunk) chunks since the last one finished
        """
        #Check the status of the realization
        if self.status == 'FINISHED' and not rerun :
            logger.warn( "Realization '%s' already finished." % self.name )
        elif ( self.status == 'SUBMITTED' or self.status == 'RUNNING' ) and not rerun :
            logger.warn( "Realization '%s' has been submitted." % self.name )
        elif first_chunk_run < 0 :
            logger.error( "ERROR: The first chunk to run is '%d'." % first_chunk_run ) 
        elif last_chunk_run  < 0 :
            logger.error( "ERROR: The last chunk to run is '%d'." % last_chunk_run )
        elif last_chunk_run > first_chunk_run :
            logger.error( "ERROR: The last chunk to run is greater than the fist one." )
        else :
            # search first chunk to run 
            if rerun and self.first_chunk_run == 1 :
                id_first_chunk_run = first_chunk_run
                ch = self.chunk.query.filter( self.chunk.chunk_id == first_chunk_run ).one()
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
                        first_chunk_run    = self.chunk.query.filter( 
                            self.chunk.sdate <= self.restart & self.chunk.edate >= self.restart 
                            ).one()
                        id_first_chunk_run = first_chunk_run.id
                    except :
                        raise Exception( 'There are not chunks to run.' )
            #search last chunk to run
            if not last_chunk_run :
                #run every chunk
                #Search last chunk of the realization
                id_last_chunk_run = self.chunk.query.order_by( self.chunk.id )[-1].id
            else:
                #search last chunk
                id_last_chunk_run = last_chunk_run
            #Search chunks to run
            q_chunks = self.chunk.query.filter( 
                            self.chunk.id >= id_first_chunk_run & self.chunk.id <= id_last_chunk_run )
            #run chunks
            for chunk in q_chunks :
                #print data of chunks
                logger.info('\t\tSubmitting Chunk %d:\t%s\t%s' % ( self.chunk.chunk_id, 
                                                          datetime2datewrf(self.chunk.sdate), 
                                                          datetime2datewrf(self.chunk.edate) ) )
                if not dryrun :
                    chunk().run( rerun )
            
    def _prepare_sub_files(self):
        """
        Prepare the files needed to submit the realization 
        """
        rea_submission_dir = join( WRF4G_DIR , 'var' , 'submission' , self.experiment.name , self.name )
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
        for file in ( join( WRF4G_DIR, 'etc', 'db.conf' ), "experiment.wrf4g", "namelist.input" ) :
            if not exists ( expandvars( file ) ) :
                raise Exception( "'%s' is not available" % file )
            else :
                shutil.copy( expandvars( file ) , rea_submission_dir )

    def check_db(self, new_chunk ):
        """ 
        Check if there is a chunk with the same no reconfigurable field. 
        If there is not a chunk with the same no reconfigurable fields => error,.
        If there is a chunk with the same reconfigurable fields, update data.
        """
        #Check if there is a chunk with the same no reconfigurable fields:
        #id,id_rea,id_chunk,sdate
        try:
            ch = self.chunk.query(  self.chunk.id       == new_chunk.id,
                                    self.chunk.rea_id   == new_chunk.rea_id,
                                    self.chunk.chunk_id == new_chunk.chunk_id,
                                    self.chunk.sdate    == new_chunk.sdate
                                    ).one()
        except Exception:
            #if ch does not exist (no chunk with the same no reconfigurable fields)
            raise Exception("ERROR: Chunk with the same name and no reconfigurable fields already exists")
        else:
            #if ch exits,it means a chunk with the same no reconfigurable fields
            logger.debug('Updating chunk on the database')
            ch = new_chunk


    def cycle_chunks(self, update = False ):
        """
        Create chunks the needed for a realization 
        """
        logger.info( "\t---> cycle_chunks: %s %s %s" % ( self.name, self.sdate , self.edate ) )
        # Define which calendar is going to be used
        exp_calendar = Calendar(self.calendar)
        chunk_id = 1
        chunk_sdate = self.sdate
        while chunk_sdate <= ( exp_calendar.sub_hours(self.edate, self.csize ) ) :
            chunk_edate = exp_calendar.add_hours( chunk_sdate, hours = self.csize )
            logger.info( "\t\t---> chunk %d: %s %s %s" %( chunk_id, self.name, chunk_sdate, chunk_edate ) )
            # Create Chunk
            ch = Chunk( rea_id   = self.id, 
                        sdate    = chunk_sdate, 
                        edate    = chunk_edate,
                        wps      = 0, 
                        chunk_id = chunk_id,
                        status   = 'PREPARED' )
            if update :
                # Check chunk on the database
                self.check_db( ch )
            else :
                # Add realization to the experiment 
                self.chunk.append( ch )
            chunk_sdate = chunk_edate 
            chunk_id    = chunk_id + 1
        # Set the number of chunks of a relaization    
        self.realization.nchunks = chunk_id
 
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
        #Select parameters:job_status,rea_status,resource,exitcode,nchunks
        #Last job of current chunk
        last_job = self.chunk.job.query.filter( self.chunk.job.chunk_id == self.current_chunk ).\
                        order_by( self.chunk.job.id )[ -1 ]
        chunk_distribution = '%d/%d' % ( self.current_chunk, self.nchunks )
        #Format chunks run / chunks total
        runt   =  int( self.cdate.strftime("%s") ) - int( self.sdate.strftime("%s") ) 
        totalt =  int( self.edate.strftime("%s") ) - int( self.sdate.strftime("%s") )
        #Percentage
        per = runt * 100.0/ totalt
        #Print output
        if long_format :
            logger.info( "%-21s %-8s %-10s %-10s %-13s %2s %4.2f %20s %20s %20s" % (
                    self.name[0:20], self.status, chunk_distribution, last_job.resource[0:10], 
                    last_job.status, str(last_job.exitcode), per,
                    self.sdate, self.restart, self.edate )
                    )
        else :
            logger.info( "%-21s %-8s %-10s %-10s %-13s %2s %4.2f" % (
                    self.name[0:20], self.status, chunk_distribution, last_job.resource[0:10], 
                    last_job.status, str(last_job.exitcode), per )
                    ) 
  
    def stop(self, dryrun=False):
        """
        Delete chunks which status is running or submitted 
        """
        q_chunks = self.chunk.query.filter( 
                                    self.chunk.status == 'SUBMITTED' &  self.chunk.status == 'RUNNING' 
                                    ).all()
        if not ( q_chunks ):
            logger.info( 'There are not chunks to stop.' )
        else :
            logger.info('\tStopping Realization %s' % self.name )
            for chunk in q_chunks :
                chunk().stop( dryrun )
    
class Chunk( ChunkModel ):
    """ 
    A class to manage WRF4G chunks
    Methods:
            run
            stop
    """
 
    #METHODS
    def run (self, rerun = False):
        """ 
        Run a chunk is run a drm4g job
        """
        #Send a gridway's job and save data in table Job
        job = Job()  #create an object "job"
        job.run( rerun ) #run job
        self.job.append( job )

    def stop(self, dryrun = False ):
        """
        Delete jobs
        """
        logger.info('\t\tDeleting Chunk %d:\t%s\t%s' % ( self.chunk_id,
                                                         datetime2datewrf(self.sdate),
                                                         datetime2datewrf(self.edate) ) 
                                                       )
        q_jobs = self.job.query.filter( self.job.status != 'PREPARED' |  self.job.status == 'FALIED'
                                        ).all()
        if not ( q_jobs ):
            logger.info( 'There are not jobs to stop.' )
        else :
            for job in q_jobs :
                job().stop( dryrun )
       
class Job( JobModel ):
    """
    A class to manage WRF4G jobs
    Methods:
            set_status
            run
            stop
    """
    
    def set_status(self, status):
        """ 
        Save the status of the job and if it is a jobstatus of CHUNK_STATUS and REA_STATUS,
        change the status of the Chunk and tthe Realization and add an event. 
        """
        #Save job's status
        self.status = status
        #if is an status of the CHUNK_STATUS and REA_STATUS
        if status in CHUNK_STATUS and status in REA_STATUS : 
            self.chunk.status = status
            if status == 'FINISHED' and self.chunk_id == self.chunk.realization.nchunks :
                self.chunk.realization.status == 'FINISHED'
            else :
                self.chunk.realization.status = status
                self.chunk.realization.current_chunk = self.chunk_id + 1
        #Add event
        events            = Events()
        events.job_status = status
        events.timestamp  = datetime.utcnow()
        self.events.append( events ) 
        
    def run(self, rerun = False):
        """ 
        Send a gridway's job and save data in table Job
        id_chunk is an instance of Chunk
        """
        if not self.gw_restarted :
            self.gw_restarted = 0
        # create gridway's job
        job      = GWJob()
        # create template
        rea_name = self.chunk.realization.name
        exp_name = self.chunk.realization.experiment.name
        rea_dir  = join( WRF4G_DIR , 'var' , 'submission' , exp_name, rea_name )
        if not exists(  wrf4g_package ) : 
            raise Exception( "'%s' file does not exist" % wrf4g_package )
        # files to add for the inputsandbox 
        inputsandbox  = "file://%(rea_dir)s/%(wrf4g_package)s,"                 
        inputsandbox += "file://%(rea_dir)s/db.conf,"          
        inputsandbox += "file://%(rea_dir)s/experiment.wrf4g," 
        inputsandbox += "file://%(rea_dir)s/namelist.input"  
        inputsandbox % { "rea_dir"       : rea_dir , 
                         "wrf4g_package" : join( WRF4G_DIR , 'var' , 'submission' , exp_name, 'WRF4G.tar.gz' ) } 
        input_files = join( rea_dir , 'wrf4g_files.tar.gz' )
        if exists( input_files ) :
            inputsandbox += ",file://%s" % ( input_files )
        # files to add for the outputsandbox
        outputsandbox = "log_%d_$GW_JOB_ID_$GW_RESTARTED.tar.gz" % ( self.chunk_id )
        arguments = '%s %s %d %s %s %s' % (
                                        exp_name,
                                        rea_name,                                     
                                        self.chunk.chunk_id,
                                        self.chunk.sdate,
                                        self.chunk.edate,
                                        '1' if rerun else '0'
                                        )
        job.create_template( 
                            name          = join(rea_dir, rea_name + '__' + str( self.chunk_id ) ),
                            arguments     = arguments,
                            np            = self.chunk.realization.experiment.np,
                            req           = self.chunk.realization.experiment.requirements,
                            environ       = self.chunk.realization.experiment.environment,
                            inputsandbox  = inputsandbox,
                            outputsandbox = outputsandbox
                            )
        #submit job
        # if the first chunk of the realization
        if self.chunk.chunk_id == 1 :
            self.gw_job = job.submit()
        else:
            #if the chunk is not the first of the realization, gwsubmit has an argument, gw_job of the job before
            id_chunk_before = self.chunk.id - 1
            id_job_before   = self.chunk.query( self.job ).\
                                filter( self.chunk == id_chunk_before).\
                                order_by( self.id )[-1].id
            gw_job_before   = self.job.query( self.job ).get( id_job_before ).gw_job
            self.gw_job     = job.submit( dep = gw_job_before )
        #Insert values default
        self.resource = '-'
        self.exitcode = '-'
        # Update status
        self.set_status( 'SUBMITTED' ) 
   
    def stop(self, dryrun=False):
        """
        Delete a job
        """
        logger.info('\t\t\tDeleting Job %d' % self.gw_job ) 
        if not dryrun :
            GWJob().kill( self.gw_job )
            self.set_status( 'CANCEL' )
