__version__  = '2.0.0'
__author__   = 'Carlos Blanco'
__revision__ = "$Id$"

import os
import re
import logging
import tarfile
import shutil
import datetime

from sqlalchemy             import ( Column, INTEGER, 
                                     VARCHAR, SMALLINT, 
                                     DATETIME, ForeignKey,
                                     create_engine, func, 
                                     and_, or_ )
from sqlalchemy.orm         import relationship
from sqlalchemy.orm.exc     import NoResultFound
from os.path                import ( exists, expandvars, 
                                     expanduser, isdir, 
                                     join )
from datetime               import datetime, timedelta 
from wrf4g                  import WRF4G_DIR, WRF4G_DEPLOYMENT_DIR, logger
from wrf4g.db               import Base
from wrf4g.utils            import ( datetime2datewrf, exec_cmd, 
                                     Calendar, validate_name, 
                                     VarEnv )
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
             
class Experiment( Base ):
    """ 
    Manage WRF4G experiments
    """
    __tablename__   = 'experiment'
    
    # Columns
    id              = Column(u'id', INTEGER, primary_key=True, nullable=False)
    name            = Column(u'name', VARCHAR(length=512), nullable=False)
    sdate           = Column(u'sdate', DATETIME())
    edate           = Column(u'edate', DATETIME())
    csize           = Column(u'csize', INTEGER)
    calendar        = Column(u'calendar', VARCHAR(length=300))
    home_dir        = Column(u'home_directory', VARCHAR(length=300))
    np              = Column(u'np', INTEGER)
    requirements    = Column(u'requirements', VARCHAR(length=1024)) 
    environment     = Column(u'environment', VARCHAR(length=1024)) 
    namelist_version= Column(u'namelist_versiont', VARCHAR(length=1024)) 
    restart_interval= Column(u'restart_interval', INTEGER) 
    max_dom         = Column(u'max_dom', INTEGER)
    mult_dates      = Column(u'multiple_dates', INTEGER) 
    smul_length_h   = Column(u'simulation_length_h', INTEGER)
    smul_interval_h = Column(u'simulation_interval_h', INTEGER)
    mult_parameters = Column(u'multiple_parameters', INTEGER)
    mult_labels     = Column(u'multiparams_labels', VARCHAR(length=1024)) 
    mult_vars       = Column(u'multiparams_variables', VARCHAR(length=1024)) 
    mult_nitems     = Column(u'multiparams_nitems', VARCHAR(length=1024)) 
    description     = Column(u'description', VARCHAR(length=1024)) 

    # Realtionships
    realization     = relationship("Realization", back_populates="experiment", lazy='dynamic')

    def run(self, rerun = False, dryrun = False ):
        """
        Run the realizations of this experiment
        n_chunk is the number of chunks to run. 
        If there are not n_chunk, run every chunk since the last one finished 
        """
        #Check if the experiment have some realization to run
        #list of realizations of the experiment
        l_realizations = self.realization.all()
        if not ( l_realizations ):
            logger.warn( 'There are not realizations to run.' )
        else:
            #if there are realizations to run
            for rea in l_realizations :
                logger.info("---> Submitting Realization: '%s'" % rea.name )
                #Run every realization
                rea.run( rerun = rerun, dryrun = dryrun )

    def check_db(self, new_realization ):
        """ 
        Check if there is a realization with the same no reconfigurable field. 
        If there is not a realization with the same no reconfigurable fields => error
        If there is a realization with the same reconfigurable fields, update data
        """
        #Check if there is a realization with the same no reconfigurable fields:
        #id,id_exp,sdate,multiple_parameters
        try:
            rea = self.realization.filter_by( Realization.name        == new_realization.name,
                                              Realization.sdate       == new_realization.sdate,
                                              Realization.mult_labels == new_realization.mult_labels
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
        logger.debug( "Create a WRF4G software bundle to use on the WN..." )
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
        home_dir        = os.getcwd() if directory == './' else directory
        calendar        = exp_features.get_variable( 'calendar', 'standard' )
        sdate           = datetime.strptime(exp_features.get_variable( 'start_date' ), "%Y-%m-%d_%H:%M:%S") 
        edate           = datetime.strptime(exp_features.get_variable( 'end_date' ), "%Y-%m-%d_%H:%M:%S")
        csize           = int( exp_features.get_variable( 'chunk_size_h' , ( sdate - edate ).total_seconds()/3600 ) )
        np              = int( exp_features.get_variable('np', '1') )
        environment     = exp_features.get_variable( 'environment' )
        description     = exp_features.get_variable( 'description' )
        requirements    = exp_features.get_variable( 'requirements' )
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
            mult_vars   = ''
            mult_nitems = ''
        if mult_dates :
            smul_length_h = int( exp_features.get_variable( 'simulation_length_h', 'multiple_dates' ) )
            if not smul_length_h :
                raise Exception( "'simulation_length_h' variable is madatory for 'multiple_dates' section." )
            smul_interval_h = int( exp_features.get_variable( 'simulation_interval_h', 'multiple_dates' ) )
            if not smul_interval_h :
                raise Exception( "'simulation_interval_h' variable is madatory for 'multiple_dates' section." )
        else :
            smul_length_h   = 0
            smul_interval_h = 0
        namelist_version = exp_features.get_variable( 'namelist_version' )
        if not namelist_version :
            raise Exception( "'namelist_version' variable is madatory." )
        if exp_features.has_section( 'NI' ) :
            restart_interval = int( exp_features.get_variable( 'restart_interval', csize * 60, 'NI' ) )
        else :
            restart_interval = csize * 60
        max_dom = int( exp_features.get_variable( 'max_dom' ) )
        if not max_dom :
            raise Exception( "'max_dom' variable is madatory." )
        NI  = exp_features.items( 'NI' )  if exp_features.has_section( 'NI'  ) else []
        NIN = exp_features.items( 'NIN' ) if exp_features.has_section( 'NIN' ) else []
        NIM = exp_features.items( 'NIM' ) if exp_features.has_section( 'NIM' ) else []
        # Return all variables  
        return ( name, home_dir, calendar, sdate, edate, csize, np, environment, description, requirements,
                 mult_parameters, mult_labels, mult_vars, mult_nitems, mult_dates, smul_length_h, 
                 smul_interval_h, namelist_version, restart_interval, max_dom, NI, NIM, NIN )

    def create(self, update = False, dryrun = False,  directory = './' ):
        """
        Create and prepare all realizations and chunks needed to submit a WRF4G experiment 
        """
        ( name, home_dir, calendar, sdate, edate, csize, np, environment, description, requirements,
          mult_parameters, mult_labels, mult_vars, mult_nitems, mult_dates, smul_length_h, 
          smul_interval_h, namelist_version, restart_interval, max_dom, NI, NIM, NIN  )  = self.read_exp_file( directory )
        if update and ( name == self.name and sdate == self.sdate and \
                mult_dates == self.mult_dates and mult_parameters == self.mult_parameters ) :
            raise Exception("ERROR: Experiment with the same name and no reconfigurable fields already exists.")
        if not update and name != self.name :
            raise Exception("ERROR: experiment.wrf4g files has a different experiment name.")
        # Update experiment variables
        self.name = name; self.home_dir = home_dir; self.calendar = calendar
        self.sdate = sdate; self.edate = edate; self.csize = csize; self.np = np
        self.environment = environment; self.description = description; self.requirements = requirements
        self.namelist_version = namelist_version; self.restart_interval = restart_interval
        self.max_dom = max_dom; self.mult_parameters = mult_parameters
        self.mult_labels = mult_labels; self.mult_vars = mult_vars; self.mult_nitems = mult_nitems
        self.mult_dates = mult_dates; self.smul_length_h = smul_length_h; self.smul_interval_h = smul_interval_h
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
                           exec_cmd( "fortnml -wof %s -s %s %s" % (namelist_input, var, ' '.join( comb.split(':') ) ) )
                       else :
                           exec_cmd( "fortnml -wof %s -s %s -n %s %s" % (namelist_input, var, nitem, comb ) )
               self.cycle_time( "%s__%s" % ( name, rea_tag ), rea_tag, update, dryrun)
        else :
           logger.info( "---> Single params run" )
           self.cycle_time( name, self.mult_labels, update, dryrun )
    
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
        exp_calendar = Calendar(self.experiment.calendar)
        rea_sdate    = self.sdate
        while rea_sdate < self.edate :
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
    
    def get_status(self, long_format = False, rea_pattern = False ):
        """ 
        Show information about realizations of the experiment for example:
        
        Realization Status   Chunks     Comp.Res      Run.Sta     JID ext      %
        testc       Done        3/3     mycomputer   Finished       0   0 100.00
        
        * Realization: Realization name. It is taken from the field experiment_name in experiment.wrf4g.
        * Status: It can be take the following values: Prepared, Submitted, Running, Failed and Done).
        * Chunks [Chunk running/Total Chunks]: A realization is split into chunks. Each chunk is sent as a job.
        * Computer resource: Resource (cluster) where the job is running.
        * Run.Sta: Job status in the WN (Downloading data, running ungrib, real, wrf, ...)
        * JID: Job identifier.
        * ext: Exit Code. If exit code is different from 0, there has been an error. 
        * % : percentage of simulation finished.
        """
        #list of realization of the experiment
        if rea_pattern :
            l_realizations = self.realization.filter( Realization.name.like( rea_pattern.replace('*','%') ) ) 
        else :
            l_realizations = self.realization.all()

        #Header of the information
        if not ( l_realizations ):
            logger.info( 'There are not realizations to check.' )
        if long_format :
            logger.info( '%-21s %-10s %-10s %-10s %-13s %6s %-3s %5s %20s %20s'% (
                                   'Realization','Status','Chunks','Comp.Res','Run.Sta','JID','ext','%','sdate', 'edate')
                       )
        else :
            logger.info( '%-21s %-10s %-10s %-10s %-13s %6s %-3s %5s' % (
                                   'Realization','Status','Chunks','Comp.Res','Run.Sta','JID','ext','%')
                       )
        for rea in l_realizations :
            #Print information of each realization
            rea.get_status( long_format )
    
    @staticmethod 
    def create_files(name, template, force, directory):
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
        if exists( exp_dir_config ) and not force :
            raise Exception("'%s' already exists" % exp_dir_config )
        elif exists( exp_dir_config ) and force :
            shutil.rmtree( exp_dir_config )
        logger.debug( "Creating '%s' directory" % exp_dir_config )
        shutil.copytree( join( WRF4G_DIR , 'etc' , 'templates' , 'experiments',  template ),
                         exp_dir_config )
        dest_path = join( exp_dir_config , 'experiment.wrf4g' )
        with open( dest_path , 'r') as f :
            data = ''.join( f.readlines( ) )
        data_updated = data % {
                               'WRF4G_EXPERIMENT_HOME' : exp_dir_config ,
                               'WRF4G_DEPLOYMENT_DIR'  : WRF4G_DEPLOYMENT_DIR ,
                               'exp_name'              : name ,
                               }
        with open(dest_path, 'w') as f :
            f.writelines( data_updated ) 

    def stop(self, dryrun = False ):
        """
        Delete jobs which status is running or submitted 
        """
        #list of realization of the experiment
        l_realizations = self.realization.all()
        if not ( l_realizations ):
            logger.info( 'There are not realizations to stop.' )
        else :
            logger.info( 'Stopping Experiment %s' % self.name )
            for rea in l_realizations :  
                rea.stop( dryrun )

    def delete(self, dryrun = False ):
        """
        Delete the experiment ,its realizations and chunks 
        """
        if not dryrun :
            local_exp_dir = join( WRF4G_DIR , 'var' , 'submission' , self.name )
            logger.debug( "Deleting '%s' directory" % local_exp_dir )
            if exists( local_exp_dir ) :
                shutil.rmtree( local_exp_dir )
    
class Realization( Base ):
    """
    A class to mange WRF4G realizations
    """
    __tablename__   = 'realization'
    
    # Columns
    id              = Column(u'id',INTEGER, primary_key=True, nullable=False)
    exp_id          = Column(u'exp_id',INTEGER, ForeignKey(u'experiment.id')) 
    name            = Column(u'name',VARCHAR(length=1024),nullable=False)
    sdate           = Column(u'sdate',DATETIME())
    edate           = Column(u'edate',DATETIME())
    restart         = Column(u'restart',DATETIME()) 
    status          = Column(u'status',VARCHAR(length=20))
    cdate           = Column(u'cdate',DATETIME())
    ctime           = Column(u'ctime',DATETIME())
    current_chunk   = Column(u'current_chunk',INTEGER)
    nchunks         = Column(u'nchunks',INTEGER)
    mult_label      = Column(u'multiparams_label',VARCHAR(length=100)) 

    # Realtionships
    experiment      = relationship("Experiment", back_populates="realization")
    chunk           = relationship("Chunk", back_populates= "realization", lazy='dynamic')

    def run(self, first_chunk_run = None , last_chunk_run = None, rerun = False, dryrun = False):
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
        elif first_chunk_run and first_chunk_run < 0 :
            logger.error( "ERROR: The first chunk to run is '%d'." % first_chunk_run ) 
        elif last_chunk_run and last_chunk_run  < 0 :
            logger.error( "ERROR: The last chunk to run is '%d'." % last_chunk_run )
        elif ( last_chunk_run and first_chunk_run ) and last_chunk_run > first_chunk_run :
            logger.error( "ERROR: The last chunk to run is greater than the fist one." )
        else :
            # search first chunk to run 
            if rerun and first_chunk_run == 1 :
                ch = self.chunk.filter( Chunk.chunk_id == first_chunk_run ).one()
                self.restart = ch.sdate
                self.cdate   = ch.sdate
            elif rerun and not first_chunk_run :
                first_chunk_run = 1
                self.restart = None
                self.cdate   = self.sdate
            else :
                #search first chunk to run
                if not self.restart : # run every chunks of the realization
                    first_chunk_run = 1
                else:
                    #search chunk with edate>restart and sdate<restart
                    try:
                        first_chunk  = self.chunk.filter( _and( 
                            Chunk.sdate <= self.restart, Chunk.edate >= self.restart 
                            ) ).one()
                        #id_first_chunk_run = first_chunk_run.id
                        first_chunk_run = first_chunk.id_chunk
                    except :
                        raise Exception( 'There are not chunks to run.' )
            #search last chunk to run
            if not last_chunk_run :
                #run every chunk
                #Search last chunk of the realization
                last_chunk_run = self.nchunks
            else:
                #search last chunk
                last_chunk_run = last_chunk_run
            #Search chunks to run
            print first_chunk_run
            print last_chunk_run
            l_chunks = self.chunk.filter( and_( 
                            Chunk.chunk_id >= first_chunk_run, Chunk.chunk_id <= last_chunk_run 
                            ) )
            #run chunks
            for chunk in l_chunks :
                #print data of chunks
                logger.info('\t---> Submitting Chunk %d:\t%s\t%s' % ( chunk.chunk_id, 
                                                              datetime2datewrf(chunk.sdate), 
                                                              datetime2datewrf(chunk.edate) ) )
                if not dryrun :
                    chunk.run( rerun )
            # Update reealizaiton status
            self.status = 'SUBMITTED'
            
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
            ch = self.chunk.filter_by( Chunk.id       == new_chunk.id,
                                       Chunk.rea_id   == new_chunk.rea_id,
                                       Chunk.chunk_id == new_chunk.chunk_id,
                                       Chunk.sdate    == new_chunk.sdate
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
        exp_calendar = Calendar(self.experiment.calendar)
        chunk_id = 1
        chunk_sdate = self.sdate
        while chunk_sdate < self.edate :
            chunk_edate = exp_calendar.add_hours( chunk_sdate, hours = self.experiment.csize )
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
        self.nchunks = chunk_id - 1
 
    def get_status(self, long_format=False):
        """ 
        Show information about the realization for example:
            Realization Status   Chunks     Comp.Res   WN         Run.Sta     JID   ext      %
            testc       Done     3/3        mycomputer wn001     Finished       0     0 100.00
            
        * Realization: Realization name. It is taken from the field experiment_name in experiment.wrf4g.
        * Status: It can be take the following values: Prepared, Submitted, Running, Failed and Done).
        * Chunks [Chunk running/Total Chunks]: A realization is split into chunks. Each chunk is sent as a job.
        * Computer resource: Resource (cluster) where the job is running.
        * Run.Sta: Job status in the WN (Downloading data, running ungrib, real, wrf, ...)
        * JID: Job identifier
        * ext: Exit Code. If exit code is different from 0, there has been an error.
        * % : percentage of simulation finished.
        """                
        #Select parameters:job_status,rea_status,resource,exitcode,nchunks
        #Last job of current chunk
        if self.status == 'PREPARED' :
            resource = '-'
            exitcode = '-'
            status   = 'PREPARED'
            gw_job   = '-'
        else :
            current_chunk = self.chunk.filter( Chunk.chunk_id == self.current_chunk ).one()
            last_job = current_chunk.job.order_by( Job.id )[-1]
            resource = last_job.resource
            exitcode = str( last_job.exitcode )
            status   = last_job.status
            gw_job   = str( last_job.gw_job )

        chunk_distribution = '%d/%d' % ( self.current_chunk, self.nchunks )
        #Format chunks run / chunks total
        runt   =  int( self.cdate.strftime("%s") ) - int( self.sdate.strftime("%s") ) 
        totalt =  int( self.edate.strftime("%s") ) - int( self.sdate.strftime("%s") )
        #Percentage
        per = runt * 100.0/ totalt
        #Print output
        if long_format :
            logger.info( "%-21s %-10s %-10s %-10s %-13s %5s %3s %6.2f %20s %20s" % (
                    self.name[0:20], self.status, chunk_distribution, resource[0:10], status, 
                    gw_job, exitcode, per, self.sdate, self.edate )
                    )
        else :
            logger.info( "%-21s %-10s %-10s %-10s %-13s %5s %3s %6.2f" % (
                    self.name[0:20], self.status, chunk_distribution, resource[0:10], 
                    status, gw_job, exitcode, per )
                    ) 
  
    def stop(self, dryrun=False):
        """
        Delete chunks which status is running or submitted 
        """
        l_chunks = self.chunk.filter( or_( Chunk.status == 'SUBMITTED', Chunk.status == 'RUNNING' 
                                    ) ).all()
        if not ( l_chunks ):
            logger.info( 'There are not chunks to stop.' )
        else :
            logger.info('---> Stopping Realization %s' % self.name )
            for chunk in l_chunks :
                chunk.stop( dryrun )
    
class Chunk( Base ):
    """ 
    A class to manage WRF4G chunks
    """
    __tablename__   = 'chunk'

    # Columns
    id              = Column(u'id',INTEGER, primary_key=True, nullable=False)
    rea_id          = Column(u'rea_id',INTEGER, ForeignKey(u'realization.id'))
    sdate           = Column(u'sdate',DATETIME())
    edate           = Column(u'edate',DATETIME())
    wps             = Column(u'wps',INTEGER) 
    status          = Column(u'status',VARCHAR(length=20))
    chunk_id        = Column(u'chunk_id',INTEGER)

    # Relationships
    realization     = relationship("Realization", back_populates= "chunk")
    job             = relationship("Job", back_populates = "chunk", lazy='dynamic')
 
    #METHODS
    def run (self, rerun = False):
        """ 
        Run a chunk is run a drm4g job
        """
        #Send a gridway's job and save data in table Job
        gw_job        = GWJob()
        # create template
        rea_name      = self.realization.name
        exp_name      = self.realization.experiment.name
        exp_path      = join( WRF4G_DIR, 'var', 'submission', exp_name )
        rea_path      = join( exp_path, exp_name )
        wrf4g_package = join( exp_path, "WRF4G.tar.gz" )
        if not exists(  wrf4g_package ) : 
            raise Exception( "'%s' file does not exist" % wrf4g_package )
        # files to add for the inputsandbox 
        inputsandbox  = "file://%s," % wrf4g_package
        inputsandbox += "file://%s/db.conf," % rea_path
        inputsandbox += "file://%s/experiment.wrf4g," % rea_path
        inputsandbox += "file://%s/namelist.input" % rea_path  
        # Add input file if it is exist
        input_files = join( rea_path , 'wrf4g_files.tar.gz' )
        if exists( input_files ) :
            inputsandbox += ",file://%s" % ( input_files )
        # files to add for the outputsandbox
        outputsandbox = "log_%d_${GW_JOB_ID}_${GW_RESTARTED}.tar.gz" % ( self.chunk_id )
        arguments = '%s %s %d %s %s %d' % (
                                        exp_name,
                                        rea_name,                                     
                                        self.chunk_id,
                                        self.sdate,
                                        self.edate,
                                        1 if rerun else 0
                                        )
        gw_job.create_template( name          = rea_name,
                                arguments     = arguments,
                                np            = self.realization.experiment.np,
                                req           = self.realization.experiment.requirements,
                                environ       = self.realization.experiment.environment,
                                inputsandbox  = inputsandbox,
                                outputsandbox = outputsandbox )
        #submit job
        job = Job()  #create an object "job"
        # if the first chunk of the realization
        if self.chunk_id == 1 :
            job.gw_job = gw_job.submit()
        else:
            #if the chunk is not the first of the realization, gwsubmit has an argument, gw_job of the job before
            chunk_before    = self.realization.chunk.filter( Chunk.chunk_id == self.chunk_id - 1 ).one()
            job_before      = chunk_before.job.order_by( Job.id )[-1]
            id_job_before   = job_before.id          
            gw_job_before   = job_before.gw_job
            job.gw_job      = gw_job.submit( dep = gw_job_before )

        job.run( rerun ) #run job
        self.job.append( job )
        # Update reealizaiton status
        self.status = 'SUBMITTED'

    def stop(self, dryrun = False ):
        """
        Delete jobs
        """
        logger.info('\t---> Stopping Chunk %d:\t%s\t%s' % ( self.chunk_id,
                                                       datetime2datewrf(self.sdate),
                                                       datetime2datewrf(self.edate) ) 
                                                       )
        l_jobs = self.job.filter( or_( Job.status != 'PREPARED', Job.status == 'FAILED' ) ).all()
        if not ( l_jobs ):
            logger.info( 'There are not jobs to stop.' )
        else :
            for job in l_jobs :
                job.stop( dryrun )
       
class Job( Base ):
    """
    A class to manage WRF4G jobs
    """
    __tablename__   = 'job'

    # Columns
    id              = Column(u'id',INTEGER, primary_key=True, nullable=False)
    gw_job          = Column(u'gw_job',INTEGER)
    gw_restarted    = Column(u'gw_restarted',INTEGER)  
    chunk_id        = Column(u'chunck_id', INTEGER, ForeignKey(u'chunk.id'))
    resource        = Column(u'resource',VARCHAR(length=45))
    status          = Column(u'status', VARCHAR(length=20))
    exitcode        = Column(u'exitcode',INTEGER)

    # Relationship
    chunk           = relationship("Chunk", back_populates = "job")
    events          = relationship("Events", back_populates = "job", lazy='dynamic')
    
    def set_status(self, status):
        """ 
        Save the status of the job and if it is a jobstatus of CHUNK_STATUS and REA_STATUS,
        change the status of the Chunk and the Realization and add an event. 
        """
        #Save job's status
        self.status = status
        #if is an status of the CHUNK_STATUS and REA_STATUS
        if ( status in CHUNK_STATUS and status in REA_STATUS ) and status != 'SUBMITTED' : 
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
        #Insert values default
        self.resource = '-'
        self.exitcode = '-'
        # Update status
        self.set_status( 'SUBMITTED' ) 
   
    def stop(self, dryrun=False):
        """
        Delete a job
        """
        logger.info('\t\t---> Stopping Job %d' % self.gw_job ) 
        if not dryrun :
            GWJob().kill( self.gw_job )
            self.set_status( 'CANCEL' )

class Events( Base ):

    __tablename__   = 'events'   
 
    # Columns
    id              = Column(u'id',INTEGER, primary_key=True, nullable=False)
    job_id          = Column(u'job_id',INTEGER, ForeignKey(u'job.id'))
    job_status      = Column(u'job_status',INTEGER)
    timestamp       = Column(u'timestamp',DATETIME())

    # Relationship
    job             = relationship("Job", back_populates = "events")