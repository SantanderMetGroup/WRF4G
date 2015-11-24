__version__  = '2.0.0'
__author__   = 'Carlos Blanco'
__revision__ = "$Id$"

import os
import re
import glob
import time
import tarfile
import shutil
import datetime
import logging
import fortran_namelist as fn
from fortran_namelist       import coerce_value_list
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
from wrf4g                  import WRF4G_DIR, WRF4G_DEPLOYMENT_DIR
from wrf4g.config           import ( get_conf, save_exp_pkl, 
                                     load_exp_pkl )
from wrf4g.db               import Base
from wrf4g.utils            import Enumerate, dict_compare 
from wrf4g.utils.archive    import extract
from wrf4g.utils.time       import datetime2datewrf, Calendar
from wrf4g.utils.file       import validate_name, edit_file
from wrf4g.utils.command    import exec_cmd
from wrf4g.utils.vcplib     import VCPURL
from wrf4g.utils.gridwaylib import GWJob

class Experiment( Base ):
    """ 
    Manage WRF4G experiments
    """
    __tablename__         = 'experiment'
    
    # Columns
    id                    = Column('id', INTEGER, primary_key=True, nullable=False)
    name                  = Column('name', VARCHAR(length=512), nullable=False)  
    calendar              = Column('calendar', VARCHAR(length=300))
    home_dir              = Column('home_directory', VARCHAR(length=300))
    np                    = Column('np', INTEGER)
    requirements          = Column('requirements', VARCHAR(length=1024)) 
    environment           = Column('environment', VARCHAR(length=1024)) 
    namelist_version      = Column('namelist_version', VARCHAR(length=1024)) 
    max_dom               = Column('max_dom', INTEGER)
    namelist              = Column('namelist', VARCHAR(length=1024))

    # Realtionships
    realization           = relationship("Realization", back_populates="experiment", lazy='dynamic')

    dryrun                = False
    
    def run(self, rerun = False, rea_pattern = False, rea_status = False, priority = 0 ):
        """
        Run the realizations of this experiment
        n_chunk is the number of chunks to run. 
        If there are not n_chunk, run every chunk since the last one finished 
        """
        #Check if the experiment have some realization to run
        #list of realizations of the experiment
        l_realizations  = self._filter_realizations( rea_pattern, rea_status )
        if not ( l_realizations ):
            logging.warn( 'There are not realizations to run.' )
        else:
            #if there are realizations to run
            for rea in l_realizations :
                logging.info("---> Submitting Realization %s" % rea.name )
                #Run every realization
                rea.dryrun = self.dryrun
                if rerun :
                   first_chunk_run = 1
                else :
                   first_chunk_run = None
                rea.run( first_chunk_run = first_chunk_run, rerun = rerun, priority = priority )

    def edit(self):
        """
        Edit experiment.wrf4g file.
        """
        edit_file( join( self.home_dir, 'experiment.wrf4g' )  )
 
    def _is_reconfigurable(self, modified_variables ) :
        """
        Check if the experiment can be updated
        """
        for elem_modified in modified_variables :
            if elem_modified in ( 'calendar', 'max_dom' ) :
                return False
        return True

    def _is_parcial_reconfigurable(self, modified_variables ) :
        """
        Check if the experiment can be updated without update the database. 
        """
        if modified_variables :
            for elem_modified in modified_variables :
                if not elem_modified in ( 'np', 'requirements', 'environment', 'clean_after_run', 'save_wps',
                                          'parallel_real', 'parallel_wrf', 'parallel_environment', 'domain_path', 
                                          'preprocessor', 'extdata_path', 'postprocessor', 'app', 'output_path' ) :
                    return False
        return True

    def check_db(self, name, start_date, end_date, chunk_size_h):
        """ 
        Check if there is a realization with the same no reconfigurable field. 
        If there is not a realization with the same no reconfigurable fields => error
        If there is a realization with the same reconfigurable fields, update data
        """
        #Check if exists the realization
        try :
            rea = self.realization.filter( Realization.name == name ).one()
        except Exception :
            return None
        else :
            #Check if there is a realization with the same no reconfigurable fields
            if rea.chunk_size_h == chunk_size_h and rea.end_date != end_date :
                logging.debug( '\t\tUpdating realization on the database...' )
                rea.end_date = end_date
                rea.status   = Realization.Status.PREPARED
                return rea
            elif rea.chunk_size_h == chunk_size_h and rea.end_date == end_date :
                return rea
            else :                                 
                #if rea does not exist (no realization with the same no reconfigurable fields)
                raise Exception( "\t\tRealization with the same name and " 
                                 "no reconfigurable fields already exists" ) 

    def prepare(self, update = False, directory = './' ):
        """
        Prepare all realizations and chunks needed to submit a WRF4G experiment.
        """
        if update :
            directory = self.home_dir
        # Read experiment.wrf4g file
        exp_conf = get_conf( directory )

        if self.name != exp_conf.default.name :
            raise Exception( "ERROR: experiment.wrf4g file has a different experiment name." )
        
        # Update experiment variables
        self.name             = exp_conf.default.name
        self.calendar         = exp_conf.default.calendar
        self.home_dir         = exp_conf.default.home_dir
        self.np               = exp_conf.default.np
        self.requirements     = exp_conf.default.requirements
        self.environment      = exp_conf.default.environment 
        self.namelist_version = exp_conf.default.namelist_version
        self.max_dom          = exp_conf.default.max_dom
        self.namelist         = exp_conf.default.namelist_values
        self.datetime_list    = exp_conf.default.datetime_list
 
        # Check if the experiment can be updated
        if update :
            if not exists( join( directory, 'experiment.pkl' ) ) :
                raise Exception( "ERROR: There is not an 'experiment.pkl' file to update this experiment" )
            old_exp_conf = load_exp_pkl( directory )
            added, removed, modified, same = dict_compare( old_exp_conf['default'], exp_conf[ 'default' ] )

            if not self._is_reconfigurable( modified ) :
                raise Exception( "ERROR: Experiment with the same name "
                                 "and no reconfigurable fields already exists." )
        if not self.dryrun :
            exp_sub_dir = join( WRF4G_DIR, 'var', 'submission', self.name )
            if not isdir( exp_sub_dir ) :
                try:
                    logging.debug( "Creating '%s' directory" % exp_sub_dir )
                    os.makedirs( exp_sub_dir )
                except Exception :
                    raise Exception( "Couldn't be created '%s' directory" % exp_sub_dir )
          
        if ( update and not self._is_parcial_reconfigurable( modified ) ) or ( not update ) :
            # Copy the namelist from the template directory 
            logging.info( "Preparing namelist..." )
            namelist_template   = join( WRF4G_DIR , 'etc', 'templates', 'namelist', 
                                       'namelist.input-%s' % self.namelist_version )
            self.namelist_input = join( directory, 'namelist.input' )
            try :
                logging.debug( "Copying '%s' to '%s'" % (namelist_template, self.namelist_input) )
                shutil.copyfile(namelist_template, self.namelist_input )
            except :
                raise Exception( "There is not a namelist template for WRF '%s'"
                                 "(File namelist.input does not exist)" % namelist_template )
            # Cycle to create a realization per combination
            self.cycle_realizations( exp_conf.default.extdata_member_list,
                                     exp_conf.default.namelist_label_comb, 
                                     exp_conf.default.namelist_dict )
        if not self.dryrun :
            # Save current configuration in the experiment.pkl file 
            save_exp_pkl( exp_conf, directory )
            # Copy configure files before submission
            self._copy_experiment_files( exp_sub_dir  )
            # Create software bundles to use on the WN
            self._create_wrf4g_bundles( exp_sub_dir  )
    
    def get_status(self, rea_pattern = False, rea_status = False ):
        """ 
        Show information about realizations of the experiment for example:
        
        Realization Status   Chunks     Comp.Res      Run.Sta     JID ext      %
        testc       Finished    3/3     mycomputer   Finished       0   0 100.00
        
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
        l_realizations = self._filter_realizations( rea_pattern, rea_status )
        #Header of the information
        if not ( l_realizations ):
            raise Exception ( 'There are not realizations to check.' )
        Realization.status_header()
        for rea in l_realizations.all() :
            #Print information of each realization
            rea.get_status( )
    
    def cancel(self, rea_pattern = False, rea_status = False, hard = False ):
        """
        Delete jobs which status is running or submitted 
        """
        #list of realization of the experiment
        l_realizations  = self._filter_realizations( rea_pattern, rea_status )
        if not ( l_realizations ):
            logging.info( 'There are not realizations to cancel.' )
        else :
            logging.info( 'Canceling Experiment %s' % self.name )
            for rea in l_realizations :
                rea.dryrun = self.dryrun
                rea.cancel( hard )

    def set_priority(self, rea_pattern = False, priority = 0 ):
        """
        Setting priority to jobs 
        """
        #list of realization of the experiment
        l_realizations  = self._filter_realizations( rea_pattern, False )
        if not ( l_realizations ):
            logging.info( 'There are not realizations to set priority.' )
        else :
            logging.info( 'Setting priority Experiment %s' % self.name )
            for rea in l_realizations :
                rea.dryrun = self.dryrun
                rea.set_priority( priority )

    def delete(self):
        """
        Delete the experiment, its realizations and chunks 
        """
        # Delete the local submission directory
        local_exp_dir = join( WRF4G_DIR , 'var' , 'submission' , self.name )
        logging.debug( "Deleting '%s' directory" % local_exp_dir )
        if not self.dryrun :
            if exists( local_exp_dir ) : 
                shutil.rmtree( local_exp_dir )

    @staticmethod 
    def create_files(name, template, force, directory):
        """
        Create the files needed to establish a WRF4G experiment
        """
        validate_name( name )
        if not template in [ 'default', 'single', 'physics' ] :
            raise Exception( "'%s' template does not exist" % template )
        exp_dir = expandvars( expanduser( os.getcwd() if directory == './' else directory ) )
        if not exists( exp_dir ):
            raise Exception("'%s' does not exist" % exp_dir )
        exp_dir_config = join( exp_dir, name )
        if exists( exp_dir_config ) and not force :
            raise Exception("'%s' already exists" % exp_dir_config )
        elif exists( exp_dir_config ) and force :
            shutil.rmtree( exp_dir_config )
        logging.debug( "Creating '%s' directory" % exp_dir_config )
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

    def cycle_realizations( self, extdata_member, combinations, namelist_combinations ):
        """
        Create realizations for each member and namelist combinations.
        """
        for member in extdata_member :
            rea_name_member = "%s_%s" % ( self.name, member.split( '|' )[ 0 ] ) if member else self.name
            for comb, physic_label in enumerate( combinations ) :
                rea_name_member_physic = "%s_%s" % ( rea_name_member, physic_label ) if physic_label else rea_name_member
                ##
                # Update namelist values
                ##
                nmli = fn.WrfNamelist( self.namelist_input )
                logging.debug( "Updating parameter 'max_dom' in the namelist" )
                nmli.setValue( "max_dom", self.max_dom )
                for mnl_variable, mnl_values in list(namelist_combinations.items()) :
                    # Update the namelist per each combination
                    logging.debug( "Updating parameter '%s' in the namelist" % mnl_variable )
                    # Modify the namelist with the parameters available in the namelist description
                    if '.' in mnl_variable :
                        section, val = mnl_variable.split( '.' )
                    else :
                        section, val = "",  mnl_variable
                    if val.startswith( "max_dom:" ) :
                        val = val[ 8: ]
                        if not val in nmli.MAX_DOM_VARIABLES : 
                            nmli.MAX_DOM_VARIABLES.extend( val  )
                    elif val.startswith( "single:" ) :
                        val = val[ 7: ]
                        if val in nmli.MAX_DOM_VARIABLES : 
                            nmli.MAX_DOM_VARIABLES.remove( val  )
                    try :                        
                        nmli.setValue( val, coerce_value_list( mnl_values[ comb ] ), section )
                    except IndexError:
                        raise Exception( "'%s' does not have values for all namelist combinations." % mnl_variable )
                nmli.trimMaxDom()
                nmli.extendMaxDomVariables()
                if nmli.wrfCheck() :
                    raise Exception( "Please review 'namelist_values' variable." ) 
                ##
                for start_date, end_date, simult_interval_h, simult_length_h, chunk_size_h, restart_interval in self.datetime_list :
                    # Update restart_interval in the namelist 
                    logging.debug( "Updating parameter 'restart_interval' in the namelist" )
                    nmli.setValue( "restart_interval", restart_interval )
                    if not self.dryrun :
                        nmli.overWriteNamelist()
                    # Define which calendar is going to be used
                    exp_calendar   = Calendar( self.calendar )
                    rea_start_date = start_date
                    while rea_start_date < end_date :
                        rea_end_date = exp_calendar.add_hours(rea_start_date, simult_length_h )
                        if rea_end_date > end_date :
                            rea_end_date = end_date
                        rea_name = "%s_%s" % ( rea_name_member_physic, rea_start_date.strftime( "%Y%m%dT%H%M%S" ) )
                        logging.info( "---> Realization %s: start date %s end date %s" % ( 
                                       rea_name, rea_start_date, rea_end_date ) )
                        # Check realization on the database
                        rea = self.check_db( name = rea_name, start_date = rea_start_date,
                                             end_date = rea_end_date, chunk_size_h = chunk_size_h ) 
                        if not rea :
                            # Create a realization 
                            rea = Realization( name          = rea_name, 
                                               start_date    = rea_start_date,
                                               end_date      = rea_end_date,
                                               chunk_size_h  = chunk_size_h,
                                               current_date  = rea_start_date,
                                               status        = Realization.Status.PREPARED,
                                               current_chunk = 1,
                                               member_label  = member.split( '|' )[ 0 ],
                                               physic_label  = physic_label )
                            # Add realization to the experiment 
                            self.realization.append( rea )
                        # Check storage
                        if not self.dryrun :   
                            rea._prepare_sub_files()
                        rea.cycle_chunks()
                        rea_start_date = exp_calendar.add_hours( rea_start_date, simult_interval_h ) 

    def _copy_experiment_files(self, exp_sub_dir ):
        """
        Copy configure files before submission.
        """    
        for file in [ join( WRF4G_DIR, "etc", "db.conf" ),
                      join( self.home_dir, "experiment.wrf4g" ),
                      join( self.home_dir, "experiment.pkl" ) ] :
            if not exists ( expandvars( file ) ) :
                raise Exception( "'%s' is not available" % file )
            else :
                shutil.copy( expandvars( file ) , exp_sub_dir )

    def _create_wrf4g_bundles(self, exp_sub_dir ):
        """
        Create bundles with the necessary software to run WRF on worker nodes.
        """
        # WRF4G bundle
        logging.debug( "Create a WRF4G software bundle to use on the worker node..." )
        wrf4g_package = join ( exp_sub_dir , "WRF4G.tar.gz" )
        if exists( wrf4g_package  ):
            logging.debug( "Removing '%s' package" % wrf4g_package ) 
            os.remove( wrf4g_package )
        current_path = os.getcwd()
        try :
            tar = tarfile.open( wrf4g_package , "w:gz" )
            os.chdir( WRF4G_DEPLOYMENT_DIR )
            logging.debug( "Creating '%s' package" % wrf4g_package )
            [ tar.add( dir ) for dir in [ "bin", "lib" ] ]
        finally :
            tar.close()
        # wrf4g_files bundle
        wrf4g_files_dir = join( self.home_dir, 'wrf4g_files' )
        if isdir( wrf4g_files_dir ):
            logging.debug( "Create a wrf4g_files.tar.gz bundle to use on the worker node..." )
            wrf4g_files_package = join ( exp_sub_dir , "wrf4g_files.tar.gz" )
            if exists( wrf4g_files_package ):
                logging.debug( "Removing '%s' package" % wrf4g_files_package )
                os.remove( wrf4g_files_package )
            tar = tarfile.open( wrf4g_files_package , "w:gz" )
            os.chdir( wrf4g_files_dir )
            for elem in os.listdir('.') :
                tar.add( elem )
            tar.close()
        os.chdir( current_path )

    def _filter_realizations(self, pattern, status ):
        """
        Filter realizations from the experiment
        """
        l_realizations = self.realization
        if pattern :
            l_realizations = l_realizations.\
                             filter( Realization.name.like( pattern.replace('*','%') ) ) 
        if status :
            l_realizations = l_realizations.\
                             filter( Realization.status == status )
        return l_realizations
    
class Realization( Base ):
    """
    A class to mange WRF4G realizations
    """
    __tablename__   = 'realization'
    
    # Columns
    id              = Column('id',INTEGER, primary_key=True, nullable=False)
    exp_id          = Column('exp_id',INTEGER, ForeignKey('experiment.id')) 
    name            = Column('name',VARCHAR(length=1024),nullable=False)
    start_date      = Column('start_date',DATETIME())
    end_date        = Column('end_date',DATETIME())
    chunk_size_h    = Column('chunk_size_h', INTEGER)
    restart         = Column('restart',DATETIME()) 
    status          = Column('status',VARCHAR(length=20))
    current_date    = Column('current_date',DATETIME())
    current_chunk   = Column('current_chunk',INTEGER)
    nchunks         = Column('nchunks',INTEGER)
    member_label    = Column('member_label',VARCHAR(length=100))
    physic_label    = Column('physic_label',VARCHAR(length=100)) 

    # Realtionships
    experiment      = relationship("Experiment", back_populates="realization")
    chunk           = relationship("Chunk", back_populates= "realization", lazy='dynamic')

    dryrun          = False

    Status          = Enumerate( 'PREPARED', 'SUBMITTED', 'RUNNING',
                                 'PENDING', 'FAILED', 'FINISHED' )
        
    def run(self, first_chunk_run = None , last_chunk_run = None, rerun = False, priority = 0 ):
        """ 
        Run n_chunk of the realization.
        If n_chunk=0 run every chunk of the realization which haven't finished yet
        else run (n_chunk) chunks since the last one finished
        """
        first_chunk_run = int( first_chunk_run ) if first_chunk_run else None 
        last_chunk_run  = int( last_chunk_run  ) if last_chunk_run  else None
        #Check the status of the realization
        if self.status == Realization.Status.FINISHED and not rerun :
            logging.warn( "\tRealization '%s' already finished." % self.name )
        elif ( self.status == Realization.Status.SUBMITTED or 
               self.status == Realization.Status.RUNNING ) and not rerun :
            logging.warn( "\tRealization '%s' has been submitted." % self.name )
        elif first_chunk_run and first_chunk_run < 0 :
            logging.error( "\tERROR: The first chunk to run is '%d'." % first_chunk_run ) 
        elif last_chunk_run and last_chunk_run  < 0 :
            logging.error( "\tERROR: The last chunk to run is '%d'." % last_chunk_run )
        elif ( last_chunk_run and first_chunk_run ) and last_chunk_run < first_chunk_run :
            logging.error( "\tERROR: The last chunk to run is greater than the fist one." )
        else :
            # search first chunk to run
            if rerun and first_chunk_run :
                ch                 = self.chunk.filter( Chunk.chunk_id == first_chunk_run ).one()
                self.restart       = ch.start_date
                self.current_date  = ch.start_date
                self.current_chunk = first_chunk_run
            elif rerun and not first_chunk_run :
                self.restart       = None
                first_chunk_run    = self.current_chunk = 1
            else :
                #search first chunk to run
                if not self.restart : # run every chunks of the realization
                    first_chunk_run = 1
                else:
                    #search chunk with end_date>restart and start_date<restart
                    try:
                        first_chunk  = self.chunk.filter( and_( Chunk.start_date <= self.restart, 
                                                                Chunk.end_date   >= self.restart ) 
                                                         ).all()[ -1 ]
                    except : 
                        raise Exception( 'There are not chunks to run.' )
                    else:
                        if ( first_chunk_run 
                             and first_chunk.chunk_id != first_chunk_run ):
                            raise Exception( 'Use the option --rerun.' )
                        else : 
                            first_chunk_run = self.current_chunk  = first_chunk.chunk_id
            #search last chunk to run
            if not last_chunk_run :
                #run every chunk
                #Search last chunk of the realization
                last_chunk_run = self.nchunks
            else:
                #search last chunk
                last_chunk_run = last_chunk_run
            #Search chunks to run
            l_chunks = self.chunk.filter( and_( Chunk.chunk_id >= first_chunk_run, 
                                                Chunk.chunk_id <= last_chunk_run )
                                        ).all()
            #run chunks
            for index, chunk in enumerate( l_chunks ) :
                #print data of chunks
                logging.info('\t---> Submitting Chunk %d %s %s' % ( chunk.chunk_id, 
                                                              datetime2datewrf(chunk.start_date), 
                                                              datetime2datewrf(chunk.end_date) ) )
                if not self.dryrun :
                    chunk.run( index, rerun, priority )
            if not self.dryrun :
                # Update reealizaiton status
                self.status = Realization.Status.SUBMITTED
            
    def _prepare_sub_files(self):
        """
        Prepare the files needed to submit the realization. 
        """
        rea_submission_dir = join( WRF4G_DIR, 'var', 'submission', self.experiment.name, self.name )
        if not isdir( rea_submission_dir ) :
            try :
                os.makedirs( rea_submission_dir )
            except Exception :
                raise Exception( "Couldn't be created '%s' directory" % rea_submission_dir )
        file_name = expandvars( join( self.experiment.home_dir, "namelist.input" ) )
        if not exists ( file_name ) :
            raise Exception( "'%s' is not available" % file_name )
        else :
            shutil.copy( file_name , rea_submission_dir )

    def check_db(self, rea_id, chunk_start_date, chunk_end_date, chunk_id ):
        """ 
        Check if there is a chunk with the same no reconfigurable field. 
        If there is not a chunk with the same no reconfigurable fields => error,.
        If there is a chunk with the same reconfigurable fields, update data.
        """
        #Check if there is a chunk with the same fields
        try:
            ch = self.chunk.filter( Chunk.rea_id     == rea_id,
                                    Chunk.chunk_id   == chunk_id,
                                    Chunk.start_date == chunk_start_date,
                                    Chunk.end_date   == chunk_end_date,
                                   ).one()
        except Exception :
            #There will be an exception for the last chunk of a realization.
            #This chunk will be able to modify 
            if self.nchunks == chunk_id :
                try :
                    ch2 = self.chunk.filter( Chunk.rea_id     == rea_id,
                                             Chunk.chunk_id   == chunk_id,
                                             Chunk.start_date == chunk_start_date,
                                             Chunk.end_date   != chunk_end_date,
                                           ).one()
                except Exception :
                    return None
                else :
                    logging.debug( '\t\tUpdating chunk on the database...' )
                    ch2.end_date = chunk_end_date
                    return ch2
            else :
                return None
        else :
            #if ch exists 
            return ch

    def cycle_chunks(self):
        """
        Create chunks the needed for a realization 
        """
        # Define which calendar is going to be used
        exp_calendar = Calendar(self.experiment.calendar)
        chunk_id = 1
        chunk_start_date = self.start_date
        while chunk_start_date < self.end_date :
            chunk_end_date = exp_calendar.add_hours( chunk_start_date, 
                                                     hours = self.chunk_size_h )
            if chunk_end_date > self.end_date :
                chunk_end_date = self.end_date
            # Check chunk on the database
            ch = self.check_db( rea_id           = self.id, 
                                chunk_start_date = chunk_start_date, 
                                chunk_end_date   = chunk_end_date,
                                chunk_id         = chunk_id
                                )
            if not ch :
                logging.info( "\t\t---> Chunk %d %s %s" %( chunk_id,
                                                       datetime2datewrf(chunk_start_date),
                                                       datetime2datewrf(chunk_end_date) ) )
                # Create Chunk
                ch = Chunk( rea_id     = self.id, 
                            start_date = chunk_start_date, 
                            end_date   = chunk_end_date,
                            wps        = 0, 
                            chunk_id   = chunk_id,
                            status     = Chunk.Status.PREPARED
                          )
                # Add realization to the experiment 
                self.chunk.append( ch )
            chunk_start_date = chunk_end_date 
            chunk_id    = chunk_id + 1
        # Set the number of chunks of a relaization    
        self.nchunks = chunk_id - 1

    @staticmethod 
    def status_header(): 
        logging.info( '\033[1;4m%-60s %-10s %-10s %-16s %-10s %6s %-3s %6s\033[0m'% (
                        'REALIZATION','STATUS','CHUNKS','RESOURCE','RUN STATUS',
                        'JID', 'EXT','%' ) )
 
    def get_status(self):
        """ 
        Show information about the realization for example:
            Realization Status    Chunks     Comp.Res       Run.Sta     JID   ext      %
            testc       Finished     3/3   mycomputer      Finished       0     0 100.00
            
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
        resource, exitcode, gw_job = '-', '-' , '-'
        if self.status == Realization.Status.PREPARED :
            status             = Realization.Status.PREPARED
            chunk_distribution = '%d/%d' % ( 0 if not self.current_chunk else self.current_chunk, self.nchunks )
        else :
            current_chunk      = self.chunk.filter( Chunk.chunk_id == self.current_chunk ).one()
            l_jobs             = current_chunk.job.order_by( Job.id ).all()
            if len( l_jobs)  == 0 :
                status         = Realization.Status.PREPARED
            else :
                last_job       = l_jobs[ -1 ]
                resource       = last_job.resource
                exitcode       = last_job.exitcode
                status         = last_job.status
                gw_job         = str( last_job.gw_job )
            chunk_distribution = '%d/%d' % ( self.current_chunk, self.nchunks )
        #Format chunks run / chunks total
        runt   = int( self.current_date.strftime("%s") ) - int( self.start_date.strftime("%s") ) 
        totalt = int( self.end_date.strftime("%s") )     - int( self.start_date.strftime("%s") )
        #Percentage
        per = runt * 100.0 / totalt
        #Print output
        logging.info( "%-60s %-10.10s %-10.10s %-16.16s %-10.10s %6.6s %-3.3s %6.2f" % (
                    self.name, self.status, chunk_distribution, resource, status, 
                    gw_job, exitcode, per ) )
  
    def get_log( self, chunk_id , directory ) :
        """
        Search and unpack log files. 
        """
        tar_path      = join( WRF4G_DIR, 'var', 'submission', self.experiment.name, self.name )
        all_tar_files = glob.glob( join( tar_path, 'log_%s_*.tar.gz' % chunk_id ) ) 
        if not all_tar_files :
            raise Exception( 'There is not a log available for this chunk.' )
        for tar_file in all_tar_files :
            logging.info( "Unpacking %s file in the %s directory" % ( tar_file, directory ) )
            extract(tar_file, expandvars( expanduser( directory ) ) )

    def cancel(self, hard = False ):
        """
        Delete chunks which status is running or submitted 
        """
        l_chunks = self.chunk.filter( or_( Chunk.status == Chunk.Status.SUBMITTED,
                                           Chunk.status == Chunk.Status.PENDING,
                                           Chunk.status == Chunk.Status.FAILED,
                                           Chunk.status == Chunk.Status.RUNNING ) 
                                    ).all()
        logging.info('---> Canceling Realization %s' % self.name )
        if not ( l_chunks ):
            logging.info( '\tThere are not chunks to cancel.' )
        else :
            for chunk in l_chunks :
                chunk.dryrun = self.dryrun
                chunk.cancel( hard )
   
    def set_priority(self, priority = 0 ):
        """
        Setting priority to chunks which status is submitted.
        """
        if priority < 0 or priority > 20 :
            raise Exception( "'%d' priority is out of the range [0, 20]" % priority )      
        l_chunks = self.chunk.filter( Chunk.status == Chunk.Status.SUBMITTED ).all()
        logging.info( '---> Setting priority Realization %s' % self.name )
        if not ( l_chunks ):
            logging.info( '\tThere are not chunks to set priority.' )
        else :
            for chunk in l_chunks :
                chunk.dryrun = self.dryrun
                chunk.set_priority( priority )
 
class Chunk( Base ):
    """ 
    A class to manage WRF4G chunks
    """
    __tablename__   = 'chunk'

    # Columns
    id              = Column('id', INTEGER, primary_key = True, nullable = False)
    rea_id          = Column('rea_id', INTEGER, ForeignKey('realization.id'))
    start_date      = Column('start_date', DATETIME())
    end_date        = Column('end_date', DATETIME())
    wps             = Column('wps', INTEGER) 
    status          = Column('status', VARCHAR(length=20))
    chunk_id        = Column('chunk_id', INTEGER)

    # Relationships
    realization     = relationship("Realization", back_populates = "chunk")
    job             = relationship("Job", back_populates = "chunk", lazy = "dynamic")

    dryrun          = False 

    Status          = Enumerate( 'PREPARED', 'SUBMITTED', 'RUNNING',
                                 'PENDING', 'FAILED', 'FINISHED' )
 
    #METHODS
    def run (self, index, rerun = False, priority = 0 ):
        """ 
        Run a chunk is run a drm4g job
        """
        #Send a gridway's job and save data in table Job
        gw_job        = GWJob()
        # create template
        rea_name      = self.realization.name
        exp_name      = self.realization.experiment.name
        exp_path      = join( WRF4G_DIR, 'var', 'submission', exp_name )
        rea_path      = join( exp_path, rea_name )
        wrf4g_package = join( exp_path, "WRF4G.tar.gz" )
        if not exists(  wrf4g_package ) : 
            raise Exception( "'%s' file does not exist" % wrf4g_package )
        # files to add for the inputsandbox 
        inputsandbox  = "file://%s,"                  % wrf4g_package
        inputsandbox += "file://%s/db.conf,"          % exp_path
        inputsandbox += "file://%s/experiment.wrf4g," % exp_path
        inputsandbox += "file://%s/experiment.pkl,"   % exp_path
        inputsandbox += "file://%s/namelist.input"    % rea_path  
        # Add input file if it is exist
        input_files = join( exp_path , 'wrf4g_files.tar.gz' )
        if exists( input_files ) :
            inputsandbox += ",file://%s" % ( input_files )
        # files to add for the outputsandbox
        outputsandbox = "log_%d_${JOB_ID}.tar.gz" % self.chunk_id
        arguments = '%s %s %d %s %s %s %d %s' % ( exp_name,
                                                  rea_name,                                     
                                                  self.chunk_id,
                                                  datetime2datewrf( self.start_date ),
                                                  datetime2datewrf( self.end_date ),
                                                  datetime2datewrf( self.realization.start_date ),
                                                  1 if rerun else 0,
                                                  self.realization.member_label )
        # Create the job template
        file_template = gw_job.create_template( name          = rea_name,
                                                directory     = rea_path,
                                                arguments     = arguments,
                                                np            = self.realization.experiment.np,
                                                req           = self.realization.experiment.requirements,
                                                environ       = self.realization.experiment.environment,
                                                inputsandbox  = inputsandbox,
                                                outputsandbox = outputsandbox )
        # Submit the template
        job = Job()  #create an object "job"
        time.sleep( 0.1 )
        # if the first chunk of the realization
        if index == 0 :
            job.gw_job    = gw_job.submit( priority = priority, file_template = file_template )
        else:
            # if the chunk is not the first of the realization, 
            # gwsubmit has an argument, gw_job of the job before
            chunk_before  = self.realization.chunk.\
                            filter( Chunk.chunk_id == self.chunk_id - 1 ).one()
            job_before    = chunk_before.job.order_by( Job.id )[-1]
            id_job_before = job_before.id          
            gw_job_before = job_before.gw_job
            job.gw_job    = gw_job.submit( dep = gw_job_before, priority = priority,
                                           file_template = file_template )
        job.chunk_id = self.chunk_id
        job.run( rerun ) 
        self.job.append( job )
        # Update realizaiton status
        self.status = Chunk.Status.SUBMITTED

    def cancel(self, hard = False ):
        """
        Delete jobs
        """
        logging.info('\t---> Canceling Chunk %d %s %s' % ( self.chunk_id,
                                                           datetime2datewrf(self.start_date),
                                                           datetime2datewrf(self.end_date) ) 
                                                          )
        l_jobs = self.job.filter( and_( Job.status != Job.Status.PREPARED, 
                                        Job.status != Job.Status.FINISHED,
                                        Job.status != Job.Status.FAILED,
                                        Job.status != Job.Status.CANCEL ) 
                                ).all()
        if not ( l_jobs ):
            logging.info( '\t\tThere are not jobs to cancel.' )
        else :
            for job in l_jobs :
                job.dryrun = self.dryrun
                job.cancel( hard )

    def set_priority(self, priority = 0 ):
        """
        Setting priority to jobs
        """
        logging.info('\t---> Setting priority Chunk %d %s %s' % ( self.chunk_id,
                                                                  datetime2datewrf(self.start_date),
                                                                  datetime2datewrf(self.end_date) )
                                                          )
        l_jobs = self.job.filter( Job.status == Job.Status.SUBMITTED ).all()
        if not ( l_jobs ):
            logging.info( '\t\tThere are not jobs to set priority.' )
        else :
            for job in l_jobs :
                job.dryrun = self.dryrun
                job.set_priority( priority )

class JobCodeError():
    LOG_PATH             = 1
    COPY_APP             = 2
    APP_ERROR            = 3
    SOURCE_SCRIPT        = 4
    LOCAL_PATH           = 5
    COPY_NODES           = 6
    JOB_SHOULD_NOT_RUN   = 7
    COPY_RST_FILE        = 8
    RESTART_MISMATCH     = 9
    COPY_NAMELIST_WPS    = 10
    COPY_REAL_FILE       = 11
    COPY_BOUND           = 12
    NAMELIST_FAILED      = 13
    PREPROCESSOR_FAILED  = 14
    LINK_GRIB_FAILED     = 15
    UNGRIB_FAILED        = 16
    METGRID_FAILED       = 17
    REAL_FAILED          = 18
    COPY_UPLOAD_WPS      = 19
    WRF_FAILED           = 20
    POSTPROCESSOR_FAILED = 21
    COPY_OUTPUT_FILE     = 22
   
class Job( Base ):
    """
    A class to manage WRF4G jobs
    """
    __tablename__   = 'job'

    # Columns
    id              = Column('id', INTEGER, primary_key=True, nullable=False)
    gw_job          = Column('gw_job', INTEGER)
    gw_restarted    = Column('gw_restarted', INTEGER)  
    chunk_id        = Column('chunck_id', INTEGER, ForeignKey('chunk.id'))
    resource        = Column('resource', VARCHAR(length=45))
    status          = Column('status', VARCHAR(length=20))
    exitcode        = Column('exitcode', VARCHAR(length=20))

    # Relationship
    chunk           = relationship("Chunk", back_populates = "job")
    events          = relationship("Events", back_populates = "job", lazy = 'dynamic')

    dryrun          = False

    Status          = Enumerate( 'UNKNOWN', 'PREPARED', 'SUBMITTED', 'RUNNING', 'PENDING', 
                                 'CANCEL', 'FAILED', 'FINISHED', 'CREATE_OUTPUT_PATH', 
                                 'CONF_APP', 'DOWN_RESTART', 'DOWN_WPS', 'DOWN_BOUND', 'UNGRIB', 
                                 'METGRID', 'REAL', 'UPLOAD_WPS', 'ICBCPROCESOR', 'WRF' )

    CodeError       = JobCodeError()

    def set_status(self, status):
        """ 
        Save the status of the job and if it is a jobstatus of CHUNK STATUS and REALIZATION STATUS,
        change the status of the Chunk and the Realization and add an event. 
        """
        #Save job's status
        self.status = status
        if status == Job.Status.CANCEL :
            self.chunk.status = Chunk.Status.PREPARED
            self.chunk.realization.status = Realization.Status.PREPARED
        #if it is an status of the CHUNK STATUS 
        elif status in Chunk.Status and status != Chunk.Status.SUBMITTED :
            self.chunk.status = status
            #if it is an status of the REALIZATION STATUS 
            if status in Realization.Status and status != Realization.Status.SUBMITTED :
                if ( status == Job.Status.FINISHED and \
                     self.chunk.chunk_id == self.chunk.realization.nchunks ) :
                    self.chunk.realization.status = Realization.Status.FINISHED
                elif ( status == Job.Status.FINISHED and \
                       self.chunk.realization.status != Realization.Status.FINISHED ) :
                    self.chunk.realization.current_chunk = self.chunk.chunk_id + 1
                elif status != Job.Status.FINISHED :
                    self.chunk.realization.status = status
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
        self.set_status( Job.Status.SUBMITTED ) 
   
    def cancel(self, hard = False ):
        """
        Delete a job
        """
        logging.info('\t\t---> Canceling Job %d' % self.gw_job ) 
        if not self.dryrun :
            GWJob().kill( self.gw_job, hard )
            self.set_status( Job.Status.CANCEL )

    def set_priority(self, priority = 0 ):
        """
        Delete a job
        """
        logging.info('\t\t---> Setting priority Job %d' % self.gw_job )
        if not self.dryrun :
            GWJob().set_priority( self.gw_job, priority )

class Events( Base ):
    """
    A class to keep track of workflow events
    """
    __tablename__   = 'events'   
 
    # Columns
    id              = Column('id',INTEGER, primary_key=True, nullable=False)
    job_id          = Column('job_id',INTEGER, ForeignKey('job.id'))
    job_status      = Column('job_status',VARCHAR(length=20))
    timestamp       = Column('timestamp',DATETIME())

    # Relationship
    job             = relationship("Job", back_populates = "events")
