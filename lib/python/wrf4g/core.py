__version__  = '2.0.0'
__author__   = 'Carlos Blanco'
__revision__ = "$Id$"

import os
import re
import tarfile
import shutil
import datetime
import logging
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
from wrf4g.config           import get_conf, save_exp_pkl, load_exp_pkl, dict_compare
from wrf4g.db               import Base
from wrf4g.utils.time       import datetime2datewrf, datetime2dateiso, Calendar
from wrf4g.utils.file       import validate_name, edit_file
from wrf4g.utils.command    import exec_cmd_subprocess as exec_cmd
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
    __tablename__         = 'experiment'
    
    # Columns
    id                    = Column(u'id', INTEGER, primary_key=True, nullable=False)
    name                  = Column(u'name', VARCHAR(length=512), nullable=False)  
    calendar              = Column(u'calendar', VARCHAR(length=300))
    home_dir              = Column(u'home_directory', VARCHAR(length=300))
    np                    = Column(u'np', INTEGER)
    requirements          = Column(u'requirements', VARCHAR(length=1024)) 
    environment           = Column(u'environment', VARCHAR(length=1024)) 
    namelist_version      = Column(u'namelist_version', VARCHAR(length=1024)) 
    max_dom               = Column(u'max_dom', INTEGER)
    namelist              = Column(u'namelist', VARCHAR(length=1024))

    # Realtionships
    realization           = relationship("Realization", back_populates="experiment", lazy='dynamic')

    dryrun                = False
    
    def run(self, rerun = False ):
        """
        Run the realizations of this experiment
        n_chunk is the number of chunks to run. 
        If there are not n_chunk, run every chunk since the last one finished 
        """
        #Check if the experiment have some realization to run
        #list of realizations of the experiment
        l_realizations = self.realization.all()
        if not ( l_realizations ):
            logging.warning( 'There are not realizations to run.' )
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
                rea.run( first_chunk_run = first_chunk_run, rerun = rerun )

    def edit(self):
        """
        Edit experiment.wrf4g file.
        """
        edit_file( join( self.home_dir, 'experiment.wrf4g' )  )

    def check_db(self, name, start_date, end_date, label ):
        """ 
        Check if there is a realization with the same no reconfigurable field. 
        If there is not a realization with the same no reconfigurable fields => error
        If there is a realization with the same reconfigurable fields, update data
        """
        #Check if there is a realization with the same no reconfigurable fields:
        #rea_name, start_date, label
        try:
            rea = self.realization.filter( Realization.name       == name,
                                           Realization.start_date == start_date,
                                           Realization.label      == label
                                         ).one()
        except Exception :
            #if rea does not exist (no realization with the same no reconfigurable fields)
            raise Exception("ERROR: Realization with the same name and no reconfigurable fields already exists") 
        else: 
            #if rea exit,it means a realization with the same no reconfigurable fields
            logging.debug('\tUpdating realization on the database...')
            rea.end_date = end_date
            return rea

    def _create_wrf4g_bundle(self):
        """
        Create a bundle with the necessary software to run WRF on WNs.
        """
        logging.debug( "Create a WRF4G software bundle to use on the WN..." )
        exp_dir = join( WRF4G_DIR , 'var' , 'submission' , self.name )
        if not isdir( exp_dir ) :
            try:
                logging.debug( "Creating '%s' directory" % exp_dir ) 
                os.makedirs( exp_dir )
            except Exception :
                raise Exception( "Couldn't be created '%s' directory" % exp_dir )
        wrf4g_package = join ( exp_dir , "WRF4G.tar.gz" )
        if exists( wrf4g_package  ):
            logging.debug( "Removing '%s' package" % wrf4g_package ) 
            os.remove( wrf4g_package )
        current_path = os.getcwd()
        tar = tarfile.open( wrf4g_package , "w:gz" )
        os.chdir( WRF4G_DEPLOYMENT_DIR )
        logging.debug( "Creating '%s' package" % wrf4g_package )
        for dir in [ "bin", "lib" ]:
            tar.add( dir )
        tar.close()
        os.chdir( current_path )

    def create(self, update = False, directory = './' ):
        """
        Create and prepare all realizations and chunks needed to submit a WRF4G experiment.
        """
        exp_conf = get_conf( directory )
        if update :
            old_exp_conf = load_exp_pkl( directory )
            added, removed, modified, same = dict_compare( old_exp_conf['default'] , exp_conf[ 'default' ] )
            for elem_modified in modified :
                if elem_modified in ( 'calendar' , 'max_dom', 'namelist_version', 'namelist' ) :    
                    raise Exception( "ERROR: Experiment with the same name "
                                     "and no reconfigurable fields already exists." )
        elif self.name != exp_conf.default.name :
            raise Exception( "ERROR: experiment.wrf4g file has a different experiment name." )     
        save_exp_pkl( exp_conf, directory )
        # Update experiment variables
        self.name                  = exp_conf.default.name
        self.calendar              = exp_conf.default.calendar
        self.home_dir              = exp_conf.default.home_dir
        self.np                    = exp_conf.default.np
        self.requirements          = exp_conf.default.requirements
        self.environment           = exp_conf.default.environment 
        self.namelist_version      = exp_conf.default.namelist_version
        self.max_dom               = exp_conf.default.max_dom
        self.namelist              = exp_conf.default.namelist
        self.datetime_list         = exp_conf.default.datetime_list
        if not self.dryrun :
            # Create a WRF4G software bundle to use on the WN
            self._create_wrf4g_bundle()
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
                             "(File namelist.input does not exist)" % self.namelist_template )
        if not self.dryrun :  
            # Update max_dom on the namelist  
            logging.debug( "Updating parameter 'max_dom' in the namelist" )
            exec_cmd( "fortnml -wof %s -s max_dom %d" % ( self.namelist_input, self.max_dom ) )          
            # Trim the namlist
            logging.debug( "Force trimming the arrays in the namelist to 'max_dom'" ) 
            exec_cmd( "fortnml -wof %s --force-trim=%d" % ( self.namelist_input, self.max_dom ) )
        # Cycle to create a realization per combination
        self.cycle_combinations( exp_conf.default.label_combination, exp_conf.default.namelist_dict, update )

    def cycle_combinations( self, combinations, namelist_combinations, update ):
        """
        Create realizations for each combination namelist.
        """
        for comb, label in enumerate( combinations ) :
            if label :
                logging.info( "---> Multicombination run %s" % (label )  )
            else :
                logging.info( "---> Single combination run" )
            for mnl_variable, mnl_values in namelist_combinations.items() :
                # Update the namelist per each combination
                if not self.dryrun :
                    logging.debug( "Updating parameter '%s' in the namelist" % mnl_variable )
                    # Modify the namelist with the parameters available in the namelist description
                    if '.' in mnl_variable :
                       section, val = mnl_variable.split( '.' )
                       cmd = "fortnml -wof %s -s %s@%s %s" % ( self.namelist_input, val, section, str( mnl_values[ comb ] ) )
                    else :
                       cmd = "fortnml -wof %s -s %s %s"    % ( self.namelist_input, mnl_variable, str( mnl_values[ comb ] ) ) 
                    exec_cmd( cmd )
            self.cycle_time( "%s__%s" % ( self.name, label ) if label else self.name, label, update )
     
    def cycle_time(self, rea_name, label, update) :
        """
        Create the realizations for each datetime definition.
        """
        for l_elem in self.datetime_list :
            start_date, end_date, simult_interval_h, simult_length_h, chunk_size_h, restart_interval = l_elem
            # Define which calendar is going to be used
            exp_calendar    = Calendar( self.calendar )
            rea_start_date  = start_date
            while rea_start_date < end_date :
                rea_end_date = exp_calendar.add_hours(rea_start_date, simult_length_h )
                cycle_name   = "%s__%s_%s" % ( rea_name, datetime2dateiso( rea_start_date ),
                                               datetime2dateiso( rea_end_date ) )
                logging.info( "\t---> Realization %s" % cycle_name  )
                if update :
                    # Check realization on the database
                    rea = self.check_db( name          = cycle_name, 
                                         start_date    = rea_start_date,
                                         end_date      = rea_end_date,
                                         label         = label ) 
                else :
                    # Create a realization 
                    rea = Realization( name          = cycle_name, 
                                       start_date    = rea_start_date,
                                       end_date      = rea_end_date,
                                       chunk_size_h  = chunk_size_h,
                                       current_date  = rea_start_date,
                                       status        = 'PREPARED',
                                       current_chunk = 1,
                                       label         = label )
                    # Add realization to the experiment 
                    self.realization.append( rea )
                # Check storage
                if not self.dryrun :
                    # Update restart_interval on the namelist to create chunks
                    exec_cmd( "fortnml -wof %s -s restart_interval %d" % ( self.namelist_input, restart_interval ) )
                    rea._prepare_sub_files()
                rea.cycle_chunks( update )
                rea_start_date = exp_calendar.add_hours(rea_start_date, simult_interval_h ) 
    
    def get_status(self, rea_pattern = False ):
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
        if rea_pattern :
            l_realizations = self.realization.\
                             filter( Realization.name.like( rea_pattern.replace('*','%') ) ) 
        else :
            l_realizations = self.realization.all()

        #Header of the information
        if not ( l_realizations ):
            raise Exception ( 'There are not realizations to check.' )
        logging.info( '%-60s %-10s %-10s %-16s %-10s %6s %-3s %6s'% (
                        'Realization','Status','Chunks','Comp.Res','Run.Sta',
                        'JID', 'Ext','%' ) )
        for rea in l_realizations :
            #Print information of each realization
            rea.get_status( )
    
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

    def stop(self):
        """
        Delete jobs which status is running or submitted 
        """
        #list of realization of the experiment
        l_realizations = self.realization.all()
        if not ( l_realizations ):
            logging.info( 'There are not realizations to stop.' )
        else :
            logging.info( 'Stopping Experiment %s' % self.name )
            for rea in l_realizations :
                rea.dryrun = self.dryrun
                rea.stop( )

    def delete(self):
        """
        Delete the experiment, its realizations and chunks 
        """
        if not self.dryrun :
            # Delete the local submission directory
            local_exp_dir = join( WRF4G_DIR , 'var' , 'submission' , self.name )
            logging.debug( "Deleting '%s' directory" % local_exp_dir )
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
    start_date      = Column(u'start_date',DATETIME())
    end_date        = Column(u'end_date',DATETIME())
    chunk_size_h    = Column(u'chunk_size_h', INTEGER)
    restart         = Column(u'restart',DATETIME()) 
    status          = Column(u'status',VARCHAR(length=20))
    current_date    = Column(u'current_date',DATETIME())
    current_chunk   = Column(u'current_chunk',INTEGER)
    nchunks         = Column(u'nchunks',INTEGER)
    label           = Column(u'label',VARCHAR(length=100)) 

    # Realtionships
    experiment      = relationship("Experiment", back_populates="realization")
    chunk           = relationship("Chunk", back_populates= "realization", lazy='dynamic')

    dryrun          = False

    def run(self, first_chunk_run = None , last_chunk_run = None, rerun = False ):
        """ 
        Run n_chunk of the realization.
        If n_chunk=0 run every chunk of the realization which haven't finished yet
        else run (n_chunk) chunks since the last one finished
        """
        first_chunk_run = int( first_chunk_run ) if first_chunk_run else None 
        last_chunk_run  = int( last_chunk_run  ) if last_chunk_run  else None
        #Check the status of the realization
        if self.status == 'FINISHED' and not rerun :
            logging.warn( "\tRealization '%s' already finished." % self.name )
        elif ( self.status == 'SUBMITTED' or self.status == 'RUNNING' ) and not rerun :
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
                ch                = self.chunk.filter( Chunk.chunk_id == first_chunk_run ).one()
                self.restart      = ch.start_date
                self.current_date = ch.start_date
            else :
                #search first chunk to run
                if not self.restart : # run every chunks of the realization
                    first_chunk_run = 1
                else:
                    #search chunk with end_date>restart and start_date<restart
                    try:
                        first_chunk  = self.chunk.filter( and_( Chunk.start_date <= self.restart, 
                                                                Chunk.end_date   >= self.restart ) 
                                                         ).all()[ 0 ]
                        first_chunk_run = first_chunk.chunk_id
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
            l_chunks = self.chunk.filter( and_( Chunk.chunk_id >= first_chunk_run, 
                                                Chunk.chunk_id <= last_chunk_run )
                                        ).all()
            #run chunks
            for index, chunk in enumerate( l_chunks ) :
                #print data of chunks
                logging.info('\t---> Submitting Chunk %d \t%s\t%s' % ( chunk.chunk_id, 
                                                              datetime2datewrf(chunk.start_date), 
                                                              datetime2datewrf(chunk.end_date) ) )
                if not self.dryrun :
                    chunk.run( index, rerun )
            if not self.dryrun :
                # Update reealizaiton status
                self.status = 'SUBMITTED'
            
    def _prepare_sub_files(self):
        """
        Prepare the files needed to submit the realization. 
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
        for file in [ join( WRF4G_DIR, 'etc', 'db.conf' ), 
                      "experiment.wrf4g", "namelist.input" , "experiment.pkl"] :
            if not exists ( expandvars( file ) ) :
                raise Exception( "'%s' is not available" % file )
            else :
                shutil.copy( expandvars( file ) , rea_submission_dir )

    def check_db(self, rea_id, chunk_start_date, chunk_end_date, chunk_id ):
        """ 
        Check if there is a chunk with the same no reconfigurable field. 
        If there is not a chunk with the same no reconfigurable fields => error,.
        If there is a chunk with the same reconfigurable fields, update data.
        """
        #Check if there is a chunk with the same no reconfigurable fields:
        #id_rea,id_chunk,start_date
        try:
            ch = self.chunk.filter( Chunk.rea_id     == rea_id,
                                    Chunk.chunk_id   == chunk_id,
                                    Chunk.start_date == chunk_start_date
                                   ).one()
        except Exception : 
            #if ch does not exist (no chunk with the same no reconfigurable fields)
            raise Exception("ERROR: Chunk with the same name and no reconfigurable fields already exists")
        else:
            #if ch exits,it means a chunk with the same no reconfigurable fields
            logging.debug( "\t\tUpdating chunk on the database" )
            ch.chunk_end_date = chunk_end_date
            return ch

    def cycle_chunks(self, update = False ):
        """
        Create chunks the needed for a realization 
        """
        # Define which calendar is going to be used
        exp_calendar = Calendar(self.experiment.calendar)
        chunk_id = 1
        chunk_start_date = self.start_date
        while chunk_start_date < self.end_date :
            chunk_end_date = exp_calendar.add_hours( chunk_start_date, hours = self.chunk_size_h )
            logging.info( "\t\t---> Chunk %d %s %s" %( chunk_id, 
                                                       datetime2datewrf(chunk_start_date),
                                                       datetime2datewrf(chunk_end_date) ) )
            if update :
                # Check chunk on the database
                ch = self.check_db( rea_id           = self.id, 
                                    chunk_start_date = chunk_start_date, 
                                    chunk_end_date   = chunk_end_date,
                                    chunk_id         = chunk_id
                                    )
            else :
                # Create Chunk
                ch = Chunk( rea_id     = self.id, 
                            start_date = chunk_start_date, 
                            end_date   = chunk_end_date,
                            wps        = 0, 
                            chunk_id   = chunk_id,
                            status     = 'PREPARED' 
                          )
                # Add realization to the experiment 
                self.chunk.append( ch )
            chunk_start_date = chunk_end_date 
            chunk_id    = chunk_id + 1
        # Set the number of chunks of a relaization    
        self.nchunks = chunk_id - 1
 
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
        if self.status == 'PREPARED' :
            resource           = '-'
            exitcode           = '-'
            status             = 'PREPARED'
            gw_job             = '-'
            chunk_distribution = '%d/%d' % ( 0 , self.nchunks )
        else :
            current_chunk      = self.chunk.filter( Chunk.chunk_id == self.current_chunk ).one()
            last_job           = current_chunk.job.order_by( Job.id )[ -1 ]
            resource           = last_job.resource
            exitcode           = last_job.exitcode
            status             = last_job.status
            gw_job             = str( last_job.gw_job )
            chunk_distribution = '%d/%d' % ( current_chunk.chunk_id, self.nchunks )
        #Format chunks run / chunks total
        runt   = int( self.current_date.strftime("%s") ) - int( self.start_date.strftime("%s") ) 
        totalt = int( self.end_date.strftime("%s") )     - int( self.start_date.strftime("%s") )
        #Percentage
        per = runt * 100.0/ totalt
        #Print output
        logging.info( "%-60.60s %-10.10s %-10.10s %-16.16s %-10.10s %6.6s %-3.3s %6.2f" % (
                    self.name[0:60], self.status, chunk_distribution, resource, status, 
                    gw_job, exitcode, per ) )
  
    def stop(self):
        """
        Delete chunks which status is running or submitted 
        """
        l_chunks = self.chunk.filter( or_( Chunk.status == 'SUBMITTED', 
                                           Chunk.status == 'RUNNING' ) 
                                    ).all()
        if not ( l_chunks ):
            logging.info( 'There are not chunks to stop.' )
        else :
            logging.info('---> Stopping Realization %s' % self.name )
            for chunk in l_chunks :
                chunk.dryrun = self.dryrun
                chunk.stop( )
    
class Chunk( Base ):
    """ 
    A class to manage WRF4G chunks
    """
    __tablename__   = 'chunk'

    # Columns
    id              = Column(u'id', INTEGER, primary_key = True, nullable = False)
    rea_id          = Column(u'rea_id', INTEGER, ForeignKey(u'realization.id'))
    start_date      = Column(u'start_date', DATETIME())
    end_date        = Column(u'end_date', DATETIME())
    wps             = Column(u'wps', INTEGER) 
    status          = Column(u'status', VARCHAR(length=20))
    chunk_id        = Column(u'chunk_id', INTEGER)

    # Relationships
    realization     = relationship("Realization", back_populates = "chunk")
    job             = relationship("Job", back_populates = "chunk", lazy = "dynamic")

    dryrun          = False 
 
    #METHODS
    def run (self, index, rerun = False):
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
        inputsandbox += "file://%s/db.conf,"          % rea_path
        inputsandbox += "file://%s/experiment.wrf4g," % rea_path
        inputsandbox += "file://%s/experiment.pkl,"   % rea_path
        inputsandbox += "file://%s/namelist.input"    % rea_path  
        # Add input file if it is exist
        input_files = join( rea_path , 'wrf4g_files.tar.gz' )
        if exists( input_files ) :
            inputsandbox += ",file://%s" % ( input_files )
        # files to add for the outputsandbox
        outputsandbox = "log_%d_${JOB_ID}.tar.gz" % self.chunk_id
        arguments = '%s %s %d %s %s %d' % (
                                        exp_name,
                                        rea_name,                                     
                                        self.chunk_id,
                                        datetime2datewrf( self.start_date ) ,
                                        datetime2datewrf( self.end_date ) ,
                                        1 if rerun else 0
                                        )
        # Create the job template
        gw_job.create_template( name          = rea_name,
                                directory     = rea_path,
                                arguments     = arguments,
                                np            = self.realization.experiment.np,
                                req           = self.realization.experiment.requirements,
                                environ       = self.realization.experiment.environment,
                                inputsandbox  = inputsandbox,
                                outputsandbox = outputsandbox )
        # Submit the template
        job = Job()  #create an object "job"
        # if the first chunk of the realization
        if index == 0 :
            job.gw_job    = gw_job.submit()
        else:
            # if the chunk is not the first of the realization, 
            # gwsubmit has an argument, gw_job of the job before
            chunk_before  = self.realization.chunk.\
                            filter( Chunk.chunk_id == self.chunk_id - 1 ).one()
            job_before    = chunk_before.job.order_by( Job.id )[-1]
            id_job_before = job_before.id          
            gw_job_before = job_before.gw_job
            job.gw_job    = gw_job.submit( dep = gw_job_before ) 
        job.chunk_id = self.chunk_id
        job.run( rerun ) 
        self.job.append( job )
        # Update realizaiton status
        self.status = 'SUBMITTED'

    def stop(self):
        """
        Delete jobs
        """
        logging.info('\t---> Stopping Chunk %d:\t%s\t%s' % ( self.chunk_id,
                                                       datetime2datewrf(self.start_date),
                                                       datetime2datewrf(self.end_date) ) 
                                                       )
        l_jobs = self.job.filter( or_( Job.status != 'PREPARED', 
                                       Job.status == 'FAILED' ) 
                                ).all()
        if not ( l_jobs ):
            logging.info( 'There are not jobs to stop.' )
        else :
            for job in l_jobs :
                job.dryrun = self.dryrun
                job.stop( )
       
class Job( Base ):
    """
    A class to manage WRF4G jobs
    """
    __tablename__   = 'job'

    # Columns
    id              = Column(u'id', INTEGER, primary_key=True, nullable=False)
    gw_job          = Column(u'gw_job', INTEGER)
    gw_restarted    = Column(u'gw_restarted', INTEGER)  
    chunk_id        = Column(u'chunck_id', INTEGER, ForeignKey(u'chunk.id'))
    resource        = Column(u'resource', VARCHAR(length=45))
    status          = Column(u'status', VARCHAR(length=20))
    exitcode        = Column(u'exitcode', VARCHAR(length=20))

    # Relationship
    chunk           = relationship("Chunk", back_populates = "job")
    events          = relationship("Events", back_populates = "job", lazy = 'dynamic')

    dryrun          = False
    
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
            if status == 'FINISHED' and self.chunk.chunk_id == self.chunk.realization.nchunks :
                self.chunk.realization.status = status
            elif status == 'FAILED' :
                self.chunk.realization.status = 'FAILED'
            else :
                self.chunk.realization.status = 'RUNNING'
        if status == 'FINISHED' and self.chunk.realization.status != 'FINISHED' : 
            self.chunk.realization.current_chunk = self.chunk.chunk_id + 1
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
   
    def stop(self):
        """
        Delete a job
        """
        logging.info('\t\t---> Stopping Job %d' % self.gw_job ) 
        if not self.dryrun :
            GWJob().kill( self.gw_job )
            self.set_status( 'CANCEL' )

class Events( Base ):

    __tablename__   = 'events'   
 
    # Columns
    id              = Column(u'id',INTEGER, primary_key=True, nullable=False)
    job_id          = Column(u'job_id',INTEGER, ForeignKey(u'job.id'))
    job_status      = Column(u'job_status',VARCHAR(length=20))
    timestamp       = Column(u'timestamp',DATETIME())

    # Relationship
    job             = relationship("Job", back_populates = "events")
