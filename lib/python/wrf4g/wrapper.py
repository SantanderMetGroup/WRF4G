import os
import re
import sys
import stat
import time
import socket
import shutil
import logging
import tarfile
import glob
import threading
import fortran_namelist as fn
from datetime             import datetime
from os.path              import ( exists, join, 
                                   dirname, isfile, 
                                   basename, expandvars )
from wrf4g.db             import get_session
from wrf4g.core           import Job
from wrf4g.config         import save_pkl
from wrf4g.utils.mpi      import ParallelEnvironment
from wrf4g.utils.osinfo   import ( get_hostname, os_release, 
                                   cpu_info, mem_info, 
                                   disk_space_check )
from wrf4g.utils.command  import exec_cmd, which
from wrf4g.utils.archive  import extract
from wrf4g.utils.time     import ( dateiso2datetime, datewrf2datetime, 
                                   datetime2datewrf, datetime2dateiso )
from wrf4g.utils.file     import WRFFile
from wrf4g.utils.namelist import wps2wrf, fix_ptop
from wrf4g.utils.vcplib   import VCPURL, copy_file
from wrf4g.config         import load_json

__version__  = '2.2.1'
__author__   = 'Carlos Blanco'
__revision__ = "$Id$"

lock = __import__('threading').Lock()

PY2 = sys.version_info[0] == 2
PY3 = sys.version_info[0] == 3

class JobError( Exception ):
    """Raised when job fails.

    Attributes:
        msg       -- explanation of why job failes
        exit_code -- job exit code
    """

    def __init__(self, msg, exit_code ):
        self.msg       = msg
        self.exit_code = exit_code

    def __str__(self):
        return repr( self.msg )

class JobDB( object ) :

    def __init__(self, job_id) :
        self.events  = []
        self.session = None
        self.job_id  = job_id
        self.job     = None

    def check_db(self):
        if not self.session and not self.job :
            try :
                self.session = get_session()
            except Exception as err :
                self.session = None
                logging.warning( "Error creating database session: %s" % str( err ) )
            else :
                try :
                    self.job = self.session.query( Job ).\
                               filter( Job.gw_job == self.job_id ).\
                               order_by( Job.id ).all()[-1]
                except :
                    self.job = None
                    logging.warning( "Error finding job '%s' on the database" % self.job_id )

    def get_job_status(self):
        self.check_db()
        if self.session and self.job :
            return self.job.status
        else :
            return Job.Status.UNKNOWN

    def set_job_status(self, status):
        self.check_db()
        try :
            if self.session and self.job :
                self.job.set_status( status )
                try :
                    self.session.commit( )
                except :
                    logging.warning( "Error updating status '%s' on the database" % status )
                    self.session.rollback()
            else :
                self.events.append( ( status, datetime.utcnow() ) )
        except :
            logging.warning( "Error setting job status" )

    def get_restart_date(self):
        self.check_db()
        if self.session and self.job :
            return self.job.chunk.realization.restart
        return None

    def has_wps(self):
        self.check_db()
        if self.session and self.job :
            return self.job.chunk.wps
        else :
            return 0

    def set_wps(self):
        self.check_db()
        try :
            if self.session and self.job :
                self.job.chunk.wps = 1
                try :
                    self.session.commit( )
                except :
                    logging.warning( "Error updating wps on the database" )
                    self.session.rollback()
        except :
            logging.warning( "Error updating wps on the database" )

    def set_restart_date(self, restart_date ):
        self.check_db()
        try :
            if self.session and self.job :
                self.job.chunk.realization.restart = restart_date
                try :
                    self.session.commit( )
                except :
                    logging.warning( "Error updating restart date '%s' on the database" % restart_date )
                    self.session.rollback()
        except :
            logging.warning( "Error updating restart date '%s' on the database" % restart_date )

    def set_current_date(self, current_date):
        self.check_db()
        try :
            if self.session and self.job :
                self.job.chunk.realization.current_date = current_date
                try :
                    self.session.commit( )
                except :
                    logging.warning( "Error updating current date '%s' on the database" % current_date )
                    self.session.rollback()
        except :
            logging.warning( "Error updating current date '%s' on the database" % current_date )

    def set_exit_code(self, exit_code ):
        self.check_db()
        try :
            if self.session and self.job :
                self.job.exitcode = exit_code
                try :
                    self.session.commit( )
                except :
                    logging.warning( "Error updating exit code" )
                    self.session.rollback()
        except :
            logging.warning( "Error updating exit code" )

    def close(self, directory ) :
        if self.session :
           self.session.close()
        save_pkl( self.events, directory, 'events.pkl' )     

class PilotParams( object ):
    """
    Class to define the parameters of the experiment 
    """
    wrf_wrapper          = os.path.abspath( sys.argv[0] )
    root_path            = os.path.dirname( os.path.dirname( wrf_wrapper ) )
    cfg                  = load_json( root_path, 'realization.json' )
    resource_cfg         = cfg[ 'ensemble/default' ].copy()
    # Find if there is a specific section for this resource
    resource_name        = os.environ.get( 'GW_HOSTNAME' )
    resource_section     = 'resource/' + resource_name
    if resource_section in cfg :
        resource_cfg.update( resource_section )
    output_path          = resource_cfg[ 'output_path' ]
    domain_path          = resource_cfg[ 'domain_path' ]
    app                  = resource_cfg.get( 'app', '' )
    preprocessor         = resource_cfg[ 'preprocessor' ]
    postprocessor        = resource_cfg.get( 'postprocessor', '' )
    clean_after_run      = resource_cfg.get( 'clean_after_run', 'no' )
    files_to_save        = resource_cfg[ 'files_to_save' ]
    max_dom              = int( resource_cfg[ 'max_dom' ] )
    save_wps             = resource_cfg.get( 'save_wps', 'no' )
    wrfout_name_end_date = resource_cfg.get( 'wrfout_name_end_date', 'no' )
    timestep_dxfactor    = resource_cfg.get( 'timestep_dxfactor', '6' )
    extdata_interval     = int( resource_cfg[ 'extdata_interval' ] )
    extdata_vtable       = resource_cfg[ 'extdata_vtable' ]
    extdata_path         = resource_cfg[ 'extdata_path' ]
    constants_name       = resource_cfg.get( 'constants_name', '' )
    job_id               = int( os.environ.get( 'GW_JOB_ID' ) )
    restarted_id         = int( os.environ.get( 'GW_RESTARTED' ) )
    exp_name             = sys.argv[1]
    rea_name             = sys.argv[2]
    nchunk               = int( sys.argv[3] )

    ##
    # Dates
    ##
    chunk_sdate          = datewrf2datetime( sys.argv[4] )
    chunk_edate          = datewrf2datetime( sys.argv[5] )
    chunk_rdate          = chunk_sdate

    ##
    # Varieble to rerun the chunk
    ##
    rerun                = int( sys.argv[6] )

    ##
    # Preprocessor parameters
    ##
    preprocessor_optargs = dict()
    if 'preprocessor_optargs' in resource_cfg :
        preprocessor_optargs = resource_cfg[ 'preprocessor_optargs' ]
    
    ##
    # Local path
    ##
    if os.environ.get( "WRF4G_LOCALSCP" ) :
        local_path = join( expandvars( os.environ.get( "WRF4G_LOCALSCP" ) ), 
                           "wrf4g_%s_%d" % ( rea_name, nchunk ) )
    else :
        local_path = root_path

    # Parallel enviroment
    parallel_real        = resource_cfg[ 'parallel_real' ]
    parallel_wrf         = resource_cfg[ 'parallel_wrf' ]
    
    parallel_env         = ParallelEnvironment.launcher_map.get( resource_cfg[ 'parallel_env' ] )
    if resource_cfg[ 'parallel_env' ] == 'DUMMY' :
        parallel_run         = resource_cfg[ 'parallel_run' ]
        parallel_run_pernode = resource_cfg[ 'parallel_run_pernode' ]
    else :
        parallel_run         = "%s %s %s " % ( parallel_env.launcher, parallel_env.np, os.environ.get( 'GW_NP' ) ) 
        parallel_run_pernode = "%s %s " % ( parallel_env.launcher, parallel_env.pernode )
        
    # WRF path variables
    wps_path             = join( local_path, 'WPS')
    wrf_path             = join( local_path, 'WRFV3' )
    wrf_run_path         = join( wrf_path,   'run' )

    ###
    # logging configuration
    ###
    log_path             = join( root_path, 'log' )
    log_file             = join( log_path,  'main.log' )
 
    codes = { 'INFO'   : logging.INFO,
              'DEBUG'  : logging.DEBUG,
              'WARNING': logging.WARNING,
              'ERROR'  : logging.ERROR
            }

    if resource_cfg.get( 'log_level' ) :
        log_level = codes[ resource_cfg.get( 'log_level' ) ]
    else :
        log_level = logging.INFO

    ##
    # Namelists
    ##
    namelist_wps         = join( wps_path,     'namelist.wps' )
    namelist_input       = join( wrf_run_path, 'namelist.input' )

    ##
    # Remote paths
    ##
    exp_output_path      = join( output_path,     exp_name )
    rea_output_path      = join( exp_output_path, rea_name )
    out_rea_output_path  = join( rea_output_path, 'output')
    rst_rea_output_path  = join( rea_output_path, 'restart')
    real_rea_output_path = join( rea_output_path, 'realout')
    log_rea_output_path  = join( rea_output_path, 'log')

def clean_wrf_files( job_db, params, clean_all = False ):
    """
    Postprocess wrfout files and copy files to the output path 
    """
    with lock :
        for patt in params.files_to_save :
            all_files_patt = glob.glob( join( params.wrf_run_path, patt + '*' ) )
            if not clean_all :
                if len( all_files_patt ) >= ( 2 * params.max_dom ) :
                    all_files_patt.sort( key = os.path.getmtime )
                    files = all_files_patt[ :params.max_dom ]
                else :
                    continue
            else :
                files = all_files_patt
            for file in files :
                logging.info( "Checking '%s' file" % file  ) 
                file_name = basename( file )
                if file_name == "wrfrst_d01_" + datetime2datewrf( params.chunk_rdate ) :
                    # Skip the initial restart file
                    logging.info( "Skipping initial restart file %s" % file_name )
                    continue
                elif "wrfout" in file_name and params.postprocessor :
                    code, output = exec_cmd( "ncdump -h %s" % file_name )
                    if "WRF4G_postprocessor" in output :
                        logging.info( "'%s' was already postprocessed" % file_name )
                        continue
                    ##
                    # Execute postprocessor
                    ##
                    logging.info( "Running postprocessor.%s" % params.postprocessor )
          
                    if not which( "postprocessor.%s" % params.postprocessor ) :
                        raise JobError( "Postprocessor '%s' does not exist" % params.postprocessor, 
                               Job.CodeError.POSTPROCESSOR_FAILED )
                    post_log = join( params.log_path, 'postprocessor.%s.log' % params.postprocessor )
                    code, output = exec_cmd( "postprocessor.%s %s &>> %s" % (
                                                params.postprocessor, file_name, post_log ) )
                    if code :
                        logging.info( output )
                        raise JobError( "Error processing '%s' file" % file_name,
                                Job.CodeError.POSTPROCESSOR_FAILED )
                    # The file will indicate that it has been postprocessed  
                    exec_cmd( 'ncatted -O -a WRF4G_postprocessor,global,o,c,"%s" %s' % 
                                            (params.postprocessor, file) )

                elif "wrfrst" in file_name and "d01" in file_name :
                    restart_date = WRFFile( file_name ).date_datetime()
                    logging.info( "Setting restart date to '%s'" % restart_date )
                    job_db.set_restart_date( restart_date )

                ##
                # Uploading "wrfout", "wrfrst", "wrfzout", "wrfz2out", "wrfrain", "wrfxtrm", "wrf24hc" files
                ##
                if patt != "wrfrst" and params.wrfout_name_end_date == 'yes' :
                    code, output = exec_cmd("ncdump -v Times %s" % file )
                    try :
                        mo = re.findall("(\d{4}-\d{2}-\d{2}_\d{2}:\d{2}:\d{2})", output )
                        mo.sort()
                        dest_file = WRFFile( file_name, mo[-1] ).file_name_out_iso()
                    except Exception as err :
                        logging.error( "ERROR: Calculating wrfout_name_end_date %s" % err )
                        dest_file = WRFFile( file_name).file_name_iso()
                        logging.info( "Destination file will be %s" % dest_file )
                else:
                    dest_file = WRFFile( file_name ).file_name_iso()
        
                if patt == "wrfrst" :
                    dest = join( params.rst_rea_output_path, dest_file )
                else :
                    dest = join( params.out_rea_output_path, dest_file )
        
                logging.info( "Uploading file '%s'" % file )
                os.chmod( file,  stat.S_IRUSR | stat.S_IWUSR | stat.S_IRGRP | stat.S_IWGRP | stat.S_IROTH )
                try :
                    copy_file( file, dest )
                except :
                    logging.error( "'%s' has not copied" % file )   
                    time.sleep( 10 )
                    logging.info( "Uploading file '%s' again" % file )
                    try :
                        copy_file( file, dest )
                    except :
                        raise JobError( "'%s' has not copied" % file, Job.CodeError.COPY_OUTPUT_FILE ) 
                try :
                    os.remove( file )     
                except : 
                    pass

def get_current_date( log_wrf ):
    try :
        try :
            f = open( log_wrf, 'r' )
            log = f.readlines()
            log.reverse()
        finally :
            f.close()
        for line in log :
            if line.find( 'Timing for main: time' ) == 0:
                current_date = datewrf2datetime( line.split()[4] )
                logging.info( "Current date is now '%s'" % current_date )            
                break
        return current_date
    except :
        return None
        
def wrf_monitor( job_db, log_wrf, params ):   
    """
    Monitor wrf.exe processes
    """
    time.sleep( 120 ) # 2 minutes
    logging.info( "Starting monitor" )
    while True :
        logging.info( "Checking wrf files" )
        current_date = get_current_date( log_wrf )
        if not current_date :
            current_date = params.chunk_rdate
        job_db.set_current_date( current_date )
        clean_wrf_files( job_db, params )
        time.sleep( 600 ) # 10 minutes

def launch_wrapper( params ):
    """
    Prepare and launch the job wrapper
    """
    ##
    # Create log directory
    ##
    try :
        os.makedirs( params.log_path )
    except :
        raise JobError( "Error creating the directory"
                        "'%s' on the worker node" % params.log_path, Job.CodeError.LOG_PATH )
    ##
    # Logging configuration
    ##
    logging.basicConfig( format = '%(asctime)s %(message)s', 
                         filename = params.log_file, level = params.log_level )
    ##
    # Show information about paths
    ## 
    logging.info( 'Information about directories' )

    # Show root path 
    logging.info( 'Root path = %s' % params.root_path )

    # Show local path
    logging.info( 'Run path  = %s' % params.local_path )
   
    ##
    # DRM4G won't remove root_path if clean_after_run is 1
    ##
    if params.clean_after_run == 'no' :
        logging.info( "Creating a .lock file" )
        f = open( join( params.root_path, '.lock' ), 'w' )
        f.close()

    ##
    # Get database session
    ##
    job_db = JobDB( params.job_id )
    try :
        ##
        # Check if this job should run
        ##
        if job_db.get_job_status() == Job.Status.CANCEL :
            raise JobError( "Error this job should not run", Job.CodeError.JOB_SHOULD_NOT_RUN )

        job_db.set_job_status( Job.Status.RUNNING )
        ##
        # Create a remote tree directory for the realization
        ##
        logging.info( "Creating remote tree directory under '%s'" % params.output_path )
        job_db.set_job_status( Job.Status.CREATE_OUTPUT_PATH )

        for remote_path in [  params.output_path, 
                              params.exp_output_path,
                              params.rea_output_path,
                              params.out_rea_output_path,
                              params.rst_rea_output_path,
                              params.real_rea_output_path,
                              params.log_rea_output_path  ] :
            vcp_dir = VCPURL( remote_path )
            if not vcp_dir.exists() :
                logging.info( "Creating remote directory '%s'" % remote_path )
                vcp_dir.mkdir()

        ##
        # Copy configured files to the ouput path
        ##
        logging.info( "Copy configured files to '%s'" % params.output_path )

        for conf_file in [ "db.conf", "experiment.wrf4g", "realization.json", "namelist.input" ] :
            oring = join( params.root_path, conf_file )
            dest  = join( params.rea_output_path , conf_file )
            try :
                copy_file( oring, dest )
            except :
                logging.warning( "Error copying file '%s' to '%s'" % ( oring, dest ) )
 
        ##
        # Setting PATH and LD_LIBRARY_PATH 
        ##
        logging.info( 'Setting PATH and LD_LIBRARY_PATH variables' )

        root_bin_path = join( params.root_path, 'bin' )
        PATH = '%s:%s' % ( root_bin_path, os.environ.get( 'PATH' ) )
        logging.info( "PATH=%s" % PATH )
        os.environ[ 'PATH' ] = PATH
        LD_LIBRARY_PATH = '%s:%s:%s' % ( join( params.root_path, 'lib' ),
                                         join( params.root_path, 'lib64' ),
                                         os.environ.get( 'LD_LIBRARY_PATH' ) )
        logging.info( "LD_LIBRARY_PATH=%s" % LD_LIBRARY_PATH )
        os.environ[ 'LD_LIBRARY_PATH' ] = LD_LIBRARY_PATH
        PYTHONPATH = '%s:%s' % ( join( params.root_path, 'lib', 'python' ),
                                 os.environ.get( 'PYTHONPATH' ) )
        logging.info( "PYTHONPATH=%s" % PYTHONPATH )
        os.environ[ 'PYTHONPATH' ] = PYTHONPATH

        if 'wrf_all_in_one' in params.app :
            OPAL_PREFIX = params.root_path
            logging.info( "OPAL_PREFIX=%s" % OPAL_PREFIX )
            os.environ[ 'OPAL_PREFIX' ] = OPAL_PREFIX
       
        ##
        # Configure app 
        ##
        logging.info( 'Configure app' )
        job_db.set_job_status( Job.Status.CONF_APP )

        archives_path = join( params.root_path, 'archives' )
        logging.info( "Creating '%s' directory" % archives_path )
        os.makedirs( archives_path )
        for app in params.app.split('\n') :
            app_tag, app_type, app_value = app.split( '|' )
            if 'bundle' in app_type :
                oring = app_value.strip()
                dest  = join( archives_path, basename( app_value.strip() ) )
                try :
                    logging.info( "Trying to copy '%s'" % oring )
                    copy_file( oring, dest )
                except :
                    raise JobError( "'%s' has not copied" % oring, Job.CodeError.COPY_APP )
                else :
                    logging.info( "Unpacking '%s' to '%s'" % ( dest, params.root_path ) )
                    extract( dest, to_path = params.root_path )
            elif 'command' in app_type :
                logging.info( 'Configuring source script for %s' % app_tag )
                with open( 'easy_source.sh', 'w' ) as f :
                    f.write( app_value )
                code, output = exec_cmd( ". ./easy_source.sh && env" )
                if code :
                    logging.info( output )
                    raise JobError( "Error executing source script for %s" % app_tag, Job.CodeError.SOURCE_SCRIPT )
                for line in output.split( '\n' ) :
                    if "=" in line and not "(" in line :
                        try :    
                            key, value = line.split( "=" )
                        except : 
                            pass
                        else :   
                            logging.debug( "%s=%s" % ( key, value ) )
                            os.environ[ key ] = value
            else :
                raise JobError( "Error app type does not exist", Job.CodeError.APP_ERROR )              
        wrf4g_files = join( params.root_path, 'wrf4g_files.tar.gz' )
        if isfile( wrf4g_files ) :
            logging.info( "Unpacking '%s'" % wrf4g_files )
            extract( wrf4g_files, to_path = params.root_path )

        ##
        # Clean archives directory
        ##  
        shutil.rmtree( archives_path )

        ##
        # Set bin files execute by the group
        ##
        logging.info( 'Setting bin files execute by the group' )

        for exe_file in os.listdir( root_bin_path ) :
            os.chmod( join( root_bin_path, exe_file ), stat.S_IRWXU )

        if 'wrf_all_in_one' in params.app :
            os.chmod( join( params.root_path, 'WPS', 'ungrib', 'ungrib.exe' ), stat.S_IRWXU )
            os.chmod( join( params.root_path, 'WPS', 'metgrid', 'metgrid.exe' ), stat.S_IRWXU )
            os.chmod( join( params.root_path, 'WRFV3', 'run', 'real.exe' ), stat.S_IRWXU )
            os.chmod( join( params.root_path, 'WRFV3', 'run', 'wrf.exe' ), stat.S_IRWXU )

        ##
        # This is a little bit tricky prepare the pallalel environment.
        ##
        if ( params.parallel_real == 'yes' or params.parallel_wrf == 'yes' ) and \
           ( params.local_path != params.root_path ) :
            logging.info( "Wiping the directory '%s' on all worker nodes" % params.local_path )
            code, output = exec_cmd( "%s rm -rf %s" % ( params.parallel_run_pernode,
                                                        params.local_path ) )
            if code :
                logging.info( output )
                raise JobError( "Error wiping the directory '%s' on worker nodes" % (
                                 params.local_path ), Job.CodeError.LOCAL_PATH )
            code, output = exec_cmd( "%s mkdir -p %s" % ( params.parallel_run_pernode, params.local_path ) )
            if code :
                logging.info( output )
                raise JobError( "Error creating direcory in all worker nodes", Job.CodeError.COPY_FILE )
            for directory in [ 'WPS' , 'WRFV3' ] :
                exec_cmd( "%s cp -r %s %s" % ( params.parallel_run_pernode,
                                          join( params.root_path, directory ), params.local_path ) ) 
                if not exists( join( params.local_path, directory ) ) :
                    raise JobError( "Error copying '%s' directory to all worker nodes" % directory,
                                    Job.CodeError.COPY_FILE )

        ##
        # Binaries for execution  
        ##
        ungrib_exe = metgrid_exe = real_exe = wrf_exe = None
        if 'wrf_all_in_one' in params.app :
            ungrib_exe  = join( params.wps_path, 'ungrib', 'ungrib.exe' )
            metgrid_exe = join( params.wps_path, 'metgrid', 'metgrid.exe' )
            real_exe    = join( params.wrf_run_path, 'real.exe' )
            wrf_exe     = join( params.wrf_run_path, 'wrf.exe' )
        else :
            ungrib_exe  = which( 'ungrib.exe' )
            metgrid_exe = which( 'metgrid.exe' )
            real_exe    = which( 'real.exe' )
            wrf_exe     = which( 'wrf.exe' )
        if not ungrib_exe or not metgrid_exe or not real_exe or not wrf_exe :
            raise JobError( "Error finding WRF binaries", Job.CodeError.BINARY )
         
        ##
        # Obtain information about the WN
        ##
        logging.info( 'Obtaining information about the worker node' )

        # Host info 
        logging.info( 'Host name        = %s' % get_hostname() )

        # OS info
        logging.info( 'Linux release    = %s' % os_release() )
                
        # CPU info
        model_name, number_of_cpus = cpu_info()
        logging.info( 'CPU (model)      = %s' % model_name )
        logging.info( 'CPU (processors) = %d' % number_of_cpus )

        # Memory info
        logging.info( 'RAM Memory       = %s MB' % mem_info() )

        # Disk space check
        logging.info( 'DiskSpace of %s  = %d GB' % ( params.root_path, disk_space_check( params.root_path ) ) )

        ##
        # Check the restart date
        ##
        logging.info( 'Checking restart date' )
        rdate = job_db.get_restart_date()
        if not rdate or params.rerun :   
            logging.info( "Restart date will be '%s'" % params.chunk_sdate )
            if params.nchunk > 1 :
                chunk_rerun = ".T."
            else :
                chunk_rerun = ".F."
        elif rdate >= params.chunk_sdate and rdate < params.chunk_edate :
            logging.info( "Restart date will be '%s'" % rdate )
            params.chunk_rdate = rdate
            chunk_rerun = ".T." 
        elif rdate == params.chunk_edate :
            raise JobError( "Restart file is the end date", Job.CodeError.RESTART_MISMATCH )
        else :
            raise JobError( "There is a mismatch in the restart date", Job.CodeError.RESTART_MISMATCH )
      
        if chunk_rerun == ".T." :
            pattern =  "wrfrst*" + datetime2dateiso( params.chunk_rdate ) + '*'
            files_downloaded = 0
            for file_name in VCPURL( params.rst_rea_output_path ).ls( pattern ):
                # file will follow the pattern: wrfrst_d01_19900101T000000Z.nc
                orig = join( params.rst_rea_output_path, file_name )
                dest = join( params.wrf_run_path, WRFFile( file_name ).file_name_wrf() )
                try :
                    logging.info( "Downloading file '%s'" % file_name ) 
                    copy_file( orig, dest )
                except :
                    raise JobError( "'%s' has not copied" % file_name, Job.CodeError.COPY_RST_FILE )
                files_downloaded += 1
            if not files_downloaded :
                raise JobError( "No restart file has been downloaded", Job.CodeError.COPY_RST_FILE )
            job_db.set_job_status( Job.Status.DOWN_RESTART )

        ##
        # Either WPS runs or the boundaries and initial conditions are available
        ##
            
        #Copy namelist.input to wrf_run_path
        shutil.copyfile( join( params.root_path, 'namelist.input' ), params.namelist_input )
        
        if job_db.has_wps() :
            logging.info( "The boundaries and initial conditions are available" )
            orig = join( params.domain_path, basename( params.namelist_wps ) )
            dest = params.namelist_wps
            try :
                logging.info( "Downloading file 'namelist.wps'" )
                copy_file( orig, dest )
            except :
                raise JobError( "'namelist.wps' has not copied", Job.CodeError.COPY_FILE )
            wps2wrf( params.namelist_wps, params.namelist_input, params.chunk_rdate, 
                        params.chunk_edate, params.max_dom, chunk_rerun, params.timestep_dxfactor)
            job_db.set_job_status( Job.Status.DOWN_WPS )
            pattern =  "wrf[lbif]*_d\d\d_" + datetime2dateiso( sdate ) + "*" 
            for file_name in VCPURL( params.real_rea_output_path ).ls( pattern ):
                orig = join( params.real_rea_output_path, file_name )
                # From wrflowinp_d08_ we remove the _ at the end
                dest = join( params.wrf_run_path, WRFFile(file_name).file_name[:-1] )
                try :
                    logging.info( "Downloading file '%s'" % file_name )
                    copy_file( orig, dest )
                except :
                    raise JobError( "'%s' has not copied" % file_name, Job.CodeError.COPY_REAL_FILE )
        else :
            logging.info( "The boundaries and initial conditions are not available" )

            # Change the directory to wps path
            os.chdir( params.wps_path )

            ##
            #  Get geo_em files and namelist.wps
            ##
            logging.info( "Download geo_em files and namelist.wps" )

            for file_name in VCPURL( params.domain_path ).ls( '*' ):
                if '.nc' in file_name or 'namelist' in file_name :
                    orig = join( params.domain_path, file_name )
                    dest = join( params.wps_path, file_name )
                    try :
                        logging.info( "Downloading file '%s'" % file_name )
                        copy_file( orig, dest )
                    except :
                        raise JobError( "'%s' has not copied" % file_name,
                               Job.CodeError.COPY_BOUND )
            job_db.set_job_status( Job.Status.DOWN_BOUND )

            ##
            #  Modify the namelist
            ##
            logging.info( "Modify namelist.wps" )

            try :
                nmlw = fn.FortranNamelist( params.namelist_wps )
                nmlw.setValue( "max_dom", params.max_dom )
                nmlw.setValue( "start_date", params.max_dom * [ datetime2datewrf( params.chunk_sdate ) ] )
                nmlw.setValue( "end_date", params.max_dom * [ datetime2datewrf( params.chunk_edate ) ] )
                nmlw.setValue( "interval_seconds", params.extdata_interval )
                nmlw.overWriteNamelist() 
            except Exception as err :
                raise JobError( "Error modifying namelist: %s" % err, Job.CodeError.NAMELIST_FAILED )

            ##
            # Preprocessor and Ungrib
            ##
            logging.info( "Run preprocessors and ungrib" )

            for i, ( vt, pp, epath ) in enumerate( zip( params.extdata_vtable.replace(' ', '').split( ',' ), 
                                                        params.preprocessor.replace(' ', '').split( ',' ), 
                                                        params.extdata_path.replace(' ', '').split( ',' ) ) ) :
                try :
                    nmlw = fn.FortranNamelist( params.namelist_wps )
                    nmlw.setValue( "prefix", vt, "ungrib" )
                    nmlw.overWriteNamelist()
                except Exception as err :
                    raise JobError( "Error modifying namelist: %s" % err, Job.CodeError.NAMELIST_FAILED )
                vtable = join( params.wps_path, 'Vtable' )
                if isfile( vtable ) :
                    os.remove( vtable ) 
                # This creates a symbolic link
                os.symlink( join( params.wps_path, 'ungrib', 'Variable_Tables', 'Vtable.%s' % vt ), vtable)

                ##
                # Execute preprocesor
                ##
                logging.info( "Running preprocessor.%s" % pp )
                
                if not which( "preprocessor.%s" % pp ) :
                   raise JobError( "Preprocessor '%s' does not exist" % pp, Job.CodeError.PREPROCESSOR_FAILED )
                optargs = ""
                for arg in params.preprocessor_optargs.values() : 
                    optargs = optargs + " " + arg.split( ',' )[ i ]
                preprocessor_log = join( params.log_path, 'preprocessor.%s.log' %  pp )
                code, output = exec_cmd( "preprocessor.%s %s %s %s %s &> %s" % (
                                            pp, datetime2datewrf( params.chunk_rdate ) , 
                                            datetime2datewrf( params.chunk_edate ), epath, 
                                            optargs, preprocessor_log ) )
                if code :
                    logging.info( output )
                    raise JobError( "Preprocessor '%s' has failed" % pp,
                            Job.CodeError.PREPROCESSOR_FAILED )

                link_grib     = join( params.wps_path, 'link_grib.sh' ) 
                os.chmod( link_grib, stat.S_IRWXU )
                grb_data_path = join( params.wps_path, 'grbData') 
                code, output  = exec_cmd( "%s %s/" % ( link_grib, grb_data_path ) )
                if code :
                    logging.info( output )
                    raise JobError( "Error linking grib files", Job.CodeError.LINK_GRIB_FAILED )
                ##
                # Run Ungrib
                ##
                logging.info( "Run ungrib" )
                job_db.set_job_status( Job.Status.UNGRIB )

                ungrib_log = join( params.log_path, 'ungrib_%s.log' % vt )
                code, output = exec_cmd( "%s > %s" % ( ungrib_exe, ungrib_log) )
                if code or not 'Successful completion' in open( ungrib_log, 'r' ).read() :
                    logging.info( output ) 
                    raise JobError( "'%s' has failed" % ungrib_exe,
                                Job.CodeError.UNGRIB_FAILED )
                else :
                    logging.info( "ungrib has successfully finished" )
                shutil.rmtree( grb_data_path )
                grib_files = glob.glob( join( params.wps_path, 'GRIBFILE.*' ) )
                for grib_file in grib_files :
                    os.remove( grib_file )
          
            ##
            #  Update namelist.wps 
            ##
            logging.info( "Update namelist for metgrid" )
         
            try :
                nmlw = fn.FortranNamelist( params.namelist_wps )
                nmlw.setValue( "fg_name", params.extdata_vtable.replace(' ', '').split( ',' ), "metgrid" )
                if params.constants_name :
                    nmlw.setValue( "constants_name", params.constants_name.replace(' ', '').split( ',' ), "metgrid" )
                for var_to_del in [ 'opt_output_from_metgrid_path',
                                    'opt_output_from_geogrid_path',
                                    'opt_metgrid_tbl_path',
                                    'opt_geogrid_tbl_path' ] :
                    nmlw.delVariable( var_to_del )
                nmlw.overWriteNamelist()
            except Exception as err :
                raise JobError( "Error modifying namelist: %s" % err, Job.CodeError.NAMELIST_FAILED )
          
            ##
            # Run Metgrid
            ##
            logging.info( "Run metgrid" )
            job_db.set_job_status( Job.Status.METGRID )

            metgrid_log = join( params.log_path, 'metgrid.log' )
            code, output = exec_cmd( "%s > %s" % ( metgrid_exe, metgrid_log ) )
            if code or not 'Successful completion' in open( metgrid_log, 'r' ).read() :
                logging.info( output )
                raise JobError( "'%s' has failed" % metgrid_exe, Job.CodeError.METGRID_FAILED )
            else :
                logging.info( "metgrid has successfully finished" )

            ##
            # Run real
            ##

            # Change the directory to wrf run path
            os.chdir( params.wrf_run_path )
            
            # Create a sumbolic link to run real
            met_files = glob.glob( join( params.wps_path, 'met_em.d*' ) )
            for met_file in met_files :
                os.symlink( met_file , join( params.wrf_run_path, basename(met_file) ) )
            fix_ptop( params.namelist_input )
            wps2wrf( params.namelist_wps, params.namelist_input, params.chunk_rdate,
                        params.chunk_edate, params.max_dom , chunk_rerun, params.timestep_dxfactor)

            if ( params.parallel_real == 'yes' or params.parallel_wrf == 'yes' ) and \
               ( params.local_path != params.root_path ) :
                logging.info( "Copying namelist file to al WNs" )
                bk_namelist = join( params.root_path, 'namelist.input.bk' )
                shutil.copyfile( params.namelist_input, bk_namelist )
                code, output = exec_cmd( "%s cp %s %s" % ( params.parallel_run_pernode,
                                  bk_namelist, params.namelist_input ) )
                if code :
                    logging.info( output )
                    raise JobError( "Error copying namelist to all WNs", Job.CodeError.COPY_FILE )

            logging.info( "Run real" )
            job_db.set_job_status( Job.Status.REAL )
 
            if params.parallel_real == 'yes' :
                real_log = join( params.wrf_run_path, 'rsl.out.0000' )
                cmd = "%s %s" % ( params.parallel_run, real_exe ) 
                code, output = exec_cmd( cmd ) 
                if isfile( real_log ) :
                    real_rsl_path = join( params.log_path, 'rsl_real' ) 
                    os.mkdir( real_rsl_path )
                    rsl_files = glob.glob( join( params.wrf_run_path, 'rsl.*' ) )
                    for rsl_file in rsl_files :
                        shutil.copyfile( rsl_file, join( real_rsl_path, basename( rsl_file ) ) )  
            else :
                real_log = join( params.log_path, 'real.log' )
                code, output = exec_cmd( "%s > %s" % ( real_exe, real_log ) )
            if code or not 'SUCCESS COMPLETE' in open( real_log, 'r' ).read() :
                logging.info( output )
                raise JobError( "'%s' has failed" % real_exe, Job.CodeError.REAL_FAILED )
            else :
                logging.info( "real has successfully finished" ) 
            ##
            # Check if wps files has to be storaged 
            ##   
            if params.save_wps == 'yes' :
                logging.info( "Saving wps" )
                job_db.set_job_status( Job.Status.UPLOAD_WPS )
                # If the files are WPS, add the date to the name. Three files have to be uploaded: wrfinput_d0?,wrfbdy_d0? and wrflowinp_d0?
                # The command: $ upload_file wps     1990-01-01_00:00:00
                # will create in the repositore three files with the following format: wrfinput_d01_19900101T000000Z
                suffix = "_" + datetime2dateiso( params.chunk_rdate )+ ".nc"
                for wps_file in VCPURL( params.wps_path ).ls("wrf[lbif]*_d\d\d") :
                    oiring = wps_file
                    dest   = join( params.real_rea_output_path, basename( wps_file) , suffix )
                    try:
                        logging.info( "Uploading '%s' file" % oiring )
                        os.chmod( oiring, stat.S_IRUSR | stat.S_IWUSR | stat.S_IRGRP | stat.S_IWGRP | stat.S_IROTH ) 
                        copy_file( oiring, dest )
                    except :
                        raise JobError( "'%s' has not copied" % oiring, Job.CodeError.COPY_UPLOAD_WPS )
                job_db.set_wps()
        
        # Change the directory to wrf run path
        os.chdir( params.wrf_run_path )

        ##
        # Start a thread to monitor wrf 
        ##
        if params.parallel_wrf == 'yes' :
            log_wrf = join( params.wrf_run_path, 'rsl.out.0000' )
        else :
            log_wrf = join( params.log_path, 'wrf.log' )
        worker = threading.Thread( target = wrf_monitor, args = ( job_db, log_wrf, params ) )
        worker.setDaemon(True)
        worker.start()

        ##
        # Wipe WPS path
        ##
        if params.clean_after_run == 'yes' : 
            logging.info( "Wiping '%s' directory " % params.wps_path )
            try :
                shutil.rmtree( params.wps_path )
            except :
                logging.info( "Error wiping '%s' directory " % params.wps_path )

        ##
        # Run wrf
        ##
        logging.info( "Run wrf" )
        job_db.set_job_status( Job.Status.WRF )

        if params.parallel_wrf == 'yes' :
            cmd = "%s %s" % ( params.parallel_run, wrf_exe )                       
            code, output = exec_cmd( cmd )
            if isfile( log_wrf ) :
                wrf_rsl_path = join( params.log_path, 'rsl_wrf' ) 
                os.mkdir( wrf_rsl_path )
                rsl_files = glob.glob( join( params.wrf_run_path, 'rsl.*' ) )
                for rsl_file in rsl_files :
                    shutil.copyfile( rsl_file, join( wrf_rsl_path, basename( rsl_file ) ) )  
        else :
            code, output = exec_cmd( "%s > %s" % ( wrf_exe, log_wrf ) )
        if code or not 'SUCCESS COMPLETE' in open( log_wrf, 'r' ).read() :
            logging.info( output )  
            raise JobError( "'%s' has failed" % wrf_exe, Job.CodeError.WRF_FAILED )
        else :
            logging.info( "wrf has successfully finished" ) 
        ##
        # Update current date
        ##
        current_date = get_current_date( log_wrf )
        if not current_date :
            current_date = params.chunk_rdate
        job_db.set_current_date( current_date )

        ##
        # Save all files
        ##    
        clean_wrf_files( job_db, params, clean_all = True )
  
        ##
        # Wipe after run
        ##
        if ( params.parallel_real == 'yes' or params.parallel_wrf == 'yes' ) and \
           ( params.local_path != params.root_path ) and ( params.clean_after_run == 'yes' ) :
            logging.info( "Wiping the directory '%s' on all worker nodes" % params.local_path )
            code, output = exec_cmd( "%s rm -rf %s" % ( params.parallel_run_pernode, params.local_path ) )
            if code :
                logging.info( output )
                logging.error( "Error wiping the directory '%s' on worker nodes" % params.local_path )
        ##
        # Update the status
        ##
        job_db.set_job_status( Job.Status.FINISHED )
        exit_code = 0
    except JobError as err :
        logging.error( err.msg )
        job_db.set_job_status( Job.Status.FAILED )
        exit_code = err.exit_code
    except :
        logging.error( "Unexpected error", exc_info = 1 )
        job_db.set_job_status( Job.Status.FAILED )
        exit_code = 255
    finally :
        ##
        # Create a log bundle 
        ##
        os.chdir( params.root_path )
        log_name = "log_%d_%d" % ( params.nchunk, params.job_id )
        log_tar  = log_name + '.tar.gz' 
        try :
            logging.info( "Create tar file for logs" ) 
            tar = tarfile.open( log_tar , "w:gz" )
            tar.add( 'log', arcname = log_name )
        finally :
            tar.close()
        # Copy to repository
        oring = join( params.root_path, log_tar )
        dest  = join( params.log_rea_output_path, log_tar )
        copy_file( oring, dest )

        ##
        # Close the connection with the database
        ##
        job_db.set_exit_code( exit_code )
        job_db.close( params.root_path  )
        sys.exit( exit_code )
