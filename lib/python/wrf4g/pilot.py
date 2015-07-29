from __future__           import with_statement
import os
import re
import sys
import time
import socket
import shutil
import logging
import tarfile
import glob
import threading
import fortran_namelist as fn
from os.path              import ( exists, join, 
                                   dirname, isfile, 
                                   basename, expandvars )
from wrf4g.db             import get_session
from wrf4g.core           import Job
from wrf4g.utils.osinfo   import ( get_hostname, os_release, 
                                   cpu_info, mem_info, 
                                   disk_space_check )
from wrf4g.utils.command  import exec_cmd, which
from wrf4g.utils.archive  import extract
from wrf4g.utils.time     import ( dateiso2datetime, datewrf2datetime, 
                                   datetime2datewrf, datetime2dateiso )
from wrf4g.utils.file     import WRFFile
from wrf4g.utils.namelist import wps2wrf, fix_ptop
from wrf4g.config         import load_exp_pkl
from wrf4g.tools.vcplib   import VCPURL, copy_file

__version__  = '2.0.0'
__author__   = 'Carlos Blanco'
__revision__ = "$Id$"

JOB_ERROR = { 'LOG_PATH'             : 1,
              'COPY_APP'             : 2,
              'APP_ERROR'            : 3,
              'SOURCE_SCRIPT'        : 4,
              'LOCAL_PATH'           : 5,
              'COPY_NODES'           : 6,
              'JOB_SHOULD_NOT_RUN'   : 7,
              'COPY_RST_FILE'        : 8,
              'RESTART_MISMATCH'     : 9,
              'COPY_NAMELIST_WPS'    : 10,
              'COPY_REAL_FILE'       : 11,
              'COPY_BOUND'           : 12,
              'NAMELIST_FAILED'      : 13,
              'PREPROCESSOR_FAILED'  : 14,
              'LINK_GRIB_FAILED'     : 15,
              'UNGRIB_FAILED'        : 16,
              'METGRID_FAILED'       : 17,
              'REAL_FAILED'          : 18,
              'COPY_UPLOAD_WPS'      : 19,
              'WRF_FAILED'           : 20,
              'POSTPROCESSOR_FAILED' : 21,
              'COPY_OUTPUT_FILE'     : 22,
              }

lock = __import__('threading').Lock()

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
        try :
            self.session = get_session()
        except :
            self.session = None
            logging.warning( "Error creating database session" )
        else :
            try :
                self.job = self.session.query( Job ).\
                           filter( Job.gw_job == job_id ).\
                           order_by( Job.id ).all()[-1]
            except :
                self.job = None
                logging.warning( "Error finding job '%s' on the database" % job_id )

    def get_job_status(self):
        if self.session and self.job :
            return self.job.status
        else :
            return 'UNKNOWN'

    def set_job_status(self, status):
        try :
            if self.session and self.job :
                self.job.set_status( status )
                try :
                    self.session.commit( )
                except :
                    logging.warning( "Error updating status '%s' on the database" % status )
                    self.session.rollback()
        except :
            logging.warning( "Error setting job status" )

    def get_restart_date(self):
        if self.session and self.job :
            return self.job.chunk.realization.restart

    def has_wps(self):
        if self.session and self.job :
            return self.job.chunk.wps
        else :
            return 0

    def set_wps(self):
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

    def close(self) :
        if self.session :
           self.session.close()

class PilotParams( object ):
    """
    Class to define the parameters of the experiment 
    """
    pilot_wrf           = os.path.abspath( sys.argv[0] )
    root_path           = os.path.dirname( os.path.dirname( pilot_wrf ) )
    exp_conf            = load_exp_pkl( root_path )
    # Find if there is a specific section for this resource
    resource_name  = os.environ.get( 'GW_HOSTNAME' )
    if exp_conf.has_key( resource_name ) :
        resource_exp_conf = exp_conf[ resource_name ]
    else :
        resource_exp_conf = exp_conf[ 'default' ]
    output_path          = resource_exp_conf[ 'output_path' ]
    domain_path          = resource_exp_conf[ 'domain_path' ]
    app                  = resource_exp_conf[ 'app' ]
    preprocessor         = resource_exp_conf[ 'preprocessor' ]
    postprocessor        = resource_exp_conf[ 'postprocessor' ]
    clean_after_run      = resource_exp_conf[ 'clean_after_run' ]
    max_dom              = int( resource_exp_conf[ 'max_dom' ] )
    save_wps             = resource_exp_conf[ 'save_wps' ]
    wrfout_name_end_date = resource_exp_conf[ 'wrfout_name_end_date' ]
    timestep_dxfactor    = resource_exp_conf[ 'timestep_dxfactor' ]
    extdata_interval     = int( resource_exp_conf[ 'extdata_interval' ] )
    extdata_vtable       = resource_exp_conf[ 'extdata_vtable' ]
    extdata_path         = resource_exp_conf[ 'extdata_path' ]
    real_parallel        = resource_exp_conf[ 'real_parallel' ]
    wrf_parallel         = resource_exp_conf[ 'wrf_parallel' ]
    ppn                  = os.environ.get( 'PPN' )
    np                   = os.environ.get( 'GW_NP' )
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
    # Multi member
    ##
    try :     member     = sys.argv[ 7 ]
    except :  member     = ''

    ##
    # Local path
    ##
    if os.environ.get( "WRF4G_LOCALSCP" ) :
        local_path = join( expandvars( os.environ.get( "WRF4G_LOCALSCP" ) ), 
                           "wrf4g_%s_%d" % ( rea_name, nchunk ) )
    else :
        local_path = root_path

    # WRF path variables
    wps_path             = join( local_path, 'WPS')
    wrf_path             = join( local_path, 'WRFV3' )
    wrf_run_path         = join( wrf_path,   'run' )

    ###
    # logging configuration
    ###
    log_path             = join( root_path, 'log' )
    log_file             = join( log_path,  'main.log' )

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

def clean_wrf_files( job_db, params, clean ):
    """
    Postprocess wrfout files and copy files to the output path 
    """
    with lock :
        for patt in [ "wrfout", "wrfzout", "wrfz2out", "wrfrst", "wrfrain", "wrfxtrm", "wrf24hc" ] :
            all_files_patt = glob.glob( join( params.wrf_run_path, patt + '*' ) )
            if clean == 'closed_files' :
                if len( all_files_patt ) >= ( 2 * params.max_dom ) :
                    all_files_patt.sort( key = os.path.getmtime )
                    files = all_files_patt[ :params.max_dom ]
                else :
                    continue
            elif clean == 'all' :
                files = all_files_patt
            else :
                logging.warning( "'%s' is not a valid option due to all files will be cleaned" % ( clean ) )
                files = all_files_patt
            for file in files :
                logging.info( "Checking '%s' file" % file  ) 
                file_name = basename( file )
                if file_name == "wrfrst_d01_" + datetime2datewrf( params.chunk_rdate ) :
                    # Skip the initial restart file
                    logging.info( "Skipping initial restart file %s" % file_name )
                    continue
                else :
                    if "wrfout" in file_name and params.postprocessor :
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
                                   JOB_ERROR[ 'POSTPROCESSOR_FAILED' ] )
                        post_log = join( params.log_path, 'postprocessor.%s.log' % params.postprocessor )
                        code, output = exec_cmd( "postprocessor.%s %s &>> %s" % (
                                                    params.postprocessor, file_name, post_log ) )
                        if code :
                            logging.info( output )
                            raise JobError( "Error processing '%s' file" % file_name,
                                    JOB_ERROR[ 'POSTPROCESSOR_FAILED' ] )
                        # The file will indicate that it has been postprocessed  
                        exec_cmd( 'ncatted -O -a WRF4G_postprocessor,global,o,c,"%s" %s' % 
                                                (params.postprocessor, file) )

                    if "wrfrst" and "d01" in file_name :
                        job_db.set_restart_date( WRFFile( file_name ).date_datetime() )

                ##
                # Uploading "wrfout", "wrfrst", "wrfzout", "wrfz2out", "wrfrain", "wrfxtrm", "wrf24hc" files
                ##
                if patt != "wrfrst" and params.wrfout_name_end_date == 'yes' :
                    code, output = exec_cmd("ncdump -v Times %s" % file )
                    try :
                        mo = re.findall("(\d{4}-\d{2}-\d{2}_\d{2}:\d{2}:\d{2})", output )
                        mo.sort()
                        dest_file = WRFFile( file_name, mo[-1] ).file_name_out_iso()
                    except Exception, err :
                        logging.error( "ERROR: Calculating wrfout_name_end_date %s" % err )
                        dest_file = WRFFile( file_name).file_name_iso()
                        logging.info( "Destination file will be %s" % dest_file )
                else:
                    dest_file = WRFFile( file_name).file_name_iso()
        
                if patt == "wrfrst" :
                    dest = join( params.rst_rea_output_path, dest_file )
                else :
                    dest = join( params.out_rea_output_path, dest_file )
        
                logging.info( "Uploading file '%s'" % file )
                os.chmod( file, 0664 )
                try :
                    copy_file( file, dest )
                except :
                    logging.error( "'%s' has not copied" % file )   
                    time.sleep( 10 )
                    logging.info( "Uploading file '%s' again" % file )
                    try :
                        copy_file( file, dest )
                    except :
                        raise JobError( "'%s' has not copied" % file, JOB_ERROR[ 'COPY_OUTPUT_FILE' ] ) 
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
        clean_wrf_files( job_db, params, 'closed_files' )
        time.sleep( 600 ) # 10 minutes

def launch_pilot( params ):
    """
    Prepare and launch the job pilot
    """
    ##
    # Give all access permissions to the group and allow other users 
    ##
    os.umask( 022 )

    ##
    # Create log directory
    ##
    try :
        os.makedirs( params.log_path )
    except :
        raise JobError( "Error creating the directory"
                        "'%s' on the worker node" % params.log_path, JOB_ERROR[ 'LOG_PATH'] )
    ##
    # Logging configuration
    ##
    logging.basicConfig( format = '%(asctime)s %(message)s', 
                         filename = params.log_file, level = logging.INFO )
    ##
    # Show information about paths
    ## 
    logging.info( 'Information about directories' )

    # Show root path 
    logging.info( 'Root path  = %s' % params.root_path )

    # Show local path
    logging.info( 'Local path = %s' % params.local_path )
   
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
        if job_db.get_job_status() == 'CANCEL' :
            raise JobError( "Error this job should not run", JOB_ERROR[ 'JOB_SHOULD_NOT_RUN'] )

        job_db.set_job_status( 'RUNNING' )
        ##
        # Create a remote tree directory for the realization
        ##
        logging.info( "Creating remote tree directory under '%s'" % params.output_path )
        job_db.set_job_status( 'CREATE_OUTPUT_PATH' )

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

        for conf_file in [ "db.conf", "experiment.wrf4g", "experiment.pkl", "namelist.input" ] :
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
        os.environ[ 'PATH' ] = '%s:%s' % ( root_bin_path, os.environ.get( 'PATH' ) )
        os.environ[ 'LD_LIBRARY_PATH' ] = '%s:%s:%s' % ( join( params.root_path, 'lib' ),
                                                         join( params.root_path, 'lib64' ),
                                                         os.environ.get( 'LD_LIBRARY_PATH' ) )
        os.environ[ 'PYTHONPATH' ] = '%s:%s' % ( join( params.root_path, 'lib', 'python' ),
                                                 os.environ.get( 'PYTHONPATH' ) )

        if 'wrf_all_in_one' in params.app :
            os.environ[ 'OPAL_PREFIX' ] = params.root_path
      
        ##
        # Configure app 
        ##
        logging.info( 'Configure app' )
        job_db.set_job_status( 'CONF_APP' )

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
                    raise JobError( "'%s' has not copied" % oring, JOB_ERROR[ 'COPY_APP' ] )
                else :
                    logging.info( "Unpacking '%s' to '%s'" % ( dest, params.root_path ) )
                    extract( dest, to_path = params.root_path )
            elif 'command' in app_type :
                logging.info( 'Configuring source script for %s' % app_tag )
                try :
                    f = open( 'easy_source.sh', 'w' )
                    f.write( app_value )
                finally :
                    f.close()
                code, output = exec_cmd( ". ./easy_source.sh && env" )
                if code :
                    logging.info( output )
                    raise JobError( "Error executing source script for %s" app_tag, JOB_ERROR[ 'SOURCE_SCRIPT'] )
                for line in output.split( '\n' ) :
                    if "=" in line and not "(" in line :
                        try :    key, value = line.split( "=" )
                        except : pass
                        else :   os.environ[ key ] = value
            else :
                raise JobError( "Error app type does not exist", JOB_ERROR[ 'APP_ERROR'] )              
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
            os.chmod( join( root_bin_path, exe_file ), 0777 )

        if 'wrf_all_in_one' in params.app :
            os.chmod( join( params.root_path, 'WPS', 'ungrib', 'ungrib.exe' ), 0777 )
            os.chmod( join( params.root_path, 'WPS', 'metgrid', 'metgrid.exe' ), 0777 )
            os.chmod( join( params.root_path, 'WRFV3', 'run', 'real.exe' ), 0777 )
            os.chmod( join( params.root_path, 'WRFV3', 'run', 'wrf.exe' ), 0777 )

        ##
        # This is a little bit tricky prepare the pallalel environment.
        ##
        if ( params.real_parallel == 'yes' or params.wrf_parallel == 'yes' ) and \
           ( params.local_path != params.root_path ) :
            logging.info( "Wiping the directory '%s' on all worker nodes" % params.local_path )
            code, output = exec_cmd( "mpirun -pernode rm -rf %s" % ( params.local_path ) )
            if code :
                logging.info( output )
                raise JobError( "Error wiping the directory '%s' on worker nodes" % (
                                 params.local_path ), JOB_ERROR[ 'LOCAL_PATH'] )
            code, output = exec_cmd( "mpirun -pernode mkdir -p %s" % ( 
                                  params.local_path ) )
            if code :
                logging.info( output )
                raise JobError( "Error copying files to all WNs", JOB_ERROR[ 'COPY_NODES' ] ) 
            for directory in [ 'WPS' , 'WRFV3' ] :
                code, output = exec_cmd( "mpirun -pernode cp -r %s %s" % (  
                                          join( params.root_path, directory ) , params.local_path ) )
                if code :
                    logging.info( output )
                    raise JobError( "Error copying '%s' directory to all WNs" % directory, 
                                    JOB_ERROR[ 'COPY_NODES' ] )

        ##
        # Binaries for execution  
        ##
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
    
        ##
        # Obtain information about the WN
        ##
        logging.info( 'Obtaining information about the worker node' )

        # Host info 
        logging.info( 'Host Name        = %s' % get_hostname() )

        # OS info
        logging.info( 'Linux release    = %s' % os_release() )
                
        # CPU info
        model_name, number_of_cpus = cpu_info()
        logging.info( 'CPU (model)      = %s' % model_name )
        logging.info( 'CPU (processors) = %d' % number_of_cpus )

        # Memory info
        logging.info( 'Memory (kB)      = %s' % mem_info() )

        # Disk space check
        logging.info( 'DiskSpace (MB)   = %d' % disk_space_check( params.root_path ) )

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
            raise JobError( "Restart file is the end date", JOB_ERROR[ 'RESTART_MISMATCH' ] )
        else :
            raise JobError( "There is a mismatch in the restart date", JOB_ERROR[ 'RESTART_MISMATCH' ] )
      
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
                    raise JobError( "'%s' has not copied" % file_name, JOB_ERROR[ 'COPY_RST_FILE' ] )
                files_downloaded += 1
            if not files_downloaded :
                raise JobError( "No restart file has been downloaded", JOB_ERROR[ 'COPY_RST_FILE' ] )
            job_db.set_job_status( 'DOWN_RESTART' )

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
                raise JobError( "'namelist.wps' has not copied", JOB_ERROR[  'COPY_NAMELIST_WPS' ] )
            wps2wrf( params.namelist_wps, params.namelist_input, params.chunk_rdate, 
                        params.chunk_edate, params.max_dom, chunk_rerun, params.timestep_dxfactor)
            job_db.set_job_status( 'DOWN_WPS' )
            pattern =  "wrf[lbif]*_d\d\d_" + datetime2dateiso( sdate ) + "*" 
            for file_name in VCPURL( params.real_rea_output_path ).ls( pattern ):
                orig = join( params.real_rea_output_path, file_name )
                # From wrflowinp_d08_ we remove the _ at the end
                dest = join( params.wrf_run_path, WRFFile(file_name).file_name[:-1] )
                try :
                    logging.info( "Downloading file '%s'" % file_name )
                    copy_file( orig, dest )
                except :
                    raise JobError( "'%s' has not copied" % file_name, JOB_ERROR[  'COPY_REAL_FILE' ] )
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
                               JOB_ERROR[ 'COPY_BOUND' ] )
            job_db.set_job_status( 'DOWN_BOUND' )

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
            except Exception, err :
                raise JobError( "Error modifying namelist: %s" % err, JOB_ERROR[ 'NAMELIST_FAILED' ] )

            ##
            # Preprocessor and Ungrib
            ##
            logging.info( "Run preprocessors and ungrib" )

            for vt, pp, epath in zip( params.extdata_vtable.replace(' ', '').split( ',' ), 
                                      params.preprocessor.replace(' ', '').split( ',' ), 
                                      params.extdata_path.replace(' ', '').split( ',' ) ) :
                try :
                    nmlw = fn.FortranNamelist( params.namelist_wps )
                    nmlw.setValue( "prefix", vt, "ungrib" )
                    nmlw.overWriteNamelist()
                except Exception, err :
                    raise JobError( "Error modifying namelist: %s" % err, JOB_ERROR[ 'NAMELIST_FAILED' ] )
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
                   raise JobError( "Preprocessor '%s' does not exist" % pp, JOB_ERROR[ 'PREPROCESSOR_FAILED' ] )
                preprocessor_log = join( params.log_path, 'preprocessor.%s.log' %  pp )
                code, output = exec_cmd( "preprocessor.%s %s %s %s %s &> %s" % (
                                            pp, datetime2datewrf( params.chunk_rdate ) , 
                                            datetime2datewrf( params.chunk_edate ), 
                                            join( epath, params.member ), vt, 
                                            preprocessor_log ) )
                if code :
                    logging.info( output )
                    raise JobError( "Preprocessor '%s' has failed" % pp,
                            JOB_ERROR[ 'PREPROCESSOR_FAILED' ] )

                link_grib     = join( params.wps_path, 'link_grib.sh' ) 
                os.chmod( link_grib, 0777 )
                grb_data_path = join( params.wps_path, 'grbData') 
                code, output  = exec_cmd( "%s %s/" % ( link_grib, grb_data_path ) )
                if code :
                    logging.info( output )
                    raise JobError( "Error linking grib files", JOB_ERROR[ 'LINK_GRIB_FAILED' ] )
                ##
                # Run Ungrib
                ##
                logging.info( "Run ungrib" )
                job_db.set_job_status( 'UNGRIB' )

                ungrib_log = join( params.log_path, 'ungrib_%s.log' % vt )
                code, output = exec_cmd( "%s > %s" % ( ungrib_exe, ungrib_log) )
                if code or not 'Successful completion' in open( ungrib_log, 'r' ).read() :
                    logging.info( output ) 
                    raise JobError( "'%s' has failed" % ungrib_exe,
                                JOB_ERROR[ 'UNGRIB_FAILED' ] )
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
                nmlw.setValue( "fg_name", params.extdata_vtable.replace( ' ', '').split( ',' ), "metgrid" )
                for var_to_del in [ 'opt_output_from_metgrid_path',
                                    'opt_output_from_geogrid_path',
                                    'opt_metgrid_tbl_path',
                                    'opt_geogrid_tbl_path' ] :
                    nmlw.delVariable( var_to_del )
                nmlw.overWriteNamelist()
            except Exception, err :
                raise JobError( "Error modifying namelist: %s" % err, JOB_ERROR[ 'NAMELIST_FAILED' ] )
          
            ##
            # Run Metgrid
            ##
            logging.info( "Run metgrid" )
            job_db.set_job_status( 'METGRID' )

            metgrid_log = join( params.log_path, 'metgrid.log' )
            code, output = exec_cmd( "%s > %s" % ( metgrid_exe, metgrid_log ) )
            if code or not 'Successful completion' in open( metgrid_log, 'r' ).read() :
                logging.info( output )
                raise JobError( "'%s' has failed" % metgrid_exe, JOB_ERROR[ 'METGRID_FAILED' ] )
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

            if ( params.real_parallel == 'yes' or params.wrf_parallel == 'yes' ) and \
               ( params.local_path != params.root_path ) :
                logging.info( "Copying namelist file to al WNs" )
                bk_namelist = join( params.root_path, 'namelist.input.bk' )
                shutil.copyfile( params.namelist_input, bk_namelist )
                code, output = exec_cmd( "mpirun -pernode cp %s %s" % (
                                  bk_namelist, params.namelist_input ) )
                if code :
                    logging.info( output )
                    raise JobError( "Error copying namelist to all WNs", JOB_ERROR[ 'COPY_NODES' ] )

            logging.info( "Run real" )
            job_db.set_job_status( 'REAL' )
 
            if params.real_parallel == 'yes' :
                real_log = join( params.log_path, 'rsl.out.0000' )
                npernode = "-npernode %s" % params.ppn if params.ppn else '' 
                cmd = "mpirun -np %s %s %s" % ( params.np, npernode, real_exe ) 
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
                raise JobError( "'%s' has failed" % real_exe, JOB_ERROR[ 'REAL_FAILED' ] )
            else :
                logging.info( "real has successfully finished" ) 
            ##
            # Check if wps files has to be storaged 
            ##   
            if params.save_wps == 'yes' :
                logging.info( "Saving wps" )
                job_db.set_job_status( 'UPLOAD_WPS' )
                # If the files are WPS, add the date to the name. Three files have to be uploaded: wrfinput_d0?,wrfbdy_d0? and wrflowinp_d0?
                # The command: $ upload_file wps     1990-01-01_00:00:00
                # will create in the repositore three files with the following format: wrfinput_d01_19900101T000000Z
                suffix = "_" + datetime2dateiso( params.chunk_rdate )+ ".nc"
                for wps_file in VCPURL( params.wps_path ).ls("wrf[lbif]*_d\d\d") :
                    oiring = wps_file
                    dest   = join( params.real_rea_output_path, basename( wps_file) , suffix )
                    try:
                        logging.info( "Uploading '%s' file" % oiring )
                        os.chmod( oiring, 0664 ) 
                        copy_file( oiring, dest )
                    except :
                        raise JobError( "'%s' has not copied" % oiring, JOB_ERROR[  'COPY_UPLOAD_WPS' ] )
                job_db.set_wps()
        
        # Change the directory to wrf run path
        os.chdir( params.wrf_run_path )

        ##
        # Start a thread to monitor wrf 
        ##
        if params.wrf_parallel == 'yes' :
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
        job_db.set_job_status( 'WRF' )

        if params.wrf_parallel == 'yes' :
            npernode = "-npernode %s" % params.ppn if params.ppn else ''
            cmd = "mpirun -np %s %s %s" % ( params.np, npernode, wrf_exe )                       
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
            raise JobError( "'%s' has failed" % wrf_exe, JOB_ERROR[ 'WRF_FAILED' ] )
        else :
            logging.info( "wrf has successfully  finished" ) 
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
        clean_wrf_files( job_db, params, 'all' )
  
        ##
        # Wipe after run
        ##
        if ( params.real_parallel == 'yes' or params.wrf_parallel == 'yes' ) and \
           ( params.local_path != params.root_path ) and ( params.clean_after_run == 'yes' ) :
            logging.info( "Wiping the directory '%s' on all worker nodes" % params.local_path )
            code, output = exec_cmd( "mpirun -pernode rm -rf %s" % ( params.local_path ) )
            if code :
                logging.info( output )
                raise JobError( "Error wiping the directory '%s' on worker nodes" % (
                                 params.local_path ), JOB_ERROR[ 'LOCAL_PATH'] )

        ##
        # Update the status
        ##
        job_db.set_job_status( 'FINISHED' )
        exit_code = 0
    except JobError, err :
        logging.error( err.msg )
        job_db.set_job_status( 'FAILED' )
        exit_code = err.exit_code
    except :
        logging.error( "Unexpected error", exc_info = 1 )
        job_db.set_job_status( 'FAILED' )
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
        job_db.close()
        sys.exit( exit_code )
