import os
import sys
import time
import socket
import shutil
import logging
import tarfile
import glob
import threading

from os.path              import exists, join, dirname, isfile, basename
from wrf4g.db             import get_session
from wrf4g.core           import Job
from wrf4g.utils.os       import ( get_hostname, os_release, 
                                   cpu_info, mem_info, 
                                   disk_space_check, which )
from wrf4g.utils          import ( VarEnv, datetime2dateiso, wrffile, 
                                    dateiso2datetime, datewrf2datetime, 
                                    datetime2datewrf, namelist_wps2wrf 
                                    )
from wrf4g.tools.vcplib   import VCPURL, copy_file
from wrf4g.tools.archive  import extract

__version__  = '2.0.0'
__author__   = 'Carlos Blanco'
__revision__ = "$Id$"


JOB_ERROR = { 
              'EXPERIMENT_FILE'      : 1,
              'LOCAL_PATH'           : 2,
              'LOG_PATH'             : 3,
              'JOB_SHOULD_NOT_RUN'   : 4,
              'COPY_RST_FILE'        : 5,
              'RESTART_MISMATCH'     : 6,
              'COPY_NAMELIST_WPS'    : 7,
              'COPY_REAL_FILE'       : 8,
              'COPY_WPS'             : 9,
              'NAMELIST_FAILED'      : 10,
              'PREPROCESSOR_FAILED'  : 11,
              'LINK_GRIB_FAILED'     : 12,
              'UNGRIB_FAILED'        : 13,
              'METGRID_FAILED'       : 14,
              'REAL_FAILED'          : 15,
              'COPY_UPLOAD_WPS'      : 16,
              'WRF_FAILED'           : 17,
              'POSTPROCESSOR_FAILED' : 18,
              'COPY_OUTPUT_FILE'     : 19,
              'COPY_NODES'           : 20,
              }

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
            logger.warning( "Error creating database session" )
        else :
            try :
                self.job = session.query.filter( Job.gw_job == job_id ).order_by( Job.id ).all()[-1]
            except :
                self.job = None
                logger.warning( "Error finding job '%s' on the database" % job_id )

    def get_job_status(self):
        if self.session and self.job :
            return self.job.status
        else :
            return 'UNKNOWN'

    def set_job_status(self, status):
        if self.session and self.job :
            self.job.set_status( status )
            try :
                self.session.commint( self.job )
            except :
                logger.warning( "Error updating status '%s' on the database" % status )
                self.session.rollback()

    def get_restart_date(self):
        if self.session and self.job :
            return self.job.chunk.realization.restart

    def has_wps(self):
        if self.session and self.job :
            return self.job.chunk.wps
        else :
            return 0

    def set_wps(self):
        if self.session and self.job :
            self.job.chunk.wps = 1
            try :
                self.session.commint( self.job )
            except :
                logger.warning( "Error updating wps on the database" )
                self.session.rollback()

    def set_restart_date(self, restart_date ):
        if self.session and self.job :
            self.job.chunk.realization.restart = restart_date
            try :
                self.session.commint( self.job )
            except :
                logger.warning( "Error updating restart date '%s' on the database" % restart_date )
                self.session.rollback()

    def set_cdate(self, cdate):
        if self.session and self.job :
            self.job.chunk.realization.cdate = cdate
            try :
                self.session.commint( self.job )
            except :
                logger.warning( "Error updating current date '%s' on the database" % cdate )
                self.session.rollback()

    def close(self) :
        if self.session : 
            self.session.close()


def clean_wrf_files( job_db, postprocessor, wrfout_name_end_date, wrf_run_path, out_rea_output_path, rst_rea_output_path, chunk_rdate, clean="all" ):
    """
    Postprocess wrfout files and copy files to the output path 
    """
    for patt in [ "wrfout", "wrfrst", "wrfrain", "wrfxtrm", "wrf24hc" ] :
        all_files_patt = glob.glob( join( wrf_run_path, patt + '*' ) )
        if clean != 'all' :
            if len( files ) >= 2 :
                files = all_files_patt[ :-1 ]
            else :
                continue
        else :
            files = all_files_patt
        for file in files :
            file_name = basename( file )
            if file_name is "wrfrst_d01_" + datetime2datewrf( chunk_rdate ) :
                # Skip the initial restart file
                logger.info( "Skipping initial restart file %s" % file_name )
                continue
            else :
                if "wrfout" in file_name and postprocessor :
                    ##
                    # Execute postprocessor
                    ##
                    logger.info( "Running postprocessor.%s" % postprocessor )

                    code, output = exec_cmd( "postprocessor.%s %s 2>&1" % (
                                                postprocessor, file_name ) )
                    logger.info( output )
                    if code :
                        raise JobError( "%s' has not copied" % file_name,
                                JOB_ERROR[ 'POSTPROCESSOR_FAILED' ] )
                    # The file will indicate that it has been postprocessed  
                    exec_cmd( 'ncatted -O -a WRF4G_postprocessor,global,o,c,"%s" %s' % 
                                            (postprocessor, file) )

                if "wrfrst" and "d01" in file_name :
                    job_db.set_restart_date( wrffile( file_name ).date_datetime() )

            ##
            # Uploading "wrfout", "wrfrst", "wrfrain", "wrfxtrm", "wrf24hc" files
            ##
            if patt is "wrfout" and wrfout_name_end_date :
                code, output = exec_cmd("ncdump -v Times %s" % file )
                mo = re.search("(\d{4}-\d{2}-\d{2}_\d{2}:\d{2}:\d{2})", output.split('\n')[-2] )
                dest_file = wrffile( file_name, mo.group() ).file_name_out_iso()
            else:
                dest_file = wrffile( file_name).file_name_iso()
            if patt is "wrfrst" :
                dest = join( rst_rea_output_path, dest_file )
            else :
                dest = join( out_rea_output_path, dest_file )
            try :
                logger.info( "Uploading file '%s'" % file )
                os.chmod( file, 0664 )
                copy_file( file, dest )
                try :
                    os.remove( file )     
                except : 
                    pass
            except :
                raise JobError( "'%s' has not copied" % file,
                        JOB_ERROR[ 'COPY_OUTPUT_FILE' ] )

def wrf_monitor( job_db,postprocessor, wrfout_name_end_date,wrf_run_path, out_rea_output_path, rst_rea_output_path, chunk_rdate ):   
    """
    Monitor wrf.exe processes
    """
    time.sleep( 120 )
    while True :
        try :
            f = open( log_wrf, 'r' )
            log = f.readlines()
        finally :
            f.close()
        for line in log[ -1: ] :
            if line.find( 'Timing for main: time' ) == 0:
                cdate =  datetime2datewrf( line.split()[4] )
        if not cdate :
            cdate = chunk_rdate
        job_db.set_cdate( cdate )
        clean_wrf_files( job_db, postprocessor, wrfout_name_end_date, wrf_run_path, out_rea_output_path, rst_rea_output_path, chunk_rdate, 'closed_files' )
        time.sleep( 60 ) # 1 minute

def exec_cmd( cmd ):
    import popen2
    logger.debug("Executing command '%s'" % cmd )
    p3     = popen2.Popen3( "%s" % cmd )
    output = p3.fromchild.read().strip()
    code   = p3.wait()
    return ( code, output )


def main():  
    ##
    # Give all access permissions to the group and allow other users 
    ##
    os.umask( 022 )

    ##
    # Define variables
    ##
    app_bundle_allowed = ('mpi' , 'wrf', 'nco', 'netcdf' , 'cdo' )

    pilot_wrf   = os.path.abspath( sys.argv[0] )
    root_path   = os.path.dirname( os.path.dirname( pilot_wrf ) )
    exp_file    = os.path.join( root_path, 'experiment.wrf4g')
    if not isfile( exp_file ) :
        raise JobError( "There is not a experiment.wrf4g file on the WN", 
                        JOB_ERROR[ 'EXPERIMENT_FILE'] )
    exp_conf        = VarEnv( exp_file  )
    resource_name   = os.environ.get('GW_HOSTNAME')
    # Find if there is a specific section for this resource
    resource_section = 'DEFAULT'
    for section in exp_conf.sections() :
        if ':' in section and section.split( ':' , 1 )[ 1 ].strip() == resource_name :
            resource_section = section

    output_path      = exp_conf.get_variable( 'output_path' , section = resource_section )
    domain_path      = exp_conf.get_variable( 'domain_path' , section = resource_section )
    app_bundles      = exp_conf.get_variable( 'app_bundles' , section =  resource_section )
    preprocessor     = exp_conf.get_variable( 'preprocessor' , section = resource_section )
    postprocessor    = exp_conf.get_variable( 'postprocessor' , section = resource_section )
    clean_after_run  = exp_conf.get_variable( 'clean_after_run' , section = resource_section )
    extdata_path     = exp_conf.get_variable( 'extdata_path' , section = resource_section )
    max_dom          = int( exp_conf.get_variable( 'max_dom' ) )
    save_wps         = int( exp_conf.get_variable( 'save_wps' , default = 0, section = resource_section ) )
    wrfout_name_end_date = int( exp_conf.get_variable( 'wrfout_name_end_date' , default = 0, section = resource_section ) )
    timestep_dxfactor= exp_conf.get_variable( 'timestep_dxfactor' )
    extdata_vtable   = exp_conf.get_variable( 'extdata_vtable' , section = resource_section )
    extdata_interval = int( exp_conf.get_variable( 'extdata_interval' , section = resource_section ) )
    real_parallel    = int( exp_conf.get_variable( 'real_parallel' , default = 0, section = resource_section ) )
    wrf_parallel     = int( exp_conf.get_variable( 'wrf_parallel' , default = 1, section = resource_section ))
    ppn              = os.environ.get( 'PPN' )
    np               = os.environ.get( 'GW_NP' )
    job_id           = int( os.environ.get( 'GW_JOB_ID' ) )
    restarted_id     = int( os.environ.get( 'GW_RESTARTED' ) )

    # Find if there is a specific section for this resource
    for section in exp_conf.sections() :
        if ':' in section and section.split( ':' , 1 )[ 1 ].strip() == resource_name :
            source_lines = experiment_conf.get_variable( 'app_source_script' , section )

    exp_name    = sys.argv[1]
    rea_name    = sys.argv[2]
    nchunk      = int( sys.argv[3] )
    chunk_sdate = datewrf2datetime( sys.argv[4] )
    chunk_edate = datewrf2datetime( sys.argv[5] )
    rerun       = int( sys.argv[6] )

    ##
    # Create a remote tree directory for the realization
    ##
    local_path = os.environ.get( "WRF4G_LOCALSCP" )
    if local_path :
        local_path = join( local_path, "wrf4g_%s_%s" % ( rea_name, nchunk ) )
        if not exists( local_path ) :
            try :
                os.makedirs( local_path )
            except :
                raise JobError( "Error creating the directory '%s' on the WN" % local_path, 
                                JOB_ERROR[ 'LOCAL_PATH'] )
    else :
        local_path = root_path

    # WRF path variables
    wps_path     = join( local_path, 'WPS')
    wrf_path     = join( local_path, 'WRFV3' )
    wrf_run_path = join( local_path, 'WRFV3', 'run')

    ###
    # logger configuration
    ###
    log_path = join( local_path, 'log' )
    log_file = join( log_path,   'pilot_wrf.log' )
    try :
        os.makedirs( log_path )
    except :
        raise JobError( "Error creating the directory '%s' on the WN" % log_path, JOB_ERROR[ 'LOG_PATH'] )

    logging.basicConfig( filename = log_file, level = logging.INFO )
    logger = logging.getLogger(__name__)
    ##
    # DRM4G won't remove root_path if clean_after_run is 1
    ##
    if clean_after_run :
        logger.info( "Creating a .lock file" )
        f = open( join(root_path, '.lock' ), 'w' )
        f.close()

    ##
    # Get database session
    ##
    job_db = JobDB( job_id )

    ##
    # Check if this job should run
    ##
    if job_db.get_job_status() == 'CANCEL' :
        raise JobError( "Errorthis job should not run", JOB_ERROR[ 'JOB_SHOULD_NOT_RUN'] )

    ##
    # Create a remote tree directory for the realization
    ##
    logger.info( "Creating remote tree directory under '%s'" % output_path )
    job_db.set_job_status( 'CREATING_OUTPUT_PATH' )

    exp_output_path      = join( output_path, exp_name )
    rea_output_path      = join( exp_output_path, rea_name )
    out_rea_output_path  = join( rea_output_path, 'output')
    rst_rea_output_path  = join( rea_output_path, 'restart')
    real_rea_output_path = join( rea_output_path, 'realout')
    log_rea_output_path  = join( rea_output_path, 'log') 

    for remote_path in [  output_path, 
                          exp_output_path,
                          rea_output_path,
                          out_rea_output_path,
                          rst_rea_output_path,
                          real_rea_output_path,
                          log_rea_output_path  ] :
        vcp_dir = VCPURL( remote_path )
        if not vcp_dir.exists() :
            logger.info( "Creating remote directory '%s'" % remote_path )
            vcp_dir.mkdir()

    ##
    # Copy configured files to the ouput path
    ##
    logger.info( "Copy configured files to '%s'" % output_path )

    for conf_file in [ "db.conf", "experiment.wrf4g", "namelist.input" ] :
        oring = join( root_path, conf_file)
        dest   = join( rea_output_path , conf_file )
        try :
            copy_file( oring, dest )
        except :
            logger.warning( "Error copying file '%s' to '%s'" % ( oring, dest ) )

    ##
    # Download and unpack the bundles 
    ##
    logger.info( 'Downloading and unpacking bundles' )
    job_db.set_job_status( 'DOWNLOADING_BUNDLES' )

    archives_path = join( local_path, 'archives' )
    logger.info( "Creating '%s' directory" % archives_path )
    os.makedirs( archives_path )
    for app_bundle in app_bundles.replace(' ','').split('\n') :
        if '|' in app_bundle : 
            bundle_name, bundle_path = app_bundle.split('|')
            if bundle_name in app_bundle_allowed :
                bundle_name, bundle_path = app_bundle.split('|')
                oiring = bundle_path
                dest   = join( archives_path, basename( bundle_path ) )
                logger.info( "Trying to copy '%s'" % oiring )
                copy_file( oiring, dest )
                logger.info( "Unpacking '%s' to '%s'" % ( dest, local_path ) )
                if bundle_name == 'mpi' :
                    mpi_config = True
                extract( dest, to_path = root_path )
    wrf4g_files = join( root_path, 'wrf4g_files.tar.gz' )
    if isfile( wrf4g_files ) :
        logger.info( "Unpacking '%s'" % wrf4g_files )
        extract( wrf4g_files, to_path = root_path )

    ##
    # This is is a little bit tricky prepare the pallalel environment.
    ##
    if ( real_parallel or wrf_parallel ) and (  local_path != root_path ) :
        logger.info( 'Copy configured files to all WNs for parallel execution' )

        code, output = exec_cmd( "mpirun -pernode --wdir cp -r %s %s" % ( root_path, local_path ) )
        if code :
            logger.info( output )
            raise JobError( "Error copying files to all WNs", JOB_ERROR[ 'COPY_NODES' ] )
    ##
    # Clean archives directory
    ##  
    shutil.rmtree( archives_path )

    ##
    # Setting PATH and LD_LIBRARY_PATH 
    ##
    logger.info( 'Setting PATH and LD_LIBRARY_PATH variables' )

    local_bin_path = join( local_path, 'bin' )
    os.environ[ 'PATH' ] = '%s:%s' % ( local_bin_path, os.environ.get( 'PATH' ) )
    os.environ[ 'LD_LIBRARY_PATH' ] = '%s:%s:%s' % ( join( local_path, 'lib' ), 
                                                     join( local_path, 'lib64' ),
                                                     os.environ.get( 'LD_LIBRARY_PATH' ) )
    os.environ[ 'PYTHONPATH' ] = '%s:%s' % ( join( local_path, 'lib', 'python' ), 
                                             os.environ.get( 'PYTHONPATH' ) )
    if mpi_config :
        os.environ[ 'OPAL_PREFIX' ] = root_path

    ##
    # Set bin files execute by the group
    ##
    logger.info( 'Setting bin files execute by the group' )

    for exe_file in os.listdir( local_bin_path ) :
        os.chmod( join( local_bin_path, exe_file ), 0777 )

    ####
    # Obtain information about the WN
    ####
    logger.info( 'Obtaining information about the WN' )

    # Host info 
    logger.info( 'Host Name = %s' % get_hostname() )

    # OS info
    logger.info( 'Linux release: %s' % os_release() )
            
    # CPU info
    model_name, number_of_cpus = cpu_info()
    logger.info( 'CPU (model) = %s' % model_name )
    logger.info( 'CPU (MHz)   = %d' % number_of_cpus )

    # Memory info
    logger.info( 'Memory (kB)  = %s' % mem_info() )

    # Disk space check
    logger.info( 'DiskSpace (MB) = %d' % disk_space_check() )

    ##
    # Check the restart date
    ##
    logger.info( 'Checking restart date' )
    rdate = job_db.get_restart_date()
    if not rdate or rerun :
        logger.info( "Restart date will be '%s'" % chunk_sdate )
        chunk_rdate = chunk_sdate
        chunk_rerun = ".F."
    elif rdate >= chunk_sdate and rdate <= chunk_edate :
        logger.info( "Restart date will be '%s'" % rdate )
        chunk_rdate = rdate
        chunk_rerun = ".T." 
        pattern =  "wrfrst*" + datetime2dateiso( chunk_rdate ) + '*'
        for file_name in VCPURL( rst_rea_output_path ).ls( pattern ):
            # file will follow the pattern: wrfrst_d01_19900101T000000Z.nc
            orig = join( rst_rea_output_path, file_name )
            dest = join( wrf_run_path, wrffile( file_name ).file_name_wrf() )
            try :
                logger.info( "Downloading file '%s'" % file_name )
                copy_file( orig, dest )
            except :
                raise JobError( "'%s' has not copied" % file_name,
                        JOB_ERROR[ 'COPY_RST_FILE' ] )
        job_db.set_job_status( 'DOWN_RESTART' )
    else :
        raise JobError( "There is a mismatch in the restart date",
                JOB_ERROR[ 'RESTART_MISMATCH' ] )

    ##
    # Either WPS runs or the boundaries and initial conditions are available
    ##
    namelist_wps   = join( wps_path,     'namelist.wps' )
    namelist_input = join( wrf_run_path, 'namelist.input' )
        
    #Copy namelist.input to wrf_run_path
    shutil.copyfile( join(root_path, 'namelist.input' ), namelist_input )
    
    if job_db.has_wps() :
        logger.info( "The boundaries and initial conditions are available" )
        orig = join( domain_path, namelist_wps )
        dest = namelist_wps
        try :
            logger.info( "Downloading file 'namelist.wps'" )
            copy_file( orig, dest )
        except :
            raise JobError( "'namelist.wps' has not copied", JOB_ERROR[  'COPY_NAMELIST_WPS' ] )
        namelist_wps2wrf( namelist_wps, namelist_input, chunk_rdate, 
                           chunk_edate, max_dom, chunk_rerun, timestep_dxfactor)
        job_db.set_job_status( 'DOWN_REAL' )
        pattern =  "wrf[lbif]*_d\d\d_" + datetime2dateiso( sdate ) + "*" 
        for file_name in VCPURL( real_rea_output_path ).ls( pattern ):
            orig = join( real_rea_output_path, file_name )
            # From wrflowinp_d08_ we remove the _ at the end
            dest = join( wrf_run_path, wrffile(file_name).file_name[:-1] )
            try :
                logger.info( "Downloading file '%s'" % file_name )
                copy_file( orig, dest )
            except :
                raise JobError( "'%s' has not copied" % file_name, JOB_ERROR[  'COPY_REAL_FILE' ] )
    else :
        logger.info( "The boundaries and initial conditions are not available" )

        # Change the directory to wps path
        os.chdir( wps_path )

        ##
        #  Get geo_em files and namelist.wps
        ##
        logger.info( "Download geo_em files and namelist.wps" )

        for file_name in VCPURL( domain_path ).ls( '*' ):
            orig = join( domain_path, file_name )
            dest = join( wps_path, file_name )
            try :
                logger.info( "Downloading file '%s'" % file_name )
                copy_file( orig, dest )
            except :
                raise JobError( "'%s' has not copied" % file_name,
                        JOB_ERROR[ 'COPY_WPS' ] )
        job_db.set_job_status( 'DOWN_WPS' )

        ##
        #  Modify the namelist
        ##
        logger.info( "Modify namelist.wps" )

        cmds = [ "fortnml -of %s -n %d -s start_date@share %s"       % ( namelist_wps, max_dom, datetime2datewrf( chunk_sdate ) ), 
                 "fortnml -of %s -n %d -s end_date@share %s"         % ( namelist_wps, max_dom, datetime2datewrf( chunk_edate ) ),
                 "fortnml -of %s -n %d -s max_dom@share %d"          % ( namelist_wps, max_dom, max_dom ),
                 "fortnml -of %s -n %d -s interval_seconds@share %d" % ( namelist_wps, max_dom, extdata_interval) ]
        for cmd in cmds :
            code, output = exec_cmd( cmd )
            if code :
                logger.info( output )
                raise JobError( "Error modifying namelist", JOB_ERROR[ 'NAMELIST_FAILED' ] )
        ##
        # Preprocessor and Ungrib
        ##
        logger.info( "Run preprocessors and ungrib" )

        for vt in extdata_vtable.replace(' ', ''). split( ',') :
            code, output = exec_cmd( "fortnml -of %s -s prefix@ungrib %s" % ( namelist_wps, vt ) )
            if code :
                logger.info( output )
                raise JobError( "Error modifying namelist", JOB_ERROR[ 'NAMELIST_FAILED' ] )
            vtable = join( wps_path, 'Vtable' )
            if isfile( vtable ) :
                os.remove( vtable ) 
            # This creates a symbolic link
            os.symlink( join(  wps_path, 'ungrib', 'Variable_Tables', 'Vtable.%s' % vt ), vtable)

            ##
            # Execute preprocesor
            ##
            logger.info( "Running preprocessor.%s" % preprocessor )
          
            code, output = exec_cmd( "preprocessor.%s %s %s %s %s 2>&1" % (
                                        preprocessor, datetime2datewrf( chunk_rdate ) , datetime2datewrf( chunk_edate ), extdata_path, vt ) )
            logger.info( output )
            if code :
                raise JobError( "Preprocessor '%s' has failed" % preprocessor,
                        JOB_ERROR[ 'PREPROCESSOR_FAILED' ] )

            link_grib     = join( wps_path, 'link_grib.sh' ) 
            os.chmod( link_grib, 0777 )
            grb_data_path = join( wps_path, 'grbData') 
            code, output  = exec_cmd( "%s %s/" % ( link_grib, grb_data_path ) )
            if code :
                raise JobError( "Error linking grib files",
                        JOB_ERROR[ 'LINK_GRIB_FAILED' ] )
            ##
            # Run Ungrib
            ##
            logger.info( "Run ungrib" )
            job_db.set_job_status( 'UNGRIB' )

            ungrib_log   = join( log_path, 'ungrib_%s.log' % vt )
            ungrib_exe   = which( 'ungrib.exe' )
            if not ungrib_exe :
                ungrib_exe = join( wps_path, 'ungrib', 'ungrib.exe' )
                os.chmod( ungrib_exe, 0777 )      
            code, output = exec_cmd( "%s > %s" % ( ungrib_exe, ungrib_log) )
            if code or not 'Successful completion' in open( ungrib_log, 'r' ).read() : 
                raise JobError( "'%s' has failed" % ungrib_exe,
                            JOB_ERROR[ 'UNGRIB_FAILED' ] )
            shutil.rmtree( grb_data_path )
            grib_files = glob.glob( join( wps_path, 'GRIBFILE.*' ) )
            for grib_file in grib_files :
                os.remove( grib_file )
      
        ##
        #  Update namelist.wps 
        ##
        logger.info( "Update namelist for metgrid" )

        exec_cmd( "fortnml -of %s -s fg_name@metgrid %s" % ( namelist_wps, extdata_vtable ) )
        for var_to_del in [ 'opt_output_from_metgrid_path',
                            'opt_output_from_geogrid_path',
                            'opt_metgrid_tbl_path',
                            'opt_geogrid_tbl_path' ] :
            exec_cmd( "fortnml -of %s -d %s" % ( namelist_wps, var_to_del ) )
      
        ##
        # Run Metgrid
        ##
        logger.info( "Run metgrid" )

        metgrid_log = join( log_path, 'metgrid.log' )
        metgrid_exe = which( 'metgrid.exe' )
        if not metgrid_exe :
            metgrid_exe = join( wps_path, 'metgrid', 'metgrid.exe' )
            os.chmod( metgrid_exe, 0777 )
        code, output = exec_cmd( "%s > %s" % ( metgrid_exe, metgrid_log ) )
        if code or not 'Successful completion' in open( metgrid_log, 'r' ).read() :
            raise JobError( "'%s' has failed" % metgrid_exe, JOB_ERROR[ 'METGRID_FAILED' ] )
        job_db.set_job_status( 'METGRID' )

        ##
        # Run real
        ##

        # Change the directory to wrf run path
        os.chdir( wrf_run_path )
        
        logger.info( "Run real" )
        job_db.set_job_status( 'REAL' )
        # Create a sumbolic link to run real
        met_files = glob.glob( join( wps_path, 'met_em.d*' ) )
        for met_file in met_files :
            os.symlink( met_file , join( wrf_run_path, basename(met_file) ) )        
        namelist_wps2wrf( namelist_wps, namelist_input, chunk_rdate,
                              chunk_edate, max_dom, chunk_rerun, timestep_dxfactor)

        real_exe       = which( 'real.exe' )
        local_real_exe = join( wrf_run_path, 'real.exe' )
        if real_exe == 'real.exe' :
            os.chmod( real_exe, 0777 )
        if real_parallel :
            real_log = join( log_path, 'rsl.out.0000' )
            npernode = "-npernode %s" % ppn if ppn else '' 
            if local_path is root_path :
                cmd = "mpirun -np %s %s %s" % ( np, npernode, real_exe ) 
            else :
                cmd = "mpirun -np %s %s --preload-files namelist.input --preload-files-dest-dir %s %s" % (
                       np, npernode, wrf_run_path ,real_exe ) 
            code, output = exec_cmd( cmd ) 
            if isfile( real_log ) :
                real_rsl_path = join( log_path, 'rsl_real' ) 
                os.mkdir( real_rsl_path )
                rsl_files = glob.glob( join( wrf_run_path, 'rsl.*' ) )
                for rsl_file in rsl_files :
                    shutil.copyfile( rsl_file, join( real_rsl_path, basename( rsl_file ) ) )  
        else :
            real_log = join( log_path, 'real.log' )
            code, output = exec_cmd( "%s > %s" % ( real_exe, real_log ) )
        if code or not 'SUCCESS COMPLETE' in open( real_log, 'r' ).read() :
            raise JobError( "'%s' has failed" % real_exe, JOB_ERROR[ 'REAL_FAILED' ] )
        
        ##
        # Check if wps files has to be storaged 
        ##   
        if save_wps :
            logger.info( "Saving wps" )
            job_db.set_job_status( 'UPLOAD_WPS' )
            # If the files are WPS, add the date to the name. Three files have to be uploaded: wrfinput_d0?,wrfbdy_d0? and wrflowinp_d0?
            # The command: $ upload_file wps     1990-01-01_00:00:00
            # will create in the repositore three files with the following format: wrfinput_d01_19900101T000000Z
            suffix = "_" + datetime2dateiso( chunk_rdate )+ ".nc"
            for wps_file in VCPURL( wps_path ).ls("wrf[lbif]*_d\d\d") :
                oiring = wps_file
                dest   = join( real_rea_output_path, basename(wps_file) , suffix )
                try:
                    logger.info( "Uploading '%s' file" % oiring )
                    os.chmod( oiring, 0664 ) 
                    copy_file( oiring, dest )
                except :
                    raise JobError( "'%s' has not copied" % oiring, JOB_ERROR[  'COPY_UPLOAD_WPS' ] )
            job_db.set_wps()
    
    # Change the directory to wrf run path
    os.chdir( wrf_run_path )

    ##
    # Start a thread to monitor wrf 
    ##
    if wrf_parallel :
        log_wrf = join( wrf_run_path, 'rsl.out.0000' )
    else :
        log_wrf = join( log_path, 'wrf.log' )
    worker = threading.Thread( target = wrf_monitor, 
                               args   = ( job_db, postprocessor, wrfout_name_end_date, wrf_run_path, 
                                          out_rea_output_path, rst_rea_output_path, 
                                          chunk_rdate ) 
                             )
    worker.setDaemon(True)
    worker.start()

    ##
    # Run wrf
    ##
    logger.info( "Run wrf" )
    job_db.set_job_status( 'WRF' )

    wrf_exe = which( 'wrf.exe' )
    if wrf_exe == 'wrf.exe' :
        os.chmod( wrf_exe, 0777 )
    if wrf_parallel :
        npernode = "-npernode %s" % ppn if ppn else ''
        if local_path is root_path :
            cmd = "mpirun -np %s %s %s" % ( np, npernode, wrf_exe )                       
        else :
            cmd = "mpirun -np %s %s --preload-files namelist.input --preload-files-dest-dir %s %s" % (
                        np, npernode, wrf_run_path ,wrf_exe )
        code, output = exec_cmd( cmd )
        if isfile( log_wrf ) :
            wrf_rsl_path = join( log_path, 'rsl_wrf' ) 
            os.mkdir( wrf_rsl_path )
            rsl_files = glob.glob( join( wrf_run_path, 'rsl.*' ) )
            for rsl_file in rsl_files :
                shutil.copyfile( rsl_file, join( wrf_rsl_path, basename( rsl_file ) ) )  
    else :
        code, output = exec_cmd( "%s > %s" % ( wrf_exe, log_wrf ) )
    if code or not 'SUCCESS COMPLETE' in open( log_wrf, 'r' ).read() :
        raise JobError( "'%s' has failed" % wrf_exe,
                JOB_ERROR[ 'WRF_FAILED' ] )
    ##
    # Save all files
    ##    
    clean_wrf_files( job_db, postprocessor,wrfout_name_end_date,wrf_run_path, out_rea_output_path, rst_rea_output_path, chunk_rdate, 'all' )

    ##
    # Close the connection with the database
    ##
    job_db.close()

    ##
    # Create a log bundle 
    ##
    os.chdir( local_path )
    log_tar = "log_%d_%d_%s.tar.gz" % ( nchunk, job_id, restarted_id )
    tar = tarfile.open( log_tar, "w:gz" )
    tar.add( 'log' )
    # Local copy that will use as outsandbox
    if root_path != local_path : 
        shutil.copyfile( log_tar, join( root_path, log_tar ) )
    # Copy to repository
    copy_file( log_tar, join( log_rea_output_path, log_tar ) )


if __name__ == '__main__':
    try :
        main()
    except JobError, err :
        try :
            logger.error( err.msg )
        except :
            sys.stderr.write( err.msg )
        sys.exit( err.exit_code )
