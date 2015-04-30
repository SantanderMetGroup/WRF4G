import os
import sys
import time
import socket
import shutil
import logging
import tarfile
import glob
import theading

from distutils            import spawn
from os.path              import exists, join, dirname, isfile, basename
from wrf4g.db             import get_session, Job
from wrf4g.core           import JOB_ERROR
from wrf4g.utils          import ( 
                                    VarEnv, datetime2dateiso, 
                                    dateiso2datetime, datewrf2datetime, 
                                    namelist_wps2wrf 
                                    )
from wrf4g.tools.vcp      import VCPURL, copy_file
from wrf4g.tools.archive  import extract

__version__  = '2.0.0'
__author__   = 'Carlos Blanco'
__revision__ = "$Id$"

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
            logging.warn( "Error creating database session" )
        else :
            try :
                self.job = session.query( Job.gw_job == job_id ).order_by( Job.id ).all()[-1]
            except :
                self.job = None
                logging.warn( "Error finding job '%s' on the database" % job_id )

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
                logging.warn( "Error updating status '%s' on the database" % status )
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
                logging.warn( "Error updating wps on the database" )
                self.session.rollback()

    def set_restart_date(self, restart_date ):
        if self.session and self.job :
            self.job.chunk.realization.restart = restart_date
            try :
                self.session.commint( self.job )
            except :
                logging.warn( "Error updating restart date '%s' on the database" % restart_date )
                self.session.rollback()

    def set_cdate(self, cdate):
        if self.session and self.job :
            self.job.chunk.realization.cdate = cdate
            try :
                self.session.commint( self.job )
            except :
                logging.warn( "Error updating current date '%s' on the database" % cdate )
                self.session.rollback()

    def close(self) :
        if self.session : 
            self.session.close()


def clean_wrf_files( job_db, wrf_run_path, out_rea_output_path, rst_rea_output_path, clean="all" , chunk_rdate ):
    """
    Postprocess wrfout files and copy files to the output path 
    """
    for patt in [ "wrfout", "wrfrst", "wrfrain", "wrfxtrm", "wrf24hc" ] :
        all_files = glob.glob( join( wrf_run_path, patt + '*' )
        if clean != 'all' :
            if len( files ) >= 2 :
                files = all_files[ :-1 ]
            else :
                continue
        else :
            files = all_files
        for file_name in files : 
            if file_name == "wrfrst_d01_" + datetime2dateiso( chunk_rdate ) :
                logging.info( "Skipping initial restart file %s" % file_name )
                continue
            else
                if "wrfout" in file_name :
                    ##
                    # Execute postprocessor
                    ##
                    logging.info( "Running postprocessor.%s" % postprocessor )

                    code, output = exec_cmd( "postprocessor.%s %s" % (
                                                preprocessor, file_name )
                    logging.info( output )
                    if code :
                        raise JobError( "%s' has not copied" % file_name,
                                JOB_ERROR[ 'POSTPROCESSOR_FAILED' ] )
                    exec_cmd( 'ncatted -O -a WRF4G_postprocessor,global,o,c,"%s" %s' % 
                                            (preprocessor , file_name) )

                if patt == "wrfrst" and "d01" in file :
                    job_db.set_restart_date( dateiso2datetime( file ) )

                ##
                # Uploading "wrfout", "wrfrst", "wrfrain", "wrfxtrm", "wrf24hc" files
                ##

                oiring = join( wrf_run_path, file_name )
                if patt == "wrfrst" :
                    dest = join( out_rea_output_path, file_name )
                else :
                    dest = join( rst_rea_output_path, file_name )
                try :
                    logging.info( "Uploading file '%s'" % file_name )
                    copy_file( oiring, dest )
                    try :
                        os.remove( oiring )     
                    except : 
                        pass
                except :
                    raise JobError( "'%s' has not copied" % file_name,
                            JOB_ERROR[ 'COPY_OUTPUT_FILE' ] )

def wrf_monitor( job_db, wrf_run_path, out_rea_output_path, rst_rea_output_path, chunk_rdate ):   
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
        else :
            for line in log[ -1: ] :
                if line.find( 'Timing for main: time' ) == 0:
                    cdate = datewrf2datetime( line.split()[4] )
            if not cdate :
                cdate = chunk_rdate
        job_db.set_cdate( cdate )
        clean_wrf_files( job_db, wrf_run_path, out_rea_output_path, rst_rea_output_path, 'closed_files' , chunk_rdate )
        time.sleep( 60 ) # 1 minute

def exe_cmd( cmd ):
    logging.debug("Executing command '%s'" % cmd )
    try:
        import subprocess
        p = subprocess.Popen( "%s" % cmd, shell = True, stdout = subprocess.PIPE,
                              stderr = subprocess.PIPE, close_fds = True )
        output = p.stdout.read().strip()
        code   = p.wait()
    except ImportError :
        import popen2
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
    root_path   = os.path.dirname( pilot_wrf )
    exp_file    = os.path.join( root_path, 'experiment.wrf4g')
    if not sfile( exp_file ) :
        raise JobError( "There is not a experiment.wrf4g file on the WN", 
                        JOB_ERROR[ 'EXPERIMENT_FILE'] )
    exp_conf        = VarEnv( join( rea_dir , 'experiment.wrf4g' )  )
    resource_name   = os.environ.get('GW_HOSTNAME')
    # Find if there is a specific section for this resource
    resource_section = 'DEFAULT'
    for section in exp_conf.sections() :
        if ':' in section and section.split( ':' , 1 )[ 1 ].strip() == resource_name :
            resource_section = section

    output_path      = exp_conf.get_variable( 'output_path' , resource_section )
    domain_path      = exp_conf.get_variable( 'domain_path' , resource_section )
    app_bundles      = exp_conf.get_variable( 'app_bundles' , resource_section )
    preprocessor     = exp_conf.get_variable( 'preprocessor' , resource_section )
    postprocessor    = exp_conf.get_variable( 'postprocessor' , resource_section )
    clean_after_run  = exp_conf.get_variable( 'clean_after_run' , resource_section )
    max_dom          = int( exp_conf.get_variable( 'max_dom' ) )
    extdata_vtable   = exp_conf.get_variable( 'extdata_vtable' , resource_section )
    extdata_interval = exp_conf.get_variable( 'extdata_interval' , resource_section )
    real_parallel    = int( exp_conf.get_variable( 'real_parallel' , resource_section ) )
    wrf_parallel     = int( exp_conf.get_variable( 'wrf_parallel' , resource_section ))
    ppn              = os.environ( 'PPN' )
    np               = os.environ( 'GW_NP' )
    job_id           = int( os.environ( 'GW_JOB_ID' ) )
    restarted_id     = int( os.environ( 'GW_RESTARTED' ) )
     # Default value
    timestep_dxfactor= 6

    # Find if there is a specific section for this resource
    for section in experiment_conf.sections() :
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
    # Logging configuration
    ###
    log_path = join( local_path, 'log' )
    log_file = join( log_path, 'pilot_wrf.log' )
    try :
        os.makedirs( log_path )
    except :
        raise JobError( "Error creating the directory '%s' on the WN" % log_path, 
                        JOB_ERROR[ 'LOG_PATH'] )
    logging.basicConfig( filename = log_file, level = logging.INFO )

    ##
    # DRM4G won't remove root_path if clean_after_run is 1
    ##
    if clean_after_run :
        logging.info( "Creating a .lock file" )
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
        raise JobError( "Errorthis job should not run", 
                        JOB_ERROR[ 'JOB_SHOULD_NOT_RUN'] )

    ##
    # Create a remote tree directory for the realization
    ##
    logging.info( "Creating remote tree directory under '%s'" % output_path )
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
            logging.info( "Creating remote directory '%s'" % remote_path )
            vcp_dir.mkdir()

    ##
    # Copy configured files to the ouput path
    ##
    logging.info( "Copy configured files to '%s'" % output_path )

    for conf_file in [ "db.conf", "experiment.wrf4g", "namelist.input" ] :
        oring = join( root_path, conf_file)
        dest   = join( rea_output_path , conf_file )
        try :
            copy_file( oring, dest )
        except :
            logging.warn( "Error copying file '%s' to '%s'" % ( oring, des ) )

    ##
    # Download and unpack the bundles 
    ##
    logging.info( 'Downloading and unpacking bundles' )
    job_db.set_job_status( 'DOWNLOADING_BUNDLES' )

    archives_path = join( local_path, 'archives' )
    logging.info( "Creating '%s' directory" % archives_path )
    os.makedirs( archives_path )
    for app_bundle in app_bundles.replace(' ','').split('\n') :
        if '|' in app_bundle and app_bundle in app_bundle_allowed :
            bundle_name, bundle_path = app_bundle.split('|')
            oiring = bundle_path
            dest   = join( archives_path, dirname( bundle_path ) )
            logging.info( "Trying to copy '%s'" % oiring )
            copy_file( oiring, dest )
            logging.info( "Unpacking '%s' to '%s'" % ( dest, local_path ) )
            if app_bundle == 'mpi' :
                path_to_extract = root_path
                mpi_config = True
            else :
                path_to_extract = local_path
            extract( dest, to_path = path_to_extract )
    wrf4g_files = join( root_path, 'wrf4g_files.tar.gz' )
    if isfile( wrf4g_files ) :
        logging.info( "Unpacking '%s'" % wrf4g_files )
        extract( wrf4g_files, to_path = local_path )

    ##
    # Setting PATH and LD_LIBRARY_PATH 
    ##
    logging.info( 'Setting PATH and LD_LIBRARY_PATH variables' )

    local_bin_path = join( local_path, 'bin' )
    os.environ[ 'PATH' ] = '%s:%s' % ( local_bin_path, os.environ.get( 'PATH' ) )
    os.environ[ 'LD_LIBRARY_PATH' ] = '%s:%s:%s' % ( join( local_path, 'lib' ), 
                                                    join( local_path, 'lib64' ),
                                                    os.environ.get( 'LD_LIBRARY_PATH' ) )
    if mpi_config :
        os.environ[ 'OPAL_PREFIX' ] = root_path

    ##
    # This is is a little bit tricky prepare the pallalel environment.
    ##
    if ( real_parallel or wrf_parallel ) and (  local_path != root_path ) :
        logging.info( 'Copy configured files to all WNs for parallel execution' )

        tmp_shared = join( root_path, 'tmp_shared' )
        os.makedirs( tmp_shared )
        shutil.copy( local_path, tmp_shared )
        code, output = exec_cmd( "mpirun -pernode --wdir cp -r %s %s" % ( tmp_shared, local_path ) )
        if code :
            logging.info( output )
            raise JobError( "Error copying files to all WNs", JOB_ERROR[ 'COPY_NODES' ] )

    ##
    # Set bin files execute by the group
    ##
    logging.info( 'Setting bin files execute by the group' )

    for exe_file in os.listdir( local_bin_path ) :
        os.chmod( join( local_bin_path, exe_file ), stat.S_IXGRP )

    ####
    # Obtain information about the WN
    ####
    logging.info( 'Obtaining information about the WN' )

    # Host info 
    logging.info( 'Host Name = %s' % socket.gethostname() )

    # OS info
    for file in [ 'debian_version' , 'lsb-release' , 'redhat-release' ] :
        file_name = join( 'etc', file ) 
        if isfile( file_name ):
            f = open( file_name, 'r' )
            logging.info( 'Linux release: %s' % f.read().strip() )
            f.close()
            
    # CPU info
    file_name = join( 'proc', 'cpuinfo' )
    if exists( file_name ) :
        f = open( file_name, 'r' )
        cpu = f.readlines()
        f.close()
        number_of_cpus = 0
        for line in cpu :
            if line.startswith( 'model name' ): 
                model_name = line.rstrip('\n').split(':')[1]
            elif line.find( 'processor' ) == 0:
                number_of_cpus = number_of_cpus + 1
        logging.info( 'CPU (model) = %s' % model_name )
        logging.info( 'CPU (MHz)   = %s' % number_of_cpus )

    # Memory info
    file_name = join( 'proc', 'meminfo' )
    if exists( file_name ):
        f = open( file_name, 'r' )
        mem = f.readlines()
        f.close()
        for line in mem :
            if line.find( 'MemTotal:' ) == 0:
                total_mem = int( line.split()[1] )
        logging.info( 'Memory (kB)  = %s' % total_mem )

    # Disk space check
    fs = os.statvfs( root_path )
    disk_space = fs[4] * fs[0] / 1024 / 1024
    logging.info( 'DiskSpace (MB) = %s' % disk_space )

    ##
    # Check the restart date
    ##
    logging.info( 'Checking restart date' )
    rdate = job_db.get_restart_date()
    if not rdate or rerun :
        logging.info( "Restart date will be '%s'" % chunk_sdate )
        chunk_rdate = chunk_sdate
        chunk_rerun = ".F."
    elif rdate >= chunk_sdate and rdate <= chunk_edate :
        logging.info( "Restart date will be '%s'" % rdate )
        chunk_rdate = rdate
        chunk_rerun = ".T." 
        pattern =  "wrfrst*" + datetime2dateiso( rdate ) + "*" 
        for file_name in VCPURL( rst_rea_output_path ).ls( pattern ):
            # file will follow the pattern: wrfrst_d01_19900101T000000Z.nc
            orig = join( rst_rea_output_path, file_name )
            dest = join( wrf_run_path, file_name )
            try :
                logging.info( "Downloading file '%s'" % file_name )
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
    namelist_wps   = join( wps_path, 'namelist.wps')
    namelist_input = join( wrf_run_path, 'namelist.input')
        
    #Copy namelist.input to wrf_run_path
    shutil.copyfile( join(root_path, 'namelist.input' ), namelist_input )
    
    if job_db.has_wps() :
        logging.info( "The boundaries and initial conditions are available" )
        orig = join( domain_path, namelist_wps )
        dest = namelist_wps
        try :
            logging.info( "Downloading file 'namelist.wps'" )
            copy_file( orig, dest )
        except :
            raise JobError( "'namelist.wps' has not copied",
                        JOB_ERROR[  'COPY_NAMELIST_WPS' ] )
        namelist_wps2wrf( namelist_wps, namelistinput, chunk_rdate, chunk_edate, max_dom, chunk_rerun, timestep_dxfactor)
        job_db.set_job_status( 'DOWN_REAL' )
        pattern =  "wrf[lbif]*_d\d\d_" + datetime2dateiso( sdate ) + "*" 
        for file_name in VCPURL( real_rea_output_path ).ls( pattern ):
            orig = join( real_rea_output_path, file_name )
            # From wrflowinp_d08_ we remove the _ at the end
            dest = join( wrf_run_path, file_name[:-1] )
            try :
                logging.info( "Downloading file '%s'" % file_name )
                copy_file( orig, dest )
            except :
                raise JobError( "'%s' has not copied" % file_name,
                        JOB_ERROR[  'COPY_REAL_FILE' ] )
    else :
        logging.info( "The boundaries and initial conditions are not available" )

        # Change the directory to wps path
        os.chdir( wps_path )

        ##
        #  Get geo_em files and namelist.wps
        ##
        logging.info( "Download geo_em files and namelist.wps" )

        for file_name in VCPURL( real_rea_output_path ).ls( '*' ):
            orig = join( domain_path, file_name )
            # From wrflowinp_d08_ we remove the _ at the end
            dest = join( wps_path, file_name )
            try :
                logging.info( "Downloading file '%s'" % file_name )
                copy_file( orig, dest )
            except :
                raise JobError( "'%s' has not copied" % file_name,
                        JOB_ERROR[ 'COPY_WPS' ] )
        job_db.set_job_status( 'DOWN_WPS' )

        ##
        #  Modify the namelist
        ##
        logging.info( "Modify namelist.wps" )

        exec_cmd( "fortnml -of %s -n %d %s" % ( namelist_wps, max_dom, chunk_sdate ) )
        exec_cmd( "fortnml -of %s -n %d %s" % ( namelist_wps, max_dom, chunk_edate ) )
        exec_cmd( "fortnml -of %s -s max_dom %d" % ( namelist_wps, max_dom ) )
        exec_cmd( "fortnml -of %s -s %s '%s'" % (namelist_input, 'prefix', extdata_vtable) )
        exec_cmd( "fortnml -of %s -s %s %d" % (namelist_input, 'interval_seconds', extdata_interval) )
      
        ##
        # Preprocessor and Ungrib
        ##
        logging.info( "Run preprocessors and ungrib" )

        for vt in extdata_vtable.replace(' ', ''). split( ',') :
            exe_cmd( "fortnml -wof %s -s prefix@ungrib %s" % ( namelist_wps, vt ) )
            vtable = join( wps_path, 'Vtable' )
            if isfile( vtable ) :
                os.remove( vtable ) 
            # This creates a symbolic link 
            os.symlink( join(  wps_path, 'Variable_Tables', 'Vtable.%s' % vt ), vtable)

            ##
            # Execute preprocesor
            ##
            logging.info( "Running preprocessor.%s" % preprocessor )

            code, output = exec_cmd( "preprocessor.%s %s %s %s %s" % (
                                        preprocessor, chunk_sdate, chunk_edate, extdata_path, vt ))
            logging.info( output )
            if code :
                raise JobError( "%s' has not copied" % file_name,
                        JOB_ERROR[ 'PREPROCESSOR_FAILED' ] )

            link_grib     = join( wps_path, 'link_grib.sh' ) 
            grb_data_path = join( wps_path, 'grbData') 
            exec_cmd( "%s %s/*" % ( link_grib, grb_data_path ) )
            ##
            # Run Ungrib
            ##
            logging.info( "Run ungrib" )
            job_db.set_job_status( 'UNGRIB' )

            log_ungrib   = join( log_path, 'ungrib_%s.log' % vt )
            ungrib_exe   = spawn.find_executable( 'ungrib.exe' )
            if not ungrib_exe :
                ungrib_exe = join( wps_path, 'ungrib', 'ungrib.exe' )
            code, output = exec_cmd( "%s >& %s" % ( ungrib_exe, log_ungrib ) )
            if code or not 'Successful completion of ungrib' in open( log_ungrib, 'r' ).read() 
                raise JobError( "'%s' has failed" % ungrib_exe,
                            JOB_ERROR[ 'UNGRIB_FAILED' ] )
            shutil.rmtree( grb_data_path )
            grib_files = glob.glob( join( wps_path, 'GRIBFILE.*' ) )
            for grib_file in grib_files :
                os.remove( grib_file )
      
        ##
        #  Update namelist.wps 
        ##
        log_file.info( "Update namelist for metgrid" ))

        exe_cmd( "fortnml -of %s -s fg_name@metgrid %s" % ( namelist_wps, extdata_vtable ) )
        for var_to_del in [ 'opt_output_from_metgrid_path',
                            'opt_output_from_geogrid_path',
                            'opt_metgrid_tbl_path',
                            'opt_geogrid_tbl_path' ] :
            exe_cmd( "fortnml -f %s -d %s" % ( namelist_wps, var_to_del ) )
      
        ##
        # Run Metgrid
        ##
        logging.info( "Run metgrid" )

        log_metgrid   = join( log_path, 'metgrid.log' )
        metgrid_exe   = spawn.find_executable( 'metgrid.exe' )
        if not metgrid_exe :
            metgrid_exe = join( wps_path, 'metgrid', 'metgrid.exe' )
        code, output = exec_cmd( "%s >& %s" % ( metgrid_exe, log_metgrid ) )
        if code or not 'Successful completion of metgrid' in open( log_metgrid, 'r' ).read() :
            raise JobError( "'%s' has failed" % metgrid_exe, 
                    JOB_ERROR[ 'METGRID_FAILED' ] )
        job_db.set_job_status( 'METGRID' )

        ##
        # Run real
        ##

        # Change the directory to wrf run path
        os.chdir( wrf_run_path )
        
        logging.info( "Run real" )
        job_db.set_job_status( 'REAL' )
        # Create a sumbolic link to run real
        os.symlink( wps_path, wrf_run_path )        
        namelist_wps2wrf( namelist_wps, namelistinput, chunk_rdate, chunk_edate, max_dom, chunk_rerun, timestep_dxfactor)
      
        real_exe = spawn.find_executable( 'real.exe' )
        if real_parallel :
            log_real = join( log_path, 'rsl.out.0000' )
            npernode = "-npernode %s" ppn if ppn else '' 
            cmd =  "mpirun -np %s %s --preload-files namelist.input --preload-files-dest-dir %s %s" % (
                    np, npernode, wrf_run_path ,real_exe ) 
            code, output = exec_cmd( cmd ) 
            if isfile( log_real ) :
                real_rsl_path = join( log_path, 'rsl_real' ) 
                os.mkdir( real_rsl_path )
                rsl_files = glob.glob( join( wrf_run_path, 'rsl.*' ) )
                for rsl_file in rsl_files :
                    shutil.copyfile( rsl_file, join( real_rsl_path, basename( rsl_file ) ) )  
        else :
            log_real = join( log_path, 'real.log' )
            code, output = exec_cmd( "%s >& %s" % ( real_exe, log_real ) )
        if code or not 'SUCCESS COMPLETE REAL_EM' in open( log_real, 'r' ).read() :
            raise JobError( "'%s' has failed" % real_exe,
                    JOB_ERROR[ 'REAL_FAILED' ] )
        
        ##
        # Check if wps files has to be storaged 
        ##   
        if save_wps :
            logging.info( "Saving wps" )
            job_db.set_job_status( 'UPLOAD_WPS' )
            # If the files are WPS, add the date to the name. Three files have to be uploaded: wrfinput_d0?,wrfbdy_d0? and wrflowinp_d0?
            #The command: $ upload_file wps     1990-01-01_00:00:00
            # will create in the repositore three files with the following format: wrfinput_d01_19900101T000000Z
            suffix = "_" + datetime2dateiso(sdate)+ ".nc"
            for wps_file in VCPURL( wps_path ).ls("wrf[lbif]*_d\d\d") :
                oiring = wps_file
                dest   = join( real_rea_output_path, basename(wps_file) , suffix )
                try:
                    logging.info( "Uploading '%s' file" % oiring )
                    copy_file( oiring, dest )
                except :
                    raise JobError( "'%s' has not copied" % oiring,
                        JOB_ERROR[  'COPY_UPLOAD_WPS' ] )
            job_db.set_wps()
    
    # Change the directory to wrf run path
    os.chdir( wrf_run_path )

    ##
    # Start a thread to monitor wrf 
    ##
    if wrf_parallel :
        log_wrf = join( log_path, 'rsl.out.0000' )
    else :
        log_wrf = join( log_path, 'wrf.log' )
    worker = threading.Thread(target = wrf_monitor, 
                            args=(job_db, wrf_run_path, out_rea_output_path, rst_rea_output_path, chunk_rdate ))
    worker.setDaemon(True)
    worker.start()

    ##
    # Run wrf
    ##
    logging.info( "Run wrf" )
    job_db.set_job_status( 'WRF' )

    wrf_exe = spawn.find_executable( 'wrf.exe' )
    if wrf_parallel :
        npernode = "-npernode %s" ppn if ppn else '' 
        cmd =  "mpirun -np %s %s --preload-files namelist.input --preload-files-dest-dir %s %s" % (
                    np, npernode, wrf_run_path ,wrf_exe )
        code, output = exec_cmd( cmd )
        if isfile( log_wrf ) :
            wrf_rsl_path = join( log_path, 'rsl_wrf' ) 
            os.mkdir( wrf_rsl_path )
            rsl_files = glob.glob( join( wrf_run_path, 'rsl.*' ) )
            for rsl_file in rsl_files :
                shutil.copyfile( rsl_file, join( wrf_rsl_path, basename( rsl_file ) ) )  
    else :
        code, output = exec_cmd( "%s >& %s" % ( wrf_exe, log_wrf ) )
    if code or not 'SUCCESS COMPLETE WRF' in open( log_wrf, 'r' ).read() :
        raise JobError( "'%s' has failed" % wrf_exe,
                JOB_ERROR[ 'WRF_FAILED' ] )
    ##
    # Save all files
    ##    
    clean_wrf_files( job_db, wrf_run_path, out_rea_output_path, rst_rea_output_path, "all" , chunk_rdate   )

    ##
    # Close the connection with the database
    ##
    job_db.close()

    ##
    # Create a log bundle 
    ##
    os.chdir( local_path )
    log_file_name = 
    tar = tarfile.open( "log_%d_%d_%s.tar.gz " % ( nchunk, job_id, restarted_id )  , "w:gz" )
    tar.add( 'log' )
    # Local copy that will use as outsandbox 
    shutil.copy_file( log_file_name, join( root_path, log_file_name ) )
    # Copy to repository
    copy_file( log_file_name, join( log_rea_output_path, log_file_name ) )


if __name__ == '__main__':
    try :
        main()
    except JobError :
        try :
            logging.error( JobError.msg )
        except :
            sys.stderr.write( JobError.msg )
        sys.exit( JobError.exit_code )
    except :
        msg = "Unexpected error:", sys.exc_info()[0]
        try :
            logging.error( msg )
         except :
            sys.stderr.write( msg )
        sys.exit( 1 )
