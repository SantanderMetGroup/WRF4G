import os
import sys
import logging

from os.path              import join, dirname
from wrf4g.db             import get_session
from wrf4g.core           import Job
from wrf4g.utils.time     import datewrf2datetime
from wrf4g.config         import load_exp_pkl

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
              'COPY_BOUND'           : 10,
              'NAMELIST_FAILED'      : 11,
              'PREPROCESSOR_FAILED'  : 12,
              'LINK_GRIB_FAILED'     : 13,
              'UNGRIB_FAILED'        : 14,
              'METGRID_FAILED'       : 15,
              'REAL_FAILED'          : 16,
              'COPY_UPLOAD_WPS'      : 17,
              'WRF_FAILED'           : 18,
              'POSTPROCESSOR_FAILED' : 19,
              'COPY_OUTPUT_FILE'     : 20,
              'COPY_NODES'           : 21,
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
        if self.session and self.job :
            self.job.set_status( status )
            try :
                self.session.commit( )
            except :
                logging.warning( "Error updating status '%s' on the database" % status )
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
                self.session.commit( )
            except :
                logging.warning( "Error updating wps on the database" )
                self.session.rollback()

    def set_restart_date(self, restart_date ):
        if self.session and self.job :
            self.job.chunk.realization.restart = restart_date
            try :
                self.session.commit( )
            except :
                logging.warning( "Error updating restart date '%s' on the database" % restart_date )
                self.session.rollback()

    def set_current_date(self, current_date):
        if self.session and self.job :
            self.job.chunk.realization.current_date = current_date
            try :
                self.session.commit( )
            except :
                logging.warning( "Error updating current date '%s' on the database" % current_date )
                self.session.rollback()

    def set_exit_code(self, exit_code ):
        if self.session and self.job :
            self.job.exitcode = exit_code
            try :
                self.session.commit( )
            except :
                logging.warning( "Error updating exit code" )
                self.session.rollback()

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
    app_bundles          = resource_exp_conf[ 'app_bundles' ]
    preprocessor         = resource_exp_conf[ 'preprocessor' ]
    postprocessor        = resource_exp_conf[ 'postprocessor' ]
    clean_after_run      = resource_exp_conf[ 'clean_after_run' ]
    extdata_path         = resource_exp_conf[ 'extdata_path' ]
    max_dom              = resource_exp_conf[ 'max_dom' ]
    save_wps             = resource_exp_conf[ 'save_wps' ]
    wrfout_name_end_date = resource_exp_conf[ 'wrfout_name_end_date' ]
    timestep_dxfactor    = resource_exp_conf[ 'timestep_dxfactor' ]
    extdata_vtable       = resource_exp_conf[ 'extdata_vtable' ]
    extdata_interval     = resource_exp_conf[ 'extdata_interval' ]
    real_parallel        = resource_exp_conf[ 'real_parallel' ]
    wrf_parallel         = resource_exp_conf[ 'wrf_parallel' ]
    ppn                  = os.environ.get( 'PPN' )
    np                   = os.environ.get( 'GW_NP' )
    job_id               = int( os.environ.get( 'GW_JOB_ID' ) )
    restarted_id         = int( os.environ.get( 'GW_RESTARTED' ) )
    app_bundle_allowed   = ('mpi' , 'wrf', 'nco', 'netcdf' , 'cdo' )
    exp_name             = sys.argv[1]
    rea_name             = sys.argv[2]
    nchunk               = int( sys.argv[3] )
    ##
    # Dates
    ##
    chunk_sdate          = datewrf2datetime( sys.argv[4] )
    chunk_edate          = datewrf2datetime( sys.argv[5] )
    chunk_rdate          = chunk_sdate

    rerun                = int( sys.argv[6] )

    ##
    # Local path
    ##
    local_scp = os.environ.get( "WRF4G_LOCALSCP" )
    if os.environ.get( "WRF4G_LOCALSCP" ) :
        local_path = join( local_scp, "wrf4g_%s_%s" % ( rea_name, nchunk ) )
    else :
        local_path = root_path

    # WRF path variables
    wps_path             = join( local_path, 'WPS')
    wrf_run_path         = join( local_path, 'WRFV3', 'run')

    ###
    # logging configuration
    ###
    log_path             = join( local_path, 'log' )
    log_file             = join( log_path,   'pilot_wrf.log' )

    ##
    # Namelists
    ##
    namelist_wps         = join( wps_path,     'namelist.wps' )
    namelist_input       = join( wrf_run_path, 'namelist.input' )

    ##
    # Remote paths
    ##
    exp_output_path      = join( output_path, exp_name )
    rea_output_path      = join( exp_output_path, rea_name )
    out_rea_output_path  = join( rea_output_path, 'output')
    rst_rea_output_path  = join( rea_output_path, 'restart')
    real_rea_output_path = join( rea_output_path, 'realout')
    log_rea_output_path  = join( rea_output_path, 'log')

