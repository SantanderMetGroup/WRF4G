import os
import re
import copy
import json
import logging
from os.path          import expandvars, expanduser, exists, join, abspath
from wrf4g.utils      import dict2obj
from wrf4g.utils.mpi  import ParallelEnvironment 
from wrf4g.utils.time import datewrf2datetime, Calendar
from wrf4g.utils.file import VarEnv, make_writeable, validate_name

__version__  = '2.2.0'
__author__   = 'Carlos Blanco'
__revision__ = "$Id$"

YES_NO_VARIABLES    = ( 'clean_after_run', 'save_wps', 'parallel_real', 
                        'parallel_wrf' , 'wrfout_name_end_date', 'chunk_restart' )

def get_conf( directory = './' ):
    """
    Read the experiment configuration from experiment.wrf4g file.
    """
    exp_dir = expandvars( expanduser( directory ) )
    if not exists( exp_dir ):
        raise Exception("'%s' does not exist" % exp_dir )
    exp_file = join( exp_dir, 'experiment.wrf4g' )
    if not exists( exp_file ):
        raise Exception("'%s' does not exist" % exp_file )
    make_writeable( exp_file )
    logging.debug( "Reading '%s' file" % exp_file )
    exp_env = VarEnv( exp_file )
    exp_conf_dict = dict()
    exp_conf_dict[ 'default' ] = exp_env.defaults()
    for section in exp_env.sections() :
        exp_conf_dict[ section ] = dict( exp_env.items( section ) )
    
    # Checking experinment.wrf4g file    
    sanity_check = SanityCheck( exp_conf_dict )
    sanity_check.experiment_name()
    sanity_check.yesno_vars()
    sanity_check.calendar()
    sanity_check.dates()
    sanity_check.parallel_env() 
    sanity_check.app()
    sanity_check.namelist()
    if sanity_check.total_errors :
        raise Exception( "Please review your experiment configuration" )
    return sanity_check.cfg_final

class SanityCheck():
    """
    Chenck if experiment variables are written well
    """

    def __init__(self, cfg) :
      
        self.cfg       = copy.deepcopy( cfg )
        self.cfg_final = copy.deepcopy( cfg )
        self.total_errors  = 0
        logging.info( "Checking the variables in experiment.wrf4g file"  )
        
    def experiment_name(self) :
        """
        Check the experiment name 
        """
        if self.cfg[ 'default' ].get( 'name' ) :
            validate_name( self.cfg[ 'default' ][ 'name' ] )
        else :
            logging.error( "ERROR: 'name' variable is mandatory" )
            self.total_errors += 1
  
    def yesno_vars(self):
        """
        Check if yes/no variables are right 
        """
        for section in list( self.cfg.keys( ) ) :
            for key in YES_NO_VARIABLES :
                if self.cfg[ section ].get( key ) : 
                    val = self.cfg[ section ][ key ].lower()
                    if val in ( 'y', 'yes' ) :
                        self.cfg_final[ section ][ key ] = 'yes'
                    elif val in ( 'n', 'no' ) :
                        self.cfg_final[ section ][ key ] = 'no'            
                    else :
                        logging.error( "ERROR: '%s' variable should be 'yes' or 'no'" % key ) 
                        self.total_errors += 1

    def calendar(self):
        """
        Check calendar type
        """
        for section in list( self.cfg.keys( ) ) :
            if self.cfg[ section ].get( 'calendar' ) :
                if not self.cfg[ section ][ 'calendar' ] in Calendar.available_types :
                    logging.error( "'%s' calendar type is not avariable" % self.cfg[ default ][ 'calendar' ] )
                    self.total_errors += 1
    
    def dates(self):
        """
        Check strart and end dates
        """
        for section in list( self.cfg.keys( ) ) :
            if self.cfg[ section ].get( 'date_time' ) :
                self.cfg_final[ section ][ 'date_time' ] = []
                for rea_dates in self.cfg[ section ][ 'date_time' ].split( '\n' ) :
                    # Delete whitespaces and obtain each element
                    elems = rea_dates.replace( ' ', '' ).split( '|' )
                    if len( elems ) != 5 and len( elems ) != 3 :
                        raise Exception( "ERROR: Number of elements in '%s' is wrong" % rea_dates ) 
                    #  date_time specification 
                    #  start_date and  end_date  
                    start_date = datewrf2datetime( elems[ 0 ] )
                    end_date   = datewrf2datetime( elems[ 1 ] )
                    if start_date >= end_date :
                        logging.error( "ERROR: '%s' is not earlier than the '%s'" % ( elems[ 0 ] , elems[ 1 ] )  )
                        self.total_errors += 1
                    # chunk_size_h
                    chunk_size_h = int( elems[ 2 ] )
                    # simulation_interval_h and simulation_length_h 
                    if len( elems ) == 5 :
                        simult_interval_h = int( elems[ 3 ] )
                        simult_length_h   = int( elems[ 4 ] )
                    else :
                        td = end_date - start_date
                        total_seconds = ( td.microseconds + ( td.seconds + td.days * 24 * 3600) * 10**6 ) / 10**6
                        simult_interval_h = simult_length_h = total_seconds / 3600
                    if chunk_size_h > simult_length_h :
                        logging.warn( "WARNING: %d 'chunk_size' is bigger than %d 'simulation_length'" %
                                                 ( chunk_size_h, simult_length_h ) )
                    # Defining restart_interval
                    # To avoid chunk restart we add 1 hour to restart_interval variable
                    if self.cfg[ section ].get( 'chunk_restart' ) or self.cfg[ section ].get( 'chunk_restart' ) == 'no' :
                        restart_interval = ( chunk_size_h + 1 ) * 60 
                    else :
                        restart_interval = chunk_size_h * 60  
                    self.cfg_final[ section ][ 'date_time' ].append( [ start_date, end_date, 
                                                         simult_interval_h, simult_length_h, 
                                                         chunk_size_h, restart_interval ] )

    def parallel_env(self) : 
        """
        Check parallel enviroment
        """
        for section in list( self.cfg.keys( ) ) :
            if self.cfg[ section ].get( 'parallel_env' ) :
                if self.cfg[ section ][ 'parallel_env' ] not in ParallelEnvironment.launcher_map :
                    logging.error( "ERROR: '%s' does not exist" % self.cfg[ section ][ 'parallel_env' ] )
                    self.total_errors += 1

    def app(self) :
        """
        Check if app variable has been configure correctly
        """
        for section in list(self.cfg.keys( ) ) :
            if self.cfg[ section ].get( 'app' ) :
                for app in self.cfg[ section ].get( 'app' ).split('\n') :
                    try :
                        app_tag, app_type, app_value = app.split( '|' )
                    except ValueError:
                        raise Exception( "ERROR: 'app' variable in section '%s' is wrong." % section )
                    app_type = app_type.strip()
                    if app_type not in ( 'bundle', 'command' ) :
                        logging.error( "ERROR: '%s' app type does not exist in section '%s'." % ( app_type, section ) )
                        self.total_errors += 1

    def namelist(self) :
        """
        Check namelist configuration
        """
        for section in list(self.cfg.keys( ) ) :
            if self.cfg[ section ].get( 'namelist_values' ) :
                self.cfg_final[ section ] [ 'namelist_values' ] = []
                # Delete whitespaces
                for nml_val in self.cfg[ section ][ 'namelist_values' ].\
                               replace(' ', '').replace('\t', '').split( '\n' ):
                    if nml_val.startswith('#'): 
                        continue
                    nml_conf_key, nml_conf_val = nml_val.split( '|' )
                    self.cfg_final[ section ][ 'namelist_values' ].\
                       append( [ nml_conf_key, nml_conf_val.strip( ',' ).split( ',' ) ] )
        
def save_json( obj_config, directory, file_name ) :
    """
    Save a python object into a json file.
    """
    def date_handler(obj):
        return obj.isoformat() if hasattr(obj, 'isoformat') else obj

    f = open( join( directory, file_name ), "w" )
    try :
        json.dump( obj_config, f , default=date_handler )
    finally :
        f.close()

def load_json( directory, file_name ) :
    """
    Load a python object back from the json file.
    """
    f = open( join( directory, file_name ), "r" )
    try :
        return json.load( f )
    finally :
        f.close()

