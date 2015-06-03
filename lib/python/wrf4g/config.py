import os
import re
import pickle
import logging
from os.path          import expandvars, expanduser, exists, join
from wrf4g.utils.time import datewrf2datetime, Calendar
from wrf4g.utils.file import VarEnv, make_writeable, validate_name

__version__  = '2.0.0'
__author__   = 'Carlos Blanco'
__revision__ = "$Id$"

mandatory_varibles = ( 'name', 'max_dom', 'start_date', 'end_date', 
                       'namelist_version', 'domain_path', 'extdata_vtable', 
                       'extdata_path', 'extdata_interval', 'preprocessor', 
                       'output_path', 'postprocessor', 'app_bundles'          
                     )

yes_no_variables   = ( 'clean_after_run', 'save_wps', 'real_parallel', 
                       'wrf_parallel' , 'wrfout_name_end_date'
                     )


default_dict       = { 
                    'description'          : '',
                    'calendar'             : 'standard',
                    'timestep_dxfactor'    : 6,
                    'np'                   : 1,
                    'requirements'         : '',
                    'environment'          : '',
                    'clean_after_run'      : 'yes',
                    'save_wps'             : 'no',
                    'real_parallel'        : 'no',
                    'wrf_parallel'         : 'yes',
                    'wrfout_name_end_date' : 'yes',
                    'app_source_script'    : '',
                    'label_combination'    : [''],
                    'namelist_dict'        : dict()
                    }

class dict2obj(dict):
    """
    Class to convert a dictionary to an object
    """
    def __init__(self, dictionary, default=None):
        self.__dictionary = dictionary
        self.__default = default
        super(self.__class__, self).__init__(dictionary)
 
    def __getattr__(self, key ):
        if key.startswith('__'):
            raise AttributeError
        if key in self.__dictionary :
            val = self.__dictionary[ key ]
            if isinstance(val, dict):
                val = self.__class__( val )
            setattr(self, key, val )
            return val
        return self.__default

    def __missing__(self, key):
        return False 

def get_conf( directory = './' ):
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
    make_writeable( exp_file )
    logging.debug( "Reading '%s' file" % exp_file )
    exp_env = VarEnv( exp_file )
    default_dict.update( dict ( exp_env.defaults() ) )
    exp_conf_dict = dict()
    exp_conf_dict[ 'default' ] = default_dict
    exp_conf_dict[ 'default' ] [ 'home_dir' ] = os.getcwd() if directory == './' else directory
    for section in exp_env.sections() :
        if ':' in section :
            # In this case key will be the name of the reosurce
            key = section.split( ':' )[1]
        else :
            key = section 
        exp_conf_dict[ key ] = dict( exp_env.items( section ) )
    return sanity_check( dict2obj( exp_conf_dict ) )

def sanity_check( exp_conf ) :
    """
    Chenck if experiment variables are written well
    """
    # Check if all mandatory variables are avaible
    default_keys = exp_conf.default.keys()
    for key in mandatory_varibles :
        if key not in default_keys :
            raise Exception( "'%s' is a mandatory variable." 
                            "Please specify it in the experiment.wrf4g file" % key ) 
    # Check the experiment name 
    validate_name( exp_conf.default.name )
    # Convert max_dom
    exp_conf.default.max_dom = int( exp_conf.default.max_dom )
    # Convert np
    exp_conf.default.np = int( exp_conf.default.np )
    # Convert extdata_interval
    exp_conf.default.extdata_interval = int( exp_conf.default.extdata_interval )
    # Check if yes/no variables are right 
    for key in yes_no_variables :
        val = exp_conf.default[ key ].lower()
        if val in ( 'y', 'yes' ) :
            exp_conf.default[ key ] = 'yes'
        elif val in ( 'n', 'no' ) :
            exp_conf.default[ key ] = 'no'            
        else :
            raise Exception( "'%s' variable should be 'yes' or 'no'" % key ) 
    # Check calendar type
    if not exp_conf.default.calendar in Calendar.available_types :
        raise Exception( "'%s' calendar type is not avariable" % exp_conf.default.calendar )
    # Check strart and end dates
    sdate  = datewrf2datetime( exp_conf.default.start_date )
    edate  = datewrf2datetime( exp_conf.default.end_date )
    if sdate >= edate :
        raise Exception( "'start_date' is not earlier than 'end_date'" )
    exp_conf.default.start_date = sdate
    exp_conf.default.end_date   = edate
    # Check chunk_size_h
    if not exp_conf.default.chunk_size_h :
        chunk_size = ( sdate - edate ).total_seconds() / 3600
        logging.warning( "'chunk_size_h' will be %d hours" %  chunk_size ) 
        exp_conf.default.chunk_size_h = chunk_size
    else :
        exp_conf.default.chunk_size_h = int( exp_conf.default.chunk_size_h )
    # Check multiple dates
    if exp_conf.default.simulation_length_h and exp_conf.default.simulation_interval_h :
        exp_conf.default.simulation_length_h   = int( exp_conf.default.simulation_length_h )
        exp_conf.default.simulation_interval_h = int( exp_conf.default.simulation_interval_h )
    # Check namelist 
    if exp_conf.default.namelist :
        # Delete whitespace
        exp_conf.default.namelist = exp_conf.default.namelist.replace(' ', '')
        for nml_val in exp_conf.default.namelist.split( '\n' ):
            nml_conf = nml_val.split( '|' )
            nml_conf_key = nml_conf[ 0 ]
            nml_conf_val = nml_conf[ 1: ]
            if 'label_combination' in nml_conf_key :
                exp_conf.default.label_combination = nml_conf_val
            else :
                values = []
                for nml_elem in nml_conf_val :
                    nml_elem_val = nml_elem.strip( ',' ).split( ',' )
                    if len( nml_elem_val ) > exp_conf.default.max_dom and \
                            not nml_conf_key.startswith( 'single' ) : 
                        logging.warning( "WARNING: Truncating values of '%s' variable" % nml_conf_key )
                        nml_elem_val = nml_elem_val[ :exp_conf.default.max_dom ]
                    elif len( nml_elem_val ) < exp_conf.default.max_dom and \
                            not nml_conf_key.startswith( 'single' ) : 
                        logging.warning( "WARNING: Expanding values of '%s' variable" % nml_conf_key )
                        nml_elem_val = nml_elem_val + [ ( nml_elem_val[ -1 ] * ( exp_conf.default.max_dom - len( nml_elem_val ) ) ) ]
                    elif nml_conf_key.startswith( 'single:' ) :
                        nml_conf_key = nml_conf_key.replace( 'single:', '' )
                    values.append( ' '.join( nml_elem_val ) )
                print values 
                exp_conf.default.namelist_dict[ nml_conf_key ] = values
    print exp_conf.default.namelist_dict
    # Check restart_interval
    if not exp_conf.default.namelist or not 'restart_interval' in exp_conf.default.namelist :
        exp_conf.default.namelist_dict[ 'restart_interval' ] = [ exp_conf.default.chunk_size_h * 60 ] * \
                                                                 len( exp_conf.default.label_combination  ) 
    return exp_conf

def save_exp_pkl( obj_config, directory ) :
    """
    Save experiment into a pickle file.
    """
    f = open( join( directory, "experiment.pkl"), "wb" )
    try :
        pickle.dump( obj_config, f )
    finally :
        f.close()

def load_exp_pkl( directory ) :
    """
    Load the experiment back from the pickle file.
    """
    f = open( join( directory, "experiment.pkl"), "rb" )
    try :
        return pickle.load( f )
    finally :
        f.close()

