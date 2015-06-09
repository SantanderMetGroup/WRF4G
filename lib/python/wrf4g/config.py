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

mandatory_varibles = ( 'name', 'max_dom', 'date_time', 'namelist_version', 
                       'domain_path', 'extdata_vtable', 'extdata_path', 
                       'extdata_interval', 'preprocessor', 'output_path', 
                       'postprocessor', 'app'          
                     )

yes_no_variables   = ( 'clean_after_run', 'save_wps', 'real_parallel', 
                       'wrf_parallel' , 'wrfout_name_end_date'
                     )


default_dict       = { 
                    'description'          : '',
                    'calendar'             : 'standard',
                    'timestep_dxfactor'    : '6',
                    'np'                   : 1,
                    'requirements'         : '',
                    'environment'          : '',
                    'clean_after_run'      : 'yes',
                    'save_wps'             : 'no',
                    'real_parallel'        : 'no',
                    'wrf_parallel'         : 'yes',
                    'wrfout_name_end_date' : 'yes',
                    'app_source_script'    : '',
                    'namelist_dict'        : dict()
                    }

def dict_compare(dict_one, dict_two ):
    """ 
    Compare two dictionaries
    """
    dict_one_keys  = set( dict_one.keys() )
    dict_two_keys  = set( dict_two.keys() )
    intersect_keys = dict_one_keys.intersection( dict_two_keys  )
    added          = dict_one_keys - dict_two_keys
    removed        = dict_two_keys - dict_one_keys
    modified       = [ k for k in intersect_keys if dict_one[ k ] != dict_two[ k ] ]
    same           = set( k for k in intersect_keys if dict_one[ k ] == dict_two[ k ] )
    return ( added, removed, modified, same )

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

    ##
    # Check if yes/no variables are right 
    ##
    for key in yes_no_variables :
        val = exp_conf.default[ key ].lower()
        if val in ( 'y', 'yes' ) :
            exp_conf.default[ key ] = 'yes'
        elif val in ( 'n', 'no' ) :
            exp_conf.default[ key ] = 'no'            
        else :
            raise Exception( "ERROR: '%s' variable should be 'yes' or 'no'" % key ) 
    # Check calendar type
    if not exp_conf.default.calendar in Calendar.available_types :
        raise Exception( "'%s' calendar type is not avariable" % exp_conf.default.calendar )
    ##
    # Check strart and end dates
    ##
    exp_conf.default.datetime_list = []
    for rea_dates in exp_conf.default.date_time.split( '\n' ) :
        # Delete whitespaces and obtain each element
        elems = rea_dates.replace( ' ', '' ).split( '|' )
        if len( elems ) > 5 or len( elems ) < 2 :
            raise Exception( "ERROR: Number of elements in '%s' is wrong" % rea_dates ) 
        #  date_time specification 
        #  start_date  |  end_date  |  interval_h | length_h  | chunk_size_h
        start_date = datewrf2datetime( elems[ 0 ] )
        end_date   = datewrf2datetime( elems[ 1 ] )
        if start_date >= end_date :
            raise Exception( "ERROR: 'start_date' is not earlier than the 'end_date'" )
        ##
        # If there are four elements chunk_size_h = simult_length_h
        # If there are three elements simult_interval_h = simult_length_h
        # If there are two elements chunk_size_h = simult_interval_h = simult_length_h
        ##
        elif len( elems ) in ( 4, 5 ) :
            simult_interval_h = int( elems[ 2 ] )
            simult_length_h   = int( elems[ 3 ] )
            if len( elems ) == 5 :
                chunk_size_h = int( elems[ 4 ] )
                if chunk_size_h > simult_length_h :
                    logging.warning( "WARNING: 'chunk_size_h' is bigger than 'simulation_length_h'" )
                    chunk_size_h = simult_length_h
            else :
                chunk_size_h  = None 
        else :
            simult_interval_h = simult_length_h = ( end_date - start_date ).total_seconds() / 3600 
            if len( elems ) == 3 :
                chunk_size_h = int( elems[ 2 ] )
            else :
                chunk_size_h = None
        if not chunk_size_h :
            chunk_size_h  = simult_length_h
            logging.warning( "WARNING: 'chunk_size_h' will be %d hours" %  chunk_size_h )
        # Defining restart_interval
        restart_interval = chunk_size_h * 60  
        exp_conf.default.datetime_list.append( [ start_date, end_date, 
                                                 simult_interval_h, simult_length_h, 
                                                 chunk_size_h, restart_interval ] )
    ##
    # Check namelist configuration for multicombinations
    ##
    exp_conf.default.label_combination = [ '' ] 
    if exp_conf.default.namelist :
        # Delete whitespaces
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
                    if nml_conf_key.startswith( 'single:' ) or nml_conf_key.startswith( 'single_list:' ):
                        nml_conf_key = nml_conf_key.replace( 'single:', '' ) 
                    elif len( nml_elem_val ) > exp_conf.default.max_dom or \
                            nml_conf_key.startswith( 'max_dom' ) : 
                        nml_elem_val = nml_elem_val[ :exp_conf.default.max_dom ]
                        logging.warning( "WARNING: Truncating values of '%s' variable --> %s" % ( nml_conf_key, nml_elem_val ) )
                        nml_elem_val = nml_elem_val[ :exp_conf.default.max_dom ]
                    elif len( nml_elem_val ) < exp_conf.default.max_dom or \
                            nml_conf_key.startswith( 'max_dom' ) : 
                        nml_elem_val = nml_elem_val + [ ( nml_elem_val[ -1 ] * \
                            ( exp_conf.default.max_dom - len( nml_elem_val ) ) ) ]
                        logging.warning( "WARNING: Expanding values of '%s' variable --> %s" % ( nml_conf_key, nml_elem_val ) )
                    values.append( ' '.join( nml_elem_val ) )
                exp_conf.default.namelist_dict[ nml_conf_key ] = values
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

