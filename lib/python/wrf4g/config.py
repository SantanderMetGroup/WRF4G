import os
import re
import pickle
import logging
from os.path          import expandvars, expanduser, exists, join, abspath
from wrf4g.utils      import dict2obj
from wrf4g.utils.mpi  import ParallelEnvironment 
from wrf4g.utils.time import datewrf2datetime, Calendar
from wrf4g.utils.file import VarEnv, make_writeable, validate_name

__version__  = '2.0.0'
__author__   = 'Carlos Blanco'
__revision__ = "$Id$"

MANDATORY_VARIABLES = ('name', 'max_dom', 'date_time', 'namelist_version', 
                       'domain_path', 'extdata_vtable', 'extdata_path', 
                       'extdata_interval', 'preprocessor', 'output_path', 
                       'postprocessor', 'parallel_environment'          
                     )

YES_NO_VARIABLES    = ('clean_after_run', 'save_wps', 'parallel_real', 
                       'parallel_wrf' , 'wrfout_name_end_date', 'chunk_restart'
                     )


DEFAULT_DICT        = { 
                    'description'          : '',
                    'calendar'             : 'standard',
                    'timestep_dxfactor'    : '6',
                    'np'                   : 1,
                    'requirements'         : '',
                    'environment'          : '',
                    'constants_name'       : '',
                    'clean_after_run'      : 'yes',
                    'save_wps'             : 'no',
                    'parallel_real'        : 'no',
                    'parallel_wrf'         : 'yes',
                    'wrfout_name_end_date' : 'no',
                    'chunk_restart'        : 'yes',
                    'namelist_dict'        : dict()
                    }

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
    DEFAULT_DICT.update( dict ( exp_env.items( 'DEFAULT' ) ) )
    exp_conf_dict = dict()
    exp_conf_dict[ 'default' ] = DEFAULT_DICT
    exp_conf_dict[ 'default' ] [ 'home_dir' ] = abspath( directory )
    for section in exp_env.sections() :
        if ':' in section :
            # In this case key will be the name of the reosurce
            key = section.split( ':' )[1]
        else :
            key = section 
        exp_conf_dict[ key ] = dict( exp_env.items( section ) )
    return sanity_check( dict2obj( exp_conf_dict ) )

class SanityCheck():
    """
    Chenck if experiment variables are written well
    """

    def __init__(self, exp_conf) :
      
        self.exp_cfg       = exp_cfg
        self.exp_cfg_final = exp_cfg
        self.total_errors  = 0
        logging.info( "Checking the variables in experiment.wrf4g file"  )

    def mandatory_variables(self) :
        """
        Check if all mandatory variables are avaible
        """
        default_keys = list( self.exp_cfg.default.keys() )
        for key in MANDATORY_VARIABLES :
            if key not in default_keys :
                logging.warn( "ERROR: '%s' is a mandatory variable." 
                              "Please specify it in the experiment.wrf4g file" % key ) 
                self.total_errors += 1
        
    def experiment_name(self) :
        """
        Check the experiment name 
        """
        validate_name( self.exp_cfg.default.name )
  
    def convert_max_dom2int(self) :
        """
        Convert max_dom
        """
        self.exp_cfg_final.default.max_dom = int( self.exp_cfg.default.max_dom )
    
    def convert_np2int(self) :
        """
        Convert np
        """
        self.exp_cfg_final.default.np      = int( self.exp_cfg.default.np )

    def yesno_vars(self):
        """
        Check if yes/no variables are right 
        """
        for key in YES_NO_VARIABLES :
            val = self.exp_cfg.default[ key ].lower()
            if val in ( 'y', 'yes' ) :
                self.exp_cfg_final.default[ key ] = 'yes'
            elif val in ( 'n', 'no' ) :
                self.exp_cfg_final.default[ key ] = 'no'            
            else :
                logging.error( "ERROR: '%s' variable should be 'yes' or 'no'" % key ) 
                self.total_errors += 1

    def calendar(self):
        """
        Check calendar type
        """
        if not self.exp_cfg.default.calendar in Calendar.available_types :
            logging.error( "'%s' calendar type is not avariable" % self.exp_cfg.default.calendar )
            self.total_errors += 1
    
    def dates(self):
        """
        Check strart and end dates
        """
        self.exp_cfg_final.default.datetime_list = []
        for rea_dates in self.exp_cfg.default.date_time.split( '\n' ) :
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
            if self.exp_cfg.default.chunk_restart == 'no' :
                restart_interval = ( chunk_size_h + 1 ) * 60 
            else :
                restart_interval = chunk_size_h * 60  
            self.exp_cfg_final.default.datetime_list.append( [ start_date, end_date, 
                                                 simult_interval_h, simult_length_h, 
                                                 chunk_size_h, restart_interval ] )

    def parallel_env(self) : 
        """
        Check parallel enviroment
        """
        if self.exp_cfg.default.parallel_environment not in ParallelEnvironment.launcher_map :
            logging.error( "ERROR: '%s' does not exist" % self.exp_cfg.default.parallel_environment )
            self.total_errors += 1

    def app(self) :
        """
        Check if app variable has been configure correctly
        """
        for section in list(self.exp_cfg.keys( ) ) :
            if self.exp_cfg[ section ].get( 'app' ) :
                for app in self.exp_cfg[ section ].get( 'app' ).split('\n') :
                    try :
                        app_tag, app_type, app_value = app.split( '|' )
                    except ValueError:
                        raise Exception( "ERROR: 'app' variable in section '%s' is wrong." % section )
                    app_type = app_type.strip()
                    if app_type not in ( 'bundle', 'command' ) :
                        logging.error( "ERROR: '%s' app type does not exist in section '%s'." % ( app_type, section ) )
                        self.total_errors += 1

    def member(self) :
        """
        Check if there are multible members 
        """
        # member tag | member number | initial month number
        if self.exp_cfg.default.extdata_member :
            exp_conf.default.extdata_member_list = [ member for member in  exp_conf.default.extdata_member.replace(' ', '').split( '\n' ) ]
        else :
            exp_conf.default.extdata_member_list = [ '' ]

    ##
    # Check namelist configuration for multicombinations
    ##
    if exp_conf.default.namelist_label_comb :
        exp_conf.default.namelist_label_comb = exp_conf.default.namelist_label_comb.\
                                               replace(' ', '').split( '|' )
    else :
        exp_conf.default.namelist_label_comb = [ '' ]
    if exp_conf.default.namelist_values :
        # Delete whitespaces
        for nml_val in exp_conf.default.namelist_values.replace(' ', '').replace('\t', '').split( '\n' ):
            if nml_val.startswith('#'): continue
            nml_conf = nml_val.split( '|' )
            nml_conf_key = nml_conf[ 0 ]
            nml_conf_val = nml_conf[ 1: ]
            values = []
            for nml_elem in nml_conf_val :
                values.append( nml_elem.strip( ',' ).split( ',' ) )
            exp_conf.default.namelist_dict[ nml_conf_key ] = values
    if exp_conf.default.namelist_label_comb :
        exp_conf.default.namelist_label_comb = exp_conf.default.namelist_label_comb.\
                                               replace(' ', '').split( '|' )
    else :
        exp_conf.default.namelist_label_comb = [ '' ]
    ensemble
    if total_errors :
        raise Exception( "Please review your experiment configuration" )
    return exp_conf

def save_exp_pkl( obj_config, directory ) :
    """
    Save experiment into a pickle file.
    """
    f = open( join( directory, "experiment.pkl"), "w" )
    try :
        pickle.dump( obj_config, f )
    finally :
        f.close()

def load_exp_pkl( directory ) :
    """
    Load the experiment back from the pickle file.
    """
    f = open( join( directory, "experiment.pkl"), "r" )
    try :
        return pickle.load( f )
    finally :
        f.close()

