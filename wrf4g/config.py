#
# Copyright 2016 Universidad de Cantabria
#
# Licensed under the EUPL, Version 1.1 only (the
# "Licence");
# You may not use this work except in compliance with the
# Licence.
# You may obtain a copy of the Licence at:
#
# http://ec.europa.eu/idabc/eupl
#
# Unless required by applicable law or agreed to in
# writing, software distributed under the Licence is
# distributed on an "AS IS" basis,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either
# express or implied.
# See the Licence for the specific language governing
# permissions and limitations under the Licence.
#

import os
import re
import copy
import json
import pickle
import logging
from os.path          import expandvars, expanduser, exists, join, abspath
from wrf4g.utils      import dict2obj
from wrf4g.utils.mpi  import ParallelEnvironment 
from wrf4g.utils.time import ( datewrf2datetime, Calendar, 
                               str2timedelta, timedelta_total_seconds )
from wrf4g.utils.file import VarEnv, make_writeable, validate_name

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
    exp_cfg_dict = dict()
    exp_cfg_dict[ 'default' ] = exp_env.defaults()
    section_startswith_ensemble = 0  
    for section in exp_env.sections() :
        if section.startswith( 'ensemble' ) :
            exp_cfg_dict[ section ] = dict( exp_env.items( section ) )
            section_startswith_ensemble += 1
    if not section_startswith_ensemble :
        exp_cfg_dict[ 'ensemble/default' ] = exp_env.defaults()
    # Checking experinment.wrf4g file    
    sanity_check = SanityCheck( exp_cfg_dict )
    sanity_check.experiment_name()
    sanity_check.yes_no_vars()
    sanity_check.calendar()
    sanity_check.log_level()
    sanity_check.dates()
    sanity_check.parallel_env() 
    sanity_check.files_to_save()
    sanity_check.app()
    sanity_check.namelist_template()
    sanity_check.ensembles()
    if sanity_check.total_errors :
        raise Exception( "Please review your experiment configuration" )
    return sanity_check.cfg_final

class SanityCheck():
    """
    Chenck if experiment variables are written well
    """

    def __init__(self, cfg) :
        self.cfg           = cfg
        self.cfg_final     = copy.deepcopy( cfg )
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
  
    _YES_NO_VARIABLES = ( 'clean_after_run', 'save_wps', 'parallel_real','parallel_metgrid'
                          'parallel_wrf' , 'wrfout_name_end_date', 'chunk_restart' )

    def yes_no_vars(self):
        """
        Check if yes/no variables are right 
        """
        for section in list( self.cfg.keys( ) ) :
            for key in self._YES_NO_VARIABLES :
                if self.cfg[ section ].get( key ) and section.startswith( 'ensemble/' ) : 
                    val = self.cfg[ section ][ key ].lower()
                    if val in ( 'y', 'yes' ) :
                        self.cfg_final[ section ][ key ] = 'yes'
                    elif val in ( 'n', 'no' ) :
                        self.cfg_final[ section ][ key ] = 'no'            
                    else :
                        logging.error( "ERROR: '%s' variable should be 'yes' or 'no'" % key ) 
                        self.total_errors += 1

    def log_level(self):
        """
        Check if log_level has a right value 
        """
        for section in list( self.cfg.keys( ) ) :
            if self.cfg[ section ].get( 'log_level' ) and section.startswith( 'ensemble/' ) :
                if not self.cfg[ section ].get( 'log_level' ) in [ 'ERROR', 'WARNING', 'INFO', 'DEBUG' ] :
                    logging.error( "log_level variable has to be ERROR, WARNING, INFO or DEBUG" )
                    self.total_errors += 1

    def calendar(self):
        """
        Check calendar type
        """
        for section in list( self.cfg.keys( ) ) :
            if self.cfg[ section ].get( 'calendar' ) and section.startswith( 'ensemble/' ) :
                if not self.cfg[ section ][ 'calendar' ] in Calendar.available_types :
                    logging.error( "'%s' calendar type is not available" % self.cfg[ default ][ 'calendar' ] )
                    self.total_errors += 1
    
    def dates(self):
        """
        Check start and end dates
        """
        for section in list( self.cfg.keys( ) ) :
            if self.cfg[ section ].get( 'date_time' ) and section.startswith( 'ensemble/' ) :
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
                    # chunk_size
                    chunk_size = str2timedelta( elems[ 2 ] )
                    # simulation_interval and simulation_length
                    if len( elems ) == 5 :
                        simult_interval = str2timedelta( elems[ 3 ] )
                        simult_length   = str2timedelta( elems[ 4 ] )
                    else :
                        simult_interval = simult_length = end_date - start_date
                    # Defining restart_interval
                    # To avoid chunk restart we add 1 hour to restart_interval variable
                    restart_interval = int(timedelta_total_seconds( chunk_size ) / 60)
                    if self.cfg[ section ].get( 'chunk_restart' ) or self.cfg[ section ].get( 'chunk_restart' ) == 'no' :
                        restart_interval = restart_interval + 3600
                    self.cfg_final[ section ][ 'date_time' ].append( [ start_date, end_date, 
                                                         simult_interval, simult_length, 
                                                         chunk_size, restart_interval ] )

    def parallel_env(self) : 
        """
        Check parallel enviroment
        """
        for section in list( self.cfg.keys( ) ) :
            if self.cfg[ section ].get( 'parallel_env' ) :
                if self.cfg[ section ][ 'parallel_env' ] not in ParallelEnvironment.launcher_map :
                    logging.error( "ERROR: '%s' does not exist" % self.cfg[ section ][ 'parallel_env' ] )
                    self.total_errors += 1
                if 'DUMMY' in self.cfg[ section ][ 'parallel_env' ] :
                    if ( not 'parallel_run' in self.cfg[ section ] ) or \
                       ( not 'parallel_run_pernode' in self.cfg[ section ] ) :
                        logging.error( "ERROR: TO use DUMMY value you have to define "
                                       "'parallel_run' and 'parallel_run_pernode' variables" )
                        self.total_errors += 1  
 

    def namelist_template(self) :
        """
        Check the namelist template
        """
        for section in list( self.cfg.keys( ) ) :
            if self.cfg[section].get('namelist_template') or self.cfg[section].get('namelist_version'):
                if self.cfg[section].get('namelist_version'):
                    logging.warn('namelist_version variable is deprecated. Use namelist_template instead')
                    self.cfg[section]['namelist_template']=self.cfg[section].get('namelist_version')
                    self.cfg[section].pop('namelist_version')
                
            else :
                logging.error( "ERROR: 'namelist_template' variable is mandatory" )
                self.total_errors += 1

    _files_to_save = ( 'wrfout', 'wrfrst')

    def files_to_save(self) :
        """
        Check files to save during simulation        
        """
        for section in list( self.cfg.keys( ) ) :
            if self.cfg[ section ].get( 'files_to_save' ) :
                self.cfg_final[ section ] [ 'files_to_save' ] = self.cfg[ section ].get( 'files_to_save' ).\
                                            replace(' ', '').split( ',' )
            else :
                self.cfg_final[ section ] [ 'files_to_save' ] = self._files_to_save

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

    def ensembles(self) :
        """
        Create ensembles for multi preprocessor_optargs and multi namelist_values        
        """
        for key_value, tag in [ ( 'preprocessor_optargs', 'arg' ) ,
                                ( 'namelist_values', 'nml' )  ] :
            self.cfg = copy.deepcopy( self.cfg_final )
            for section in list( self.cfg.keys( ) ) :
                if self.cfg[ section ].get( key_value ) and section.startswith( 'ensemble/' ) :
                    no_comb = max( [ line.count( '|' ) for line in self.cfg[ section ][ key_value ].split( '\n' ) ] )
                    if no_comb > 1 :
                        section_names = [ section + '-' + tag + str( elem ) for elem in range( 1, no_comb + 1 ) ]
                        for new_section in section_names :
                            self.cfg_final[ new_section ] = copy.deepcopy( self.cfg[ section ] )
                            self.cfg_final[ new_section ] [ key_value ] = dict()
                    else :
                        section_names = [ section ]
                        self.cfg_final[ section ] [ key_value ] = dict()
                    # Delete whitespaces
                    for line in self.cfg[ section ][ key_value ].replace(' ', '').replace('\t', '').split( '\n' ):
                        if line.startswith('#') : continue
                        key  = line.split( '|' ) [ 0 ]
                        vals = line.split( '|' ) [ 1: ]
                        if len( vals ) != no_comb :
                            [ vals.append( vals[-1] ) for elem in range( no_comb - len( vals ) ) ]
                            logging.warn( "WARNING: Extending '%s' with this value '%s'" % ( key, vals[-1] ) )
                        for i, new_section in enumerate( section_names ) :
                            self.cfg_final[ new_section ][ key_value ] [ key ] = vals[i]
                    if no_comb > 1 : del self.cfg_final[ section ]
       
def save_pkl( obj_config, directory, file_name ) :
    """
    Save a python object into a pickle file.
    """
    with open( join( directory, file_name ), "wb" ) as f :
        pickle.dump( obj_config, f )

def load_pkl( directory, file_name ) :
    """
    Load a python object back from the pickle file.
    """
    with open( join( directory, file_name ), "r" ) as f :
        return pickle.load( f )

def save_json( obj_config, directory, file_name ) :
    """
    Save a python object into a json file.
    """
    # datetime objects have to be converted 
    from datetime               import datetime, date, timedelta
    from dateutil.relativedelta import relativedelta
    class DatetimeEncoder(json.JSONEncoder):
        def default(self, obj):
            if isinstance(obj, datetime):
                return obj.strftime('%s')
            elif isinstance(obj, date):
                return obj.strftime('%s')
            elif isinstance(obj, timedelta):
                return str(obj)
            elif isinstance(obj, relativedelta):
                return str(obj)
            return json.JSONEncoder.default(self, obj)

    with open( join( directory, file_name ), "w" ) as f :
        json.dump( obj_config, f , cls = DatetimeEncoder)

def load_json( directory, file_name ) :
    """
    Load a python object back from the json file.
    """
    with open( join( directory, file_name ), "r" ) as f :
        return json.load( f )

