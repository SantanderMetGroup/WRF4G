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
import logging
from os.path           import basename, dirname   
from wrf4g.utils.time  import ( datewrf2datetime, 
                                dateiso2datetime,
                                datetime2datewrf, 
                                datetime2dateiso )
try:
    from configparser import SafeConfigParser
except ImportError:
    from ConfigParser import SafeConfigParser  # ver. < 3.0


class VarEnv( object ):
    """
    Allow to load the available variables in a file 
    """
    
    def __init__(self, file):
        """
        'file' to read
        """
        self._cp = SafeConfigParser()
        self._cp.read( file )

    def defaults( self ):
        """
        Return a dictionary containing the instance-wide defaults.
        """
        return self._cp.defaults()       
  
    def has_section( self , section ):
        """
        Indicate whether the named section is present in the configuration.
        """
        return self._cp.has_section( section  )

    def items( self , section):
        """
        Return a list of tuples with (name, value) for each option in the section.
        """
        return self._cp.items( section )

    def sections( self ):
        """
        Return a list of section names, excluding [DEFAULT] section.
        """
        return self._cp.sections()
    
    def write( self , dest_file ):
        """
        Write an ini-format representation of the configuration
        """
        self._cp.write( dest_file )
        
    def set_var( self , section , name , value ) :
        """
        Set an option
        """
        self._cp.set(section, option, value)
    
    def get_var( self, var_name, default = '', section = 'DEFAULT' ) :
        """
        Get a value for given section. The default section will be 'DEFAULT'. 
        """        
        try :
            value = dict( self._cp.items( section ) )[ var_name.lower() ]
            if value.startswith( '"' ) and value.endswith( '"' ) :
                value = value[ 1 : -1 ]
            return value
        except ( KeyError , IOError ):
            return default

def make_writeable( file_name ):
    """
    Make sure that the file is writeable.
    Useful if our source is read-only.
    """
    if not os.access(file_name, os.W_OK):
        st = os.stat(file_name)
        new_permissions = stat.S_IMODE(st.st_mode) | stat.S_IWUSR
        os.chmod(file_name, new_permissions)

def validate_name( name ):
    # If it's not a valid directory name.
    if not re.search(r'^[_a-zA-Z]\w*$', name):
        # Provide a smart error message, depending on the error.
        if not re.search(r'^[_a-zA-Z]', name):
            message = 'make sure the name begins with a letter or underscore'
        else:
            message = 'use only numbers, letters and underscores'
        raise Exception ("%s is not a valid name. Please %s." % (name, message) )

def edit_file( file_name ):
    """
    Edit files. vi is used be default. If you want to use another editor,
    please edit EDITOR shell variable.
    """
    logging.debug( "Editing '%s' file" % file_name )    
    os.system( "%s %s" % ( os.environ.get('EDITOR', 'vi') , file_name ) )

class WRFFile( object ) :
    """
    This class manage the restart and output files and the dates they represent.
    It recieves a file name with one of the following shapes: wrfrst_d01_1991-01-01_12:00:00 or
    wrfrst_d01_19910101T120000Z and it return the date of the file, the name,...
    """

    def __init__(self, url, edate=None):
        """
        Change the name of the file in the repository (Change date to the iso format
        and add .nc at the end of the name
        """
        # wrfrst_d01_1991-01-01_12:00:00
        if edate:
            self.edate = datewrf2datetime(edate)
        g = re.search("(.*)(\d{4}-\d{2}-\d{2}_\d{2}[:_]\d{2}[:_]\d{2})", url)
        if g:
            base_file, date_file = g.groups()
            self.date = datewrf2datetime(date_file)
        else:
            # wrfrst_d01_19910101T120000Z.nc
            g = re.search("(.*)(\d{8}T\d{6}Z)", url)
            if not g:
                out="File name is not well formed"
                raise Exception(out)
            else :
                base_file, date_file = g.groups()
                self.date = dateiso2datetime(date_file)
        self.file_name = basename(base_file)
        self.dir_name = dirname(base_file)

    def date_datetime(self):
        return self.date

    def date_wrf(self):
        return datetime2datewrf(self.date)

    def date_iso(self):
        return datetime2dateiso(self.date)

    def file_name_wrf(self):
        return self.file_name + datetime2datewrf(self.date)

    def file_name_iso(self):
        return "%s%s.nc" % (self.file_name,datetime2dateiso(self.date))

    def file_name_out_iso(self):
        return "%s%s_%s.nc" % (self.file_name, datetime2dateiso(self.date), datetime2dateiso(self.edate))

