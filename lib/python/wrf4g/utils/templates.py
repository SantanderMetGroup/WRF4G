import os
import re
import ConfigParser

__version__  = '2.0.0'
__author__   = 'Carlos Blanco'
__revision__ = "$Id$"

class VarEnv( object ):
    """
    Allow to load the available variables in a file 
    """
    
    def __init__(self, file):
        """
        'file' to read
        """
        self._cp = ConfigParser.ConfigParser()
        self._cp.optionxform=str
        self._cp.read( file )
        
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
        
    def set_variable( self , section , name , value ) :
        """
        Set an option
        """
        self._cp.set(section, option, value)
    
    def get_variable( self, var_name, default = '', section = 'DEFAULT') :
        """
        Get a value for given section. The default section will be 'DEFAULT'. 
        """        
        try :
            value = dict( self._cp.items( section ) )[ var_name ]
            if value.startswith( '"' ) and value.endswith( '"' ) :
                value = value[ 1 : -1 ]
            elif value.startswith( "'" ) and value.endswith( "'" ) :
                value = value[ 1 : -1 ]
            return value
        except ( KeyError , IOError ):
            return default

def make_writeable( filename ):
    """
    Make sure that the file is writeable.
    Useful if our source is read-only.
    """
    if not os.access(filename, os.W_OK):
        st = os.stat(filename)
        new_permissions = stat.S_IMODE(st.st_mode) | stat.S_IWUSR
        os.chmod(filename, new_permissions)

def validate_name( name ):
    # If it's not a valid directory name.
    if not re.search(r'^[_a-zA-Z]\w*$', name):
        # Provide a smart error message, depending on the error.
        if not search(r'^[_a-zA-Z]', name):
            message = 'make sure the name begins with a letter or underscore'
        else:
            message = 'use only numbers, letters and underscores'
        raise Exception ("%r is not a valid %s name. Please %s." % (name, message) )
 
def edit_file( file_name ):
    """
    Edit files. vi is used be default. If you want to use another editor,
    please edit EDITOR shell variable.
    """
    os.system( "%s %s" % ( os.environ.get('EDITOR', 'vi') , file_name ) )

def yes_no_choice( message ,  default = 'y' ) :
    """
    To ask for Yes/No questions
    """
    choices = 'Y/n' if default.lower() in ('y', 'yes') else 'y/N'
    choice = raw_input("%s (%s) " % (message, choices))
    values = ('y', 'yes', '') if default == 'y' else ('y', 'yes')
    return choice.strip().lower() in values

