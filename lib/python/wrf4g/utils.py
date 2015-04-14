from __future__     import with_statement
from datetime       import datetime, timedelta
from os.path        import join, basename, dirname, exists, expandvars, isdir
from re             import search, match
from wrf4g          import WRF4G_DIR

import os
import sys
import time
import calendar
import socket
import ConfigParser

__version__  = '1.5.2'
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
    
    def get_variable( self , var_name , section = 'DEFAULT' , default = '') :
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

class wrffile( object ) :
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

        g = search("(.*)(\d{4}-\d{2}-\d{2}_\d{2}:\d{2}:\d{2})", url)
        if g:
            base_file, date_file = g.groups()
            self.date = datewrf2datetime(date_file)
        else :
            # wrfrst_d01_19910101T120000Z.nc
            g = search("(.*)(\d{8}T\d{6}Z)", url)
            if not g:
                out="File name is not well formed"
                raise Exception(out)
            else :
                base_file, date_file = g.groups()
                self.date = dateiso2datetime(date_file)
        self.file_name = basename(base_file)
        self.dir_name = dirname(base_file)

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

# UNCTIONS FOR MANAGE DATES 
def datewrf2datetime (datewrf):
    g = match("(\d{4})-(\d{2})-(\d{2})_(\d{2}):(\d{2}):(\d{2})", datewrf)
    if not g :
        raise Exception("Date is not well formed")
    date_tuple = g.groups()
    date_object = datetime(*tuple(map(int, date_tuple)))
    return date_object

def dateiso2datetime (dateiso):
    g = match("(\d{4})(\d{2})(\d{2})T(\d{2})(\d{2})(\d{2})Z", dateiso)
    if not g :
        raise Exception("Date is not well formed")
    date_tuple = g.groups()
    date_object = datetime(*tuple(map(int, date_tuple)))
    return date_object

def datetime2datewrf (date_object):
    return date_object.strftime("%Y-%m-%d_%H:%M:%S")

def datetime2dateiso (date_object):
    return date_object.strftime("%Y%m%dT%H%M%SZ")

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
    if not search(r'^[_a-zA-Z]\w*$', name):
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

def pairs2dict(pairs):
    d={}
    for p in pairs.split(','):
        s=p.split('=')
        d[s[0]]=s[1]
    return d

def list2fields(arr):
    fields=''
    for i in arr:
       fields="%s,%s" %(fields,i)
    fields=fields[1:]
    return fields

def create_hash():
    import random
    rand=random.randint(1,60000000)
    text=str(rand)
    return text

def process_is_runnig( pid ):
    """
    Check is a process is running given a file
    """
    try:
        with open( pid , 'r' ) as f:
            lines = f.readlines()
        os.kill( int( lines[0].strip() ) , 0 )
    except :
        return False
    else:
        return True

def exec_cmd( cmd , stdin=subprocess.PIPE, stdout=subprocess.PIPE,
              stderr=subprocess.STDOUT, env=os.environ ):
    """
    Execute shell commands
    """
    logger.debug( "Executing command ... " + cmd )
    cmd_to_exec = subprocess.Popen(  cmd ,
                                  shell=True ,
                                  stdin=subprocess.PIPE,
                                  stdout=subprocess.PIPE,
                                  stderr=subprocess.PIPE,
                                  env=env
                                  )
    out , err =  cmd_to_exec.communicate()
    return out , err

class DataBase( object ):
    """
    Class to manage MySQL database
    """

    def __init__( self, port=25000 ):
        self.port       = port
        self.file_pid   = join( WRF4G_DIR, 'var', 'mysql.pid' )
        self.mysql_sock = join( WRF4G_DIR, 'var', 'mysql.sock' )
        self.mysql_log  = join( WRF4G_DIR, 'var', 'log', 'mysql.log' )

    def _port_is_free( self ):
        sock = socket.socket( socket.AF_INET, socket.SOCK_STREAM )
        if sock.connect_ex( ( '127.0.0.1', int( self.port ) ) ) is 0 :
            return False
        else :
            return True

    def status( self ):
        if not exists( self.mysql_pid ) :
            logger.info( "WRF4G_DB (MySQL) has not started" )
        elif process_is_runnig( self.mysql_pid ) :
            logger.info( "WRF4G_DB (MySQL) is running" )
        else :
            logger.info( "WRF4G_DB (MySQL) is stopped" )

    def start( self ):
        logger.info( "Starting WRF4G_DB (MySQL) ... " )
        if not self._port_is_free() and not process_is_runnig( self.mysql_pid ):
            raise Exception( "WARNING: Another process is listening on port %s."
              "Change the port by executing 'wrf4g start --db-port=new_port'." % self.mysql_port  
              )
        elif not exists( self.mysql_pid ) or ( exists( self.mysql_pid ) and not process_is_runnig( self.mysql_pid ) ) :
            mysql_options = "--no-defaults --port=%s --socket=%s --log-error=%s --pid-file=%s" % ( self.mysql_port ,
                                                                                                     self.mysql_sock ,
                                                                                                     self.mysql_log ,
                                                                                                     self.mysql_pid
                                                                                                     )
            cmd =  "cd %s ; nohup ./bin/mysqld_safe %s &>/dev/null &" % ( MYSQL_DIR , mysql_options )
            exec_cmd( cmd )
            time.sleep( 1.0 )
            if not exists( self.mysql_pid ) or self._port_is_free() :
                logger.error( "ERROR: MySQL did not start, check '%s' for more information " % self.mysql_log )
            else :
                logger.info( "OK" )
        else :
            logger.warn( "WARNING: MySQL is already running" )

    def stop( self ):
        if not exists( self.mysql_pid ) :
            logger.info( "WRF4G_DB (MySQL) has not started" )
        else :
            logger.info( "Stopping WRF4G_DB (MySQL) ..." )
            if not exists( self.mysql_pid ) and not process_is_runnig( self.mysql_pid ) ) :
                logger.warn( "WARNING: MySQL is already stopped." )
            elif exists( self.mysql_pid ) and process_is_runnig( self.mysql_pid ) :
                with open( self.mysql_pid , 'r') as f:
                    pid = f.readline().strip()
                mysql_ppid, err = exec_cmd( "ps h -p %s -o ppid" % pid )
                if err :
                    raise Exception( err )
                try :
                    os.kill( int( mysql_ppid ), signal.SIGKILL )
                    os.kill( int( pid ), signal.SIGKILL )
                    logger.info( "OK" )
                except Exception , err :
                    logger.error( "ERROR: stopping MySQL: %s" % err )
            else :
                logger.warn( "WARNING: MySQL is already stopped." )

class Calendar( object ):
    """
    Class to manage calendars such as 'standard' and '365_day' 
    """
    def __init__( self, type = 'standard' ):
        if not type in [ 'standard', '365_day' ] :
            raise Exception( "Calendar type not available" )
        else :
            self.type = type

    def sub(self, date1, date2 ):
        """
        Subtract two dates returning a timedelta object
        """
        if self.type == '365_day' :
            diff_days = self._no_leap_day( date1 ) - self._no_leap_day( date2 )
            return timedelta( days=diff_days )
        else :
            return date1 - date2

    def add_hours(self, date, hours):
        """
        Add hours to a date returning a datetime object
        """
        if self.type == '365_day' and calendar.isleap( date.year ) :
            date_add = date + timedelta(hours=hours)
            day_29   = datetime( date.year, 2 , 29 )
            if ( date < day_29 ) and ( day_29 < date_add ) :
               hours_to_add = 24
            else :
               hours_to_add = 0
            return date + timedelta(hours=hours+hours_to_add)
        else :
            return date + timedelta(hours=hours)

    def sub_hours(self, date, hours):
        """
        Subtract hours to a date returning a datetime object
        """
        if self.type == '365_day' and calendar.isleap( date.year ) :
            date_sub = date - timedelta(hours=hours)
            day_29   = datetime( date.year, 2 , 29 )
            if ( date > day_29 ) and ( day_29 > date_sub ) :
               hours_to_add = 24
            else :
               hours_to_add = 0
            return date + timedelta(hours=hours-hours_to_add)
        else :
            return date + timedelta(hours=hours)

    def _no_leap_day(self, date):
        year        = date.year
        month       = date.month
        day         = date.day
        hour        = date.hour
        minute      = date.minute
        second      = date.second
        microsecond = date.microsecond
        # Convert time to fractions of a day
        day = day + hour / 24.0 + minute / 1440.0 + (second + microsecond/1.e6) / 86400.0
        # Start Meeus algorithm 
        if month < 3 :
            month = month + 12
            year = year - 1
        day = int(365.0 * (year + 4716)) + int(30.6001 * (month + 1)) + day - 1524.5
        return day

