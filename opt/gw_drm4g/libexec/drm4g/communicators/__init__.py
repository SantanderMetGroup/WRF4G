import logging
from drm4g import REMOTE_JOBS_DIR, SSH_PORT

__version__  = '2.2.0'
__author__   = 'Carlos Blanco'
__revision__ = "$Id: __init__.py 2250 2014-08-27 09:04:57Z carlos $"

logger  = logging.getLogger(__name__)

class ComException(Exception):
    """
    Exception raised by failures in Communicator class
    """
    pass

class Communicator(object):
    """
    Communicator is a abstract class that you must overload for your
    particular communicator. Communicator defines several methods to 
    interact with computing resources. 
    """
    def __init__(self):
        self.work_directory = REMOTE_JOBS_DIR
        self.port           = SSH_PORT
        self.username       = None
        self.frontend       = None
        self.private_key    = None 

    def connect(self):
        """
        To establish the connection to resource. 
        """
        pass

    def execCommand(self, command , input=None ):
        """
        Execute command and return stdout and stderr.
        
        @param command: a shell command to execute.
        @type command: string
        @param input: optional input argument
        @type input: string
        @return: stdout and stderr associated with the command executed
        @rtype: tuple of string (stdout, stderr)
        """
        pass

    def mkDirectory(self, destination_url):
        """
        Create a directory.
  
        @param destination_url: url of the folder to create
        @type destination_url: string       
        """
        pass

    def copy(self, source_url, destination_url, execution_mode = 'X'):
        """
        Copy a file from source_url to destination_url. If execution_mode = 'X' you 
        set execute permission to the destination file.
        
        @param source_url : file source (url) to copy
        @type source_url: string
        @param destination_url : file destination (url)
        @type destination_url: string
        @param execution_mode : give execute permissions to the file  
        @type execution_mode : string
        """
        pass

    def rmDirectory(self, destination_url):
        """
        Remove a directory.
  
        @param destination_url: url of the folder to remove
        @type destination_url: string       
        """
        pass

    def close(self):
        """
        Close the connection.
        """
        pass

    def checkOutLock(self, destination_url):
        """
        @param destination_url: url of the folder to check out if .lock file exists
        @type destination_url: string
        @return: False, if .lock file do not exist or True, if It exists
        @rtype: boolean
        """
        pass
  
    
    
    
