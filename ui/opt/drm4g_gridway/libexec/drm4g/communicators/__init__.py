__version__  = '0.1'
__author__   = 'Carlos Blanco'
__revision__ = "$Id: __init__.py 1790 2013-03-26 16:56:41Z carlos $"

__all__ = ['ComException', 'Communicator']

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
        self._path_work = "NULL"
        self._host      = "NULL"
        self._username  = "NULL"
        self._port      = "NULL"
        self._key_file  = "NULL"

    def connect(self):
        """
        To establish the connection to resource. 
        """
        pass

    def execCommand(self, command):
        """
        Execute command and return stdout and stderr.
        
        @param command: a shell command to execute.
        @type command: string
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

    def close(self, force = True):
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

    def setWorkDir(self, path_work_directory):
        self._path_work = path_work_directory

    def getWorkDir(self):
        return self._path_work

    def setHostName(self, host_name):
        self._host = host_name

    def getHostName(self):
        return self._host

    def setUserName(self, username):
        self._username = username

    def getUserName(self):
        return self._username
    
    def setPort(self, port):
        self._port = port

    def getPort(self):
        return self._port
    
    def setKeyFile(self, key_file):
        self._key_file = key_file

    def getKeyFile(self):
        return self._key_file

    workDirectory = property(getWorkDir, setWorkDir)
    hostName      = property(getHostName, setHostName)
    userName      = property(getUserName, setUserName)
    port          = property(getPort, setPort)
    keyFile       = property(getKeyFile, setKeyFile)
