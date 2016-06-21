import sys
import subprocess
import os
import re
import logging
import drm4g.communicators
from drm4g.communicators import ComException
from drm4g.utils.url     import urlparse

__version__  = '2.4.1'
__author__   = 'Carlos Blanco'
__revision__ = "$Id: local.py 2352 2015-02-24 10:23:57Z carlos $"

logger  = logging.getLogger(__name__)

if sys.version_info[0] == 2 :
    execution_permissions = 0755
else :
    execution_permissions = 0o755

class Communicator(drm4g.communicators.Communicator):
    """
    Interact with local resources using shell commands 
    """
    def connect(self):
        logger.debug( "Your are using the local communicator" )     

    def execCommand(self, command, input=None ):
        command_proc = subprocess.Popen(command,
            shell = True,
            stdin  = subprocess.PIPE,
            stdout = subprocess.PIPE,
            stderr = subprocess.PIPE,
            env = os.environ)
        if input :
            for line in input.split():
                command_proc.stdin.write("%s\n" % line)
                command_proc.stdin.flush()
                stdout, stderr = command_proc.communicate("%s\n" % line)
        else :
            stdout, stderr = command_proc.communicate()
        return stdout , stderr 
        
    def mkDirectory(self, url):
        to_dir = self._set_dir(urlparse(url).path)
        out, err = self.execCommand("mkdir -p %s" % to_dir )
        if err:
            output = "Could not create %s directory: %s " % ( to_dir , ' '.join( err.split( '\n' ) ) )
            logger.error( output )
            raise ComException( output )  
        
    def copy(self, source_url, destination_url, execution_mode):
        if 'file://' in source_url:
            from_dir = urlparse(source_url).path
            to_dir   = self._set_dir(urlparse(destination_url).path)
        else:
            from_dir = self._set_dir(urlparse(source_url).path)
            to_dir   = urlparse(destination_url).path
        out, err = self.execCommand("cp -r %s %s" % (from_dir,to_dir))
        if err:
            output = "Could not copy from %s to %s : %s" % ( from_dir, to_dir , ' '.join( err.split( '\n' ) ) )
            logger.error( output )
            raise ComException( output )
        if execution_mode == 'X':
            os.chmod(to_dir, execution_permissions )
            
    def rmDirectory(self, url):
        to_dir   = self._set_dir(urlparse(url).path)    
        out, err = self.execCommand("rm -rf %s" % to_dir )
        if err:
            output = "Could not remove %s directory: %s " % ( to_dir , ' '.join( err.split( '\n' ) ) )
            logger.error( output )
            raise ComException( output )
    
    def checkOutLock(self, url):   
        to_dir = self._set_dir(urlparse(url).path)
        return os.path.isfile( '%s/.lock' % to_dir )
   
    def close(self):
        pass


    #internal
    def _set_dir(self, path):
        work_directory = os.path.expanduser( self.work_directory )
        return re.compile( r'^~' ).sub( work_directory , path )
            
        
