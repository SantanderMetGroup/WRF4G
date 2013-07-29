import subprocess
import os
import re
import drm4g.communicators 
from drm4g.utils.url import urlparse

__version__ = '0.1'
__author__  = 'Carlos Blanco'
__revision__ = "$Id: local.py 1768 2013-02-14 11:20:12Z carlos $"

class Communicator(drm4g.communicators.Communicator):
    """
    Interact with local resources using shell commands 
    """
        
    def execCommand(self, command):
        command_proc = subprocess.Popen(command,
            shell = True,
            stdout = subprocess.PIPE,
            stderr = subprocess.PIPE,
            env = os.environ)
        return command_proc.communicate()
        
    def mkDirectory(self, url):
        to_dir = self._setDir(urlparse(url).path)
        out, err = self.execCommand("mkdir -p %s" % (to_dir))
        if err:
            raise drm4g.communicators.ComException("Couldn't create %s directory" %(to_dir))  
        
    def copy(self, source_url, destination_url, execution_mode):
        if 'file://' in source_url:
            from_dir = urlparse(source_url).path
            to_dir   = self._setDir(urlparse(destination_url).path)
        else:
            from_dir = self._setDir(urlparse(source_url).path)
            to_dir   = urlparse(destination_url).path
        out, err = self.execCommand("cp -r %s %s" % (from_dir,to_dir))
        if err:
            raise drm4g.communicators.ComException("Couldn't copy from %s to %s" % (from_dir, to_dir))
        if execution_mode == 'X':
            os.chmod(to_dir, 0755)#execution permissions
            
    def rmDirectory(self, url):
        to_dir = self._setDir(urlparse(url).path)    
        out, err = self.execCommand("rm -rf %s" % (to_dir))
        if err:
            raise drm4g.communicators.ComException("Couldn't remove %s directory" %(to_dir))
    
    def checkOutLock(self, url):   
        to_dir = self._setDir(urlparse(url).path)
        return os.path.isfile( '%s/.lock'% (to_dir))

    #internal
    
    def _setDir(self, path):
        if self.workDirectory != '~':
            return re.compile(r'^~').sub(self.workDirectory, path)
        else: 
            return os.path.expanduser(path)
