import subprocess
import os
import shutil
import re
import drm4g.communicators 
from drm4g.utils.url import urlparse

__version__ = '0.1'
__author__  = 'Carlos Blanco'
__revision__ = "$Id: local.py 1232 2011-10-26 09:09:40Z carlos $"

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
        to_dir = self._set_dir(urlparse(url).path)    
        os.mkdir(to_dir)
        
    def copy(self, source_url, destination_url, execution_mode):
        if 'file://' in source_url:
            from_dir = urlparse(source_url).path
            to_dir   = self._set_dir(urlparse(destination_url).path)
        else:
            from_dir = self._set_dir(urlparse(source_url).path)
            to_dir   = urlparse(destination_url).path
        shutil.copy(from_dir,to_dir)
        if execution_mode == 'X':
            os.chmod(to_dir, 0755)#execution permissions
            
    def rmDirectory(self, url):
        to_dir = self._set_dir(urlparse(url).path)    
        out, err = self.execCommand("LANG=POSIX rm -rf %s" % (to_dir))
        if err:
            raise drm4g.communicators.ComException("Couldn't remove: %s" %(to_dir))

    #internal
    
    def _set_dir(self, path):
        work_dir = self.getWorkDir()
        if work_dir != '~':
            return re.compile(r'^~').sub(work_dir, path)
        else: 
            return os.path.expanduser(path)
