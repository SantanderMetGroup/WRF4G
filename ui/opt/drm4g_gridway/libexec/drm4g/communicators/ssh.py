import os
import sys
import platform
import traceback
try:
    import paramiko
except ImportError:
    try:
        GW_LOCATION = os.environ['GW_LOCATION']
    except Exception:
        print 'Please, set GW_LOCATION variable'
        sys.exit(-1)
    try:
        if sys.version_info < (2,5):
            sys.path.append(os.path.join(GW_LOCATION, 'libexec', 'drm4g', 'utils', 'Cryptos', 'Crypto24_x86_64'))
        else:
            sys.path.append(os.path.join(GW_LOCATION, 'libexec', 'drm4g', 'utils', 'Cryptos', 'Crypto_x86_64'))
        sys.path.append(os.path.join(GW_LOCATION, 'libexec','drm4g','utils'))
        import paramiko
    except Exception, e:
        print 'Caught exception: %s: %s' % (e.__class__, str(e))
        traceback.print_exc(file=sys.stdout)
        sys.exit(-1)

import socket
import re
from drm4g.utils.url import urlparse
import drm4g.communicators 

__version__ = '0.1'
__author__  = 'Carlos Blanco'
__revision__ = "$Id: ssh.py 1357 2012-01-10 19:59:38Z carlos $"

class Communicator (drm4g.communicators.Communicator):
    
    """
    Create a SSH session to remote resources.  
    """
    ##################
    # AUTHENTICATION #
    ##################

    PRIVATE_KEYS = (
        ("rsa", r"~/.ssh/id_rsa"),
        ("dsa", r"~/.ssh/id_dsa"),
        )
    port    = 22
    timeout = 20 # seconds
 
    def __init__(self):
        self._lock = __import__('threading').Lock()
 
    def connect(self):
        self._lock.acquire()
        try:
            agent = paramiko.Agent()
            keys = agent.get_keys()
            for key, path in self.PRIVATE_KEYS:
                try:
                    privatekeyfile = os.path.expanduser(path)
                    if key == 'rsa':
                        ki_rsa = paramiko.RSAKey.from_private_key_file(privatekeyfile)
                        keys = keys + (ki_rsa,)
                    if key == 'dsa':        
                        ki_dsa = paramiko.DSSKey.from_private_key_file(privatekeyfile)
                        keys = keys + (ki_dsa,)
                except Exception: pass
            for key in keys:
                try:
                    sock = socket.socket()
                    try:
                        sock.settimeout(self.timeout)
                    except:
                        pass 
                    sock.connect((self.hostName, self.port))  
                    self._trans = paramiko.Transport(sock)
                    self._trans.connect(username = self.userName, pkey = key)
                    if self._trans.is_authenticated(): break
                except socket.gaierror:
                    raise drm4g.communicators.ComException('Could not resolve hostname ' + self.hostName)
                except Exception: pass
        finally: self._lock.release()            
        if not self._isAuthenticated():
            raise drm4g.communicators.ComException('Authentication failed to ' + self.hostName)        
        
    def execCommand(self, command):
        if not self._isAuthenticated(): 
            self.connect()
        self._lock.acquire()
        try: channel = self._trans.open_session()
        finally: self._lock.release()
        channel.exec_command(command)
        stdout = channel.makefile('rb', -1).readlines()
        stderr = channel.makefile_stderr('rb', -1).readlines()
        return ''.join(stdout), ''.join(stderr)
            
    def mkDirectory(self, url):
        if not self._isAuthenticated(): 
            self.connect()
        self._lock.acquire()
        try: sftp = paramiko.SFTPClient.from_transport(self._trans)
        finally: self._lock.release()
        to_dir = self._setDir(urlparse(url).path)    
        sftp.mkdir(to_dir)
        try: sftp.close()
        except Exception: pass
            
    def copy(self, source_url, destination_url, execution_mode):
        if not self._isAuthenticated(): 
            self.connect()
        self._lock.acquire()
        try: sftp = paramiko.SFTPClient.from_transport(self._trans)
        finally: self._lock.release()
        if 'file://' in source_url:
            from_dir = urlparse(source_url).path
            to_dir   = self._setDir(urlparse(destination_url).path)
            sftp.put(from_dir, to_dir)
            if execution_mode == 'X': sftp.chmod(to_dir, 0755)#execution permissions
        else:
            from_dir = self._setDir(urlparse(source_url).path)
            to_dir   = urlparse(destination_url).path
            sftp.get(from_dir, to_dir)
        try: sftp.close()
        except Exception: pass
            
    def rmDirectory(self, url):
        if not self._isAuthenticated(): 
            self.connect()
        self._lock.acquire()
        try: sftp = paramiko.SFTPClient.from_transport(self._trans)
        finally: self._lock.release()
        to_dir = self._setDir(urlparse(url).path) 
        try: sftp.listdir(to_dir)
        except IOError: pass
        else: self._rmdirRecursive(to_dir,sftp)
    	try: sftp.close()
        except Exception: pass
        
    def close(self):
        self._lock.acquire()
        try:
            try: self._trans.close()
            except Exception: pass
        finally: self._lock.release()
            
    #internal

    def _isAuthenticated(self):
        self._lock.acquire()
        try:
            result = self._trans.is_authenticated()
            return result
        finally: self._lock.release()
   
    def _rmdirRecursive(self, path, sftp):
        for file in sftp.listdir(path):
            file_or_dir = '%s/%s' % (path, file)
            try: sftp.remove(file_or_dir)
            except IOError: self._rmdirRecursive(file_or_dir,sftp)
        sftp.rmdir(path) 
  
    def _setDir(self, path):
        if self.workDirectory != r'~':
            return re.compile(r'^~').sub(self.workDirectory, path)
        else:
            return '.%s' % (path[1:])




