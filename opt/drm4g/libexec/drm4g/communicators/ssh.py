import sys
import platform
from os.path     import dirname, abspath, join, expanduser, exists

try:
    GW_LOCATION  = dirname( dirname ( abspath( __file__ ) ) )
    cryptos_path = join( GW_LOCATION , 'utils' , 'Cryptos' )
    if platform.architecture()[0] == '32bit':
        crypto_package = 'Crypto_i686'
    else:
        crypto_package = 'Crypto_x86_64'
    sys.path.insert( 0, join( cryptos_path , crypto_package ) )
    sys.path.insert( 0, join( GW_LOCATION , 'utils' ) )

    from paramiko.transport     import Transport
    from paramiko.agent         import Agent
    from paramiko.dsskey        import DSSKey
    from paramiko.rsakey        import RSAKey
    from scp                    import SCPClient
except Exception as e :
    exit( 'Caught exception: %s' % str(e) )

import socket
import re
import logging
import drm4g.communicators
import drm4g.commands
from drm4g.communicators    import ComException, logger
from drm4g                  import SFTP_CONNECTIONS, SSH_CONNECT_TIMEOUT  
from drm4g.utils.url        import urlparse

__version__  = '2.4.1'
__author__   = 'Carlos Blanco'
__revision__ = "$Id: ssh.py 2352 2015-02-24 10:23:57Z carlos $"

class Communicator (drm4g.communicators.Communicator):
    """
    Create a SSH session to remote resources.  
    """
    _lock       = __import__('threading').Lock()
    _sem        = __import__('threading').Semaphore(SFTP_CONNECTIONS)
    _trans      = None 
 
    def connect(self):
        with self._lock :
            if not self._trans or not self._trans.is_authenticated( ) :
                logger.debug("Opening ssh connection ... ")
                keys = None
                logger.debug("Trying ssh-agent ... " )
                drm4g_agent = drm4g.commands.Agent()
                drm4g_agent.start()
                drm4g_agent.update_agent_env()
                # paramiko agent
                agent = Agent()
                keys  = agent.get_keys()
                if not keys :
                    logger.error( "Error trying to connect to '%s'" % self.frontend )
                    logger.error( "Impossible to load '%s' key from the ssh-agent"  % self.private_key )
                    try:
                        status_ssh_agent = agent._conn
                    except Exception as err :
                        logger.warning("Probably you are using paramiko version <= 1.7.7.2 : %s " % str( err ) )
                        status_ssh_agent = agent.conn
                    if not status_ssh_agent:
                        output = "'ssh-agent' is not running"
                        raise ComException( output ) 
                    else:
                        if agent.get_keys():
                            output = "ssh-agent is running but none of the keys have been accepted" 
                            "by remote frontend %s." % self.frontend
                            raise ComException( output )
                        else:
                            output = "'ssh-agent' is running but without any keys"
                            raise ComException( output  )
                """
                logger.debug("Trying '%s' key ... " % self.private_key )
                private_key_path = expanduser( self.private_key )
                if ( not exists( private_key_path ) ) and ( not 'PRIVATE KEY' in  self.private_key ):
                    output = "'%s'key does not exist" % private_key_path
                    raise ComException( output ) 
                for pkey_class in (RSAKey, DSSKey):
                    try :
                        if 'PRIVATE KEY' in self.private_key :
                            import StringIO
                            key  = pkey_class.from_private_key( StringIO.StringIO ( self.private_key.strip( "'" ) ) )
                        else : 
                            key  = pkey_class.from_private_key_file( private_key_path )
                        keys = (key,)
                    except Exception :
                        pass
                if not keys :
                    output = "Impossible to load '%s' key "  % self.private_key
                    logger.error( output )
                    raise ComException( ) 
                """
                for key in keys:
                    try:
                        sock = socket.socket()
                        try:
                            sock.settimeout( SSH_CONNECT_TIMEOUT )
                        except :
                            output = "Timeout trying to connect to '%s'" % self.frontend 
                            raise ComException( output )
                        logger.debug( "Connecting to '%s' as user '%s' port  '%s' ..." 
                                           % ( self.frontend , self.username, self.port ) )
                        if ':' in self.frontend :
                            self.frontend , self.port = self.frontend.split( ':' )
                        sock.connect( ( self.frontend , self.port ) )
                        self._trans      = Transport( sock )
                        self._trans.connect( username = self.username , pkey = key )
                        if self._trans.is_authenticated( ) :
                            break
                    except socket.gaierror:
                        output = "Could not resolve hostname '%s' " % self.frontend
                        raise ComException( output )
                    except Exception as err :
                        logger.warning( "Error connecting '%s': %s" % ( self.frontend , str ( err ) ) ) 
            if not self._trans :
                output = "Authentication failed to '%s'. Try to execute `ssh -vvv -p %d %s@%s` and see the response." % ( self.frontend , self.port, self.username, self.frontend )
                raise ComException( output  )
       
    def execCommand(self , command , input = None ):
        self.connect()
        with self._lock :
            channel = self._trans.open_session()
        channel.settimeout( SSH_CONNECT_TIMEOUT )
        channel.exec_command( command )
        if input :
            for line in input.split( ):
                channel.makefile( 'wb' , -1 ).write( '%s\n' % line )
                channel.makefile( 'wb' , -1 ).flush( )
        stdout = ''.join( channel.makefile( 'rb' , -1 ).readlines( ) )
        stderr = ''.join( channel.makefile_stderr( 'rb' , -1).readlines( ) )
        if channel :
            channel.close( )
        return stdout , stderr
            
    def mkDirectory(self, url):
        to_dir         = self._set_dir(urlparse(url).path)    
        stdout, stderr = self.execCommand( "mkdir -p %s" % to_dir )
        if stderr :
            raise ComException( "Could not create %s directory on '%s': %s" % ( to_dir , self.frontend , stderr ) )
        
    def rmDirectory(self, url):
        to_dir         = self._set_dir(urlparse(url).path)    
        stdout, stderr = self.execCommand( "rm -rf %s" % to_dir )
        if stderr:
            raise ComException( "Could not remove %s directory on '%s': %s" % ( to_dir , self.frontend , stderr ) )
            
    def copy( self , source_url , destination_url , execution_mode = '' ) :
        with self._sem :
            self.connect( )
            scp = SCPClient( self._trans )
            if 'file://' in source_url :
                from_dir = urlparse( source_url ).path
                to_dir   = self._set_dir( urlparse( destination_url ).path )
                scp.put( from_dir , to_dir )
                if execution_mode == 'X': 
                    stdout, stderr = self.execCommand( "chmod +x %s" % to_dir )
            else:
                from_dir = self._set_dir( urlparse( source_url ).path )
                to_dir   = urlparse(destination_url).path
                logger.warning( "%s , %s" %  (from_dir, to_dir  ))
                scp.get( from_dir, to_dir )
            
    def checkOutLock(self, url):
        to_dir = self._set_dir( urlparse( url ).path )
        stdout, stderr = self.execCommand( "ls %s/.lock" % to_dir )
        if stderr:
            return False
        return True

    def close( self ) :
        try : 
            if self._trans :
                self._trans.close( )
        except Exception as err: 
            logger.warning( "Could not close the SSH connection to '%s': %s" % ( self.frontend , str( err ) ) )
            
    def __del__( self ) :
        """
        Attempt to clean up if not explicitly closed.
        """
        self.close( )


    #internal
    def _set_dir(self, path):
        work_directory =  re.compile( r'^~' ).sub( self.work_directory , path )
        if work_directory[0] == r'~' :
            return ".%s" %  work_directory[ 1: ]
        else :
            return  work_directory
