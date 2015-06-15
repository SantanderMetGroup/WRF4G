import cmd
import os
import sys
import re
import getpass
import subprocess
import logging

from drm4g     import REMOTE_VOS_DIR, DRM4G_CONFIG_FILE, DRM4G_BIN, DRM4G_DIR
from os.path   import expanduser, join, dirname, exists, basename, expandvars

__version__  = '2.3.1'
__author__   = 'Carlos Blanco'
__revision__ = "$Id: __init__.py 2352 2015-02-24 10:23:57Z carlos $"

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
    logging.debug( "Executing command ... " + cmd )
    cmd_to_exec = subprocess.Popen(  cmd , 
                                  shell=True , 
                                  stdin=subprocess.PIPE,
                                  stdout=subprocess.PIPE,
                                  stderr=subprocess.PIPE,
                                  env=env
                                  )
    out , err =  cmd_to_exec.communicate()
    return out , err

def yes_no_choice( message ,  default = 'y' ) :
    """
    Ask for Yes/No questions
    """
    choices = 'Y/n' if default.lower() in ('y', 'yes') else 'y/N'
    choice = raw_input("%s (%s) " % (message, choices))
    values = ('y', 'yes', '') if default == 'y' else ('y', 'yes')
    return choice.strip().lower() in values

class Agent( object ):
    """
    Class to manage ssh-agent command. 
    """
    
    def __init__( self ):
        self.agent_env  = dict() 
        self.agent_file = join( DRM4G_DIR  , 'var' , 'agent.conf' )

    def start( self ):
        def _start():
            logging.debug('Starting ssh-agent ...')
            # 's' option generates Bourne shell commands on stdout
            out , err = exec_cmd( 'ssh-agent -s ' ) 
            logging.debug( out )
            match = re.search( 'SSH_AUTH_SOCK=(?P<SSH_AUTH_SOCK>[^;]+);.*' \
                           + 'SSH_AGENT_PID=(?P<SSH_AGENT_PID>\d+);', out, re.DOTALL)
            if match :
                self.agent_env = match.groupdict()
                logging.debug('Agent pid: %s'  % self.agent_env['SSH_AGENT_PID'])
            else:
                logging.error( err )
                raise Exception('Cannot determine agent data from output: %s' % out )
            with open( self.agent_file , 'w') as f:
                f.write( self.agent_env['SSH_AGENT_PID'] + '\n' + self.agent_env['SSH_AUTH_SOCK'] )
        
        if not self.is_alive() :
            _start()
        elif not self.agent_env:
            self.get_agent_env()
 
    def status( self ) :
        if self.is_alive() :
            logging.info( "ssh-agent is running" )
        else :
            logging.info( "ssh-agent is stopped" )

    def is_alive( self ):
        if not exists( self.agent_file ) :
            logging.debug("'%s' does not exist" % ( self.agent_file ) )
            return False
        else :
            if process_is_runnig( self.agent_file ):
                return True
            else :
                return False
            
    def get_agent_env( self ):
        logging.debug("Reading '%s' file" % ( self.agent_file ) )
        with open( self.agent_file , 'r' ) as f:
            lines = f.readlines()
        self.agent_env['SSH_AGENT_PID'] = lines[0].strip()
        self.agent_env['SSH_AUTH_SOCK'] = lines[1].strip()
            
    def update_agent_env( self ):
        env = os.environ
        if not self.agent_env :
            self.get_agent_env()
        env.update( self.agent_env )
        return env
    
    def add_key( self, identity_file, lifetime ):
        logging.debug("Adding '%s' into ssh-agent for %s hours" % ( identity_file , lifetime ) )
        out , err = exec_cmd( 'ssh-add -t %sh %s' % ( lifetime , identity_file ),
                  stdin=sys.stdin, stdout=sys.stdout, env=self.update_agent_env() )
        if err :
            logging.info( err )
    
    def delete_key( self, identity_file ):
        logging.debug('Deleting key %s' % identity_file )
        out , err = exec_cmd( 'ssh-add -d %s' % identity_file,
                              stdin=sys.stdin, stdout=sys.stdout, env=self.update_agent_env() )
        if err :
            logging.info( err )
    
    def copy_key( self, identity_file , user, frontend ):
        logging.debug("Coping '%s' to ~/.ssh/authorized_keys file on '%s'" % ( identity_file, frontend ) )
        out , err = exec_cmd( 'ssh-copy-id -i %s %s@%s' %(  identity_file , user, frontend ),
                              stdin=sys.stdin, stdout=sys.stdout, env=self.update_agent_env() )
        logging.debug( out ) 
    
    def list_key( self , identity_file ):
        logging.debug("Listing '%s' key" % identity_file)
        out , err = exec_cmd( 'ssh-add -L' , env=self.update_agent_env() )
        match = re.search( '.*%s' % basename( identity_file ) , out)
        if match :
            logging.info( match.group() )
        else :
            logging.info( "The private key '%s' is not available anymore" % identity_file)
        
    def stop( self ):
        logging.debug( 'Stopping ssh-agent ... ' )
        if self.is_alive():
            out , err = exec_cmd( 'ssh-agent -k' , env=self.update_agent_env() )
            logging.debug( out )
            if err :
                logging.info( err )
        else:
            logging.debug( 'ssh-agent is already stopped' )
        try:
            os.remove( self.agent_file )
        except :
            pass
        

class Daemon( object ):
    
    def __init__( self ):
        self.gwd_pid  = join( DRM4G_DIR  , 'var' , 'gwd.pid' )
                
    def status( self ):
        if self.is_alive() :
            logging.info( "DRM4G is running" )
        else :
            logging.info( "DRM4G is stopped" )
  
    def is_alive( self ):
        if not exists( self.gwd_pid ) :
            return False
        else :
            if process_is_runnig( self.gwd_pid ) :
                return True 
            else :
                return False
  
    def start( self ):
        logging.info( "Starting DRM4G .... " )
        if not exists( self.gwd_pid ) or ( exists( self.gwd_pid ) and not process_is_runnig( self.gwd_pid ) ) :
            lock = join( DRM4G_DIR , 'var' '/.lock' )
            if exists( lock ) : 
                os.remove( lock )
            os.environ[ 'PATH' ] = '%s:%s' % ( DRM4G_BIN , os.getenv( 'PATH' ) )
            logging.debug( "Starting gwd .... " )
            out , err = exec_cmd( join( DRM4G_BIN , 'gwd' ) )
            if err :
                logging.info( err )
            if out :
                logging.info( out ) 
            if not err and not out :
                logging.info( "OK" )
        else :
            logging.info( "WARNING: DRM4G is already running." )
                
    def stop( self ):
        logging.info( "Stopping DRM4G .... " )
        logging.debug( "Stopping gwd .... " )
        out , err = exec_cmd( "%s -k" % join( DRM4G_BIN , "gwd" ) )
        if err :
            logging.info( err )
        if out :
            logging.info( out )
        if not err and not out :
            logging.info( "OK" )
            
    def clear( self ):
        yes_choise = yes_no_choice( "Do you want to continue clearing DRM4G " )
        if yes_choise :
            logging.info( "Clearing DRM4G .... " )
            cmd = "%s -c" % join( DRM4G_BIN , "gwd" )
            out , err = exec_cmd( cmd )
            logging.debug( out ) 
            if err :
                logging.info( err )
            if out :
                logging.info( out )
            if not err and not out :
                logging.info( "OK" )
        else :
            self.start()
            
class Resource( object ):
    
    def __init__( self , config ):
        self.config = config
        
    def check_frontends( self ) :
        """
        Check if the frontend of a given resource is reachable.
        """
        self.check( )
        communicators = self.config.make_communicators()
        for resname, resdict in sorted( self.config.resources.iteritems() ) :
            if resdict[ 'enable' ] == 'true' :
                communicator = communicators.get( resname )
                try :
                    communicator.connect()
                    logging.info( "Resource '%s' :" % ( resname ) )
                    logging.info( "--> The front-end '%s' is accessible\n" % communicator.frontend )
                except Exception , err :
                    logging.error( "Resource '%s' :" % ( resname ) )
                    logging.error( "--> The front-end '%s' is not accessible\n" % communicator.frontend )
                            
    def edit( self ) :
        """
        Edit resources file.
        """
        logging.debug( "Editing '%s' file" % DRM4G_CONFIG_FILE )
        os.system( "%s %s" % ( os.environ.get('EDITOR', 'vi') , DRM4G_CONFIG_FILE ) )
        self.check( )

    def list( self ) :
        """
        Check if the resource.conf file has been configured well and list the resources available.
        """
        self.check( )
        logging.info( "\033[1;4m%-20.20s%-20.20s\033[0m" % ('Name', 'State' ) )
        for resname, resdict in sorted( self.config.resources.iteritems() ) :
            if resdict[ 'enable' ] == 'true' :
                state = 'enabled'
            else :
                state = 'disabled'
            logging.info( "%-20.20s%s" % ( resname, state ) )
                    
    def features( self ) :
        """
        List the features of a given resource.
        """
        self.check( )
        for resname , resdict in sorted( self.config.resources.iteritems() ) :
            logging.info( "Resource '%s' :" % ( resname ) )
            for key , val in sorted( resdict.iteritems() ) :
                logging.info( "\t--> '%s' : '%s'" % ( key, val ) )         
    
    def check( self ) :
        """
        Check if the resource.conf file has been configured well.
        """
        self.config.load()
        errors = self.config.check()
        if errors :
            raise Exception( "Please, review your configuration file" )
    
class Proxy( object ):
    
    def __init__( self, config, name ):
        self.config    = config
        self.resource  = self.config.resources[ name ]
        self.communicator = self.config.make_communicators()[ name ]
        if self.resource.has_key( 'myproxy_server' ) :
            self.prefix = "X509_USER_PROXY=%s MYPROXY_SERVER=%s " % (
                                                                 join( REMOTE_VOS_DIR , self.resource[ 'myproxy_server' ] ),
                                                                 self.resource[ 'myproxy_server' ]
                                                                 )
        else :
            self.prefix = "X509_USER_PROXY=%s/${MYPROXY_SERVER} " % REMOTE_VOS_DIR
        
    def create( self , proxy_lifetime ):
        logging.debug("Creating '%s' directory to store the proxy ... " % REMOTE_VOS_DIR )
        cmd = "mkdir -p %s" % REMOTE_VOS_DIR
        logging.debug( "Executing command ... " + cmd )
        out, err = self.communicator.execCommand( cmd )
        if not err :
            message      = 'Insert your Grid password: '
            grid_passwd  = getpass.getpass(message)
            cmd = self.prefix + "myproxy-init -S --cred_lifetime %s --proxy_lifetime %s --local_proxy -n -d" % ( 
                                                                                                         proxy_lifetime ,
                                                                                                         proxy_lifetime
                                                                                                         )
            logging.debug( "Executing command ... " + cmd )
            out , err = self.communicator.execCommand( cmd , input = grid_passwd )
            logging.info( out )
            if err :
                logging.info( err )
        else :
            raise Exception( err )
    
    def configure( self, certificate ) :
        dir_certificate   = dirname( certificate ) 
        base_certificate  = basename( certificate )
        logging.info( "Converting '%s' key to pem format ... " % base_certificate )      
        cmd = "openssl pkcs12 -nocerts -in %s -out %s" % ( certificate, join( dir_certificate, 'userkey.pem' ) ) 
        out , err = exec_cmd( cmd, stdin=sys.stdin, stdout=sys.stdout ) 
        if "invalid password" in err :
            raise Exception( err )
        logging.info( "Converting '%s' certificate to pem format ... " % base_certificate )
        cmd = "openssl pkcs12 -clcerts -nokeys -in %s -out %s" % ( certificate, join( dir_certificate, 'usercert.pem' ) )
        out , err = exec_cmd( cmd , stdin=sys.stdin, stdout=sys.stdout )
        if "invalid password" in err :
            raise Exception( err )
        logging.debug( "Creating '~/.globus' directory ... " )
        cmd = "mkdir -p ~/.globus"
        logging.debug( "Executing command ... " + cmd )
        out, err = self.communicator.execCommand( cmd )
        if err :
            raise Exception( err )
        for file in [ 'userkey.pem' , 'usercert.pem' ] :
            cmd = "rm -rf $HOME/.globus/%s" % file 
            logging.debug( "Executing command ... " + cmd )
            out, err = self.communicator.execCommand( cmd )
            if err :
                raise Exception( err )
            logging.info( "Copying '%s' to '%s' ..." % ( file , self.resource.get( 'frontend' ) ) )
            self.communicator.copy( 'file://%s'  % join( dir_certificate, file ) , 
                                    'ssh://_/%s' % join( '.globus' , file ) )
        logging.info( "Modifying userkey.pem permissions ... " )
        cmd = "chmod 400 $HOME/.globus/userkey.pem"
        logging.debug( "Executing command ... " + cmd )
        out, err = self.communicator.execCommand( cmd )
        if err :
            raise Exception( err )
        logging.info( "Modifying usercert.pem permissions ... " )
        cmd = "chmod 600 $HOME/.globus/usercert.pem"
        logging.debug( "Executing command ... " + cmd )
        out, err = self.communicator.execCommand( cmd )
        if err :
            raise Exception( err )
 
    def check( self ):
        cmd = self.prefix + "grid-proxy-info"
        logging.debug( "Executing command ... " + cmd )
        out, err = self.communicator.execCommand( cmd )
        logging.info( out )
        if err :
            logging.info( err ) 
    
    def destroy( self ):
        cmd = self.prefix + "myproxy-destroy"
        logging.debug( "Executing command ... " + cmd ) 
        out , err = self.communicator.execCommand( cmd )
        logging.info( out )
        if err : 
            logging.info( err )
        cmd = self.prefix + "grid-proxy-destroy"
        logging.debug( "Executing command ... " + cmd )
        out , err = self.communicator.execCommand( cmd )
        logging.info( out )
        if err :
            logging.info( err )

