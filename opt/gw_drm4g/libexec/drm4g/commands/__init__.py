import cmd
import os
import sys
import re
import getpass
import subprocess
import logging
import drm4g.core.configure

from drm4g                import REMOTE_VOS_DIR , DRM4G_CONFIG_FILE , DRM4G_BIN , DRM4G_DIR
from drm4g.utils.docopt   import docopt , DocoptExit
from os.path              import expanduser , join , dirname , exists , basename

__version__  = '2.1.1'
__author__   = 'Carlos Blanco'
__revision__ = "$Id: __init__.py 1931 2013-09-25 10:46:59Z carlos $"

logger = logging.getLogger(__name__)

def docopt_cmd(func):
    """
    This decorator is used to pass the result
    of the docopt parsing to the called action.
    """
    def fn(self, arg):
        try:
            opt = docopt(fn.__doc__, arg)
        except SystemExit:
            return
        return func(self, opt)

    fn.__name__ = func.__name__
    fn.__doc__  = func.__doc__
    fn.__dict__.update( func.__dict__ )
    return fn

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
    logger.debug( "Executing ... " + cmd )
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
            logger.debug('Starting ssh-agent ...')
            # 's' option generates Bourne shell commands on stdout
            out , err = exec_cmd( 'ssh-agent -s ' ) 
            logger.debug( out )
            match = re.search( 'SSH_AUTH_SOCK=(?P<SSH_AUTH_SOCK>[^;]+);.*' \
                           + 'SSH_AGENT_PID=(?P<SSH_AGENT_PID>\d+);', out, re.DOTALL)
            if match :
                self.agent_env = match.groupdict()
                logger.debug('Agent pid: %s'  % self.agent_env['SSH_AGENT_PID'])
            else:
                logger.error( err )
                raise Exception('Cannot determine agent data from output: %s' % out )
            with open( self.agent_file , 'w') as f:
                f.write( self.agent_env['SSH_AGENT_PID'] + '\n' + self.agent_env['SSH_AUTH_SOCK'] )
        
        if not self.is_alive() :
            _start()
        elif not self.agent_env:
            self.get_agent_env()
            
    def is_alive( self ):
        if not exists( self.agent_file ) :
            logger.debug("'%s' does not exist" % ( self.agent_file ) )
            return False
        else :
            if process_is_runnig( self.agent_file ):
                return True
            else :
                return False
            
    def get_agent_env( self ):
        logger.debug("Reading '%s' file" % ( self.agent_file ) )
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
    
    def add_key( self, identity_file ):
        logger.debug('Adding keys into ssh-agent')
        out , err = exec_cmd( 'ssh-add %s' % identity_file, 
                  stdin=sys.stdin, stdout=sys.stdout, env=self.update_agent_env() )
        if err :
            logger.info( err )
    
    def delete_key( self, identity_file ):
        logger.debug('Deleting key %s' % identity_file )
        out , err = exec_cmd( 'ssh-add -d %s' % identity_file,
                              stdin=sys.stdin, stdout=sys.stdout, env=self.update_agent_env() )
        if err :
            logger.info( err )
    
    def copy_key( self, identity_file , user, frontend ):
        logger.debug("Coping '%s' to '%s'" % ( identity_file, frontend ) )
        out , err = exec_cmd( 'ssh-copy-id -i %s %s@%s' %(  identity_file , user, frontend ),
                              stdin=sys.stdin, stdout=sys.stdout, env=self.update_agent_env() )
        if err :
            logger.info( err ) 
    
    def list_key( self , identity_file ):
        logger.debug("Listing '%s' key" % identity_file)
        out , err = exec_cmd( 'ssh-add -L' , env=self.update_agent_env() )
        match = re.search( '.*%s' % basename( identity_file ) , out)
        if match :
            logger.info( match.group() )
        
    def stop( self ):
        logger.debug( 'Stopping ssh-agent ... ' )
        if self.is_alive():
            out , err = exec_cmd( 'ssh-agent -k' , env=self.update_agent_env() )
            logger.debug( out )
            if err :
                logger.info( err )
        else:
            logger.debug( 'ssh-agent is already stopped' )
        try:
            os.remove( self.agent_file )
        except :
            pass
        

class Daemon( object ):
    
    def __init__( self ):
        self.gwd_pid  = join( DRM4G_DIR  , 'var' , 'gwd.pid' )
                
    def status( self ):
        if self.is_alive() :
            logger.info( "DRM4G is running" )
        else :
            logger.info( "DRM4G is stopped" )
  
    def is_alive( self ):
        if not exists( self.gwd_pid ) :
            return False
        else :
            if process_is_runnig( self.gwd_pid ) :
                return True 
            else :
                return False
  
    def start( self ):
        logger.info( "Starting DRM4G .... " )
        if not exists( self.gwd_pid ) or ( exists( self.gwd_pid ) and not process_is_runnig( self.gwd_pid ) ) :
            lock = join( DRM4G_DIR , 'var' '/.lock' )
            if exists( lock ) : 
                os.remove( lock )
            os.environ[ 'PATH' ] = '%s:%s' % ( DRM4G_BIN , os.getenv( 'PATH' ) )
            logger.debug( "Starting gwd .... " )
            out , err = exec_cmd( join( DRM4G_BIN , 'gwd' ) )
            if err :
                logger.info( err )
            if out :
                logger.info( out ) 
            if not err and not out :
                logger.info( "OK" )
        else :
            logger.info( "WARNING: DRM4G is already running." )
                
    def stop( self ):
        logger.info( "Stopping DRM4G .... " )
        logger.debug( "Stopping gwd .... " )
        out , err = exec_cmd( "%s -k" % join( DRM4G_BIN , "gwd" ) )
        if err :
            logger.info( err )
        if out :
            logger.info( out )
        if not err and not out :
            logger.info( "OK" )
            
    def clear( self ):
        self.stop()
        yes_choise = yes_no_choice( "Do you want to continue clearing DRM4G " )
        if yes_choise :
            logger.info( "Clearing DRM4G .... " )
            cmd = "%s -c" % join( DRM4G_BIN , "gwd" )
            out , err = exec_cmd( cmd )
            logger.debug( out ) 
            if err :
                logger.info( err )
            if out :
                logger.info( out )
            if not err and not out :
                logger.info( "OK" )
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
                logger.debug( "Resource '%s' :" % ( resname ) )
                communicator = communicators.get( resname )
                try :
                    communicator.connect()
                    logger.debug( "--> The front-end '%s' is accessible\n" % communicator.frontend )
                except Exception , err :
                    logger.error( "--> The front-end '%s' is not accessible\n" % communicator.frontend )
                            
    def edit( self ) :
        """
        Edit resources file.
        """
        logger.debug( "Editing '%s' file" % DRM4G_CONFIG_FILE )
        os.system( "%s %s" % ( os.environ.get('EDITOR', 'vi') , DRM4G_CONFIG_FILE ) )
        self.check( )

    def list( self ) :
        """
        Check if the resource.conf file has been configured well and list the resources available.
        """
        self.check( )
        logger.info( "\tName                          State" )
        logger.info( "---------------------------------------------" )
        for resname, resdict in sorted( self.config.resources.iteritems() ) :
            if resdict[ 'enable' ] == 'true' :
                state = 'enabled'
            else :
                state = 'disabled'
            logger.info( "\t%-30.30s%s" % ( resname , state ) )
                    
    def features( self ) :
        """
        List the features of a given resource.
        """
        self.check( )
        for resname , resdict in sorted( self.config.resources.iteritems() ) :
            logger.info( "Resource '%s' :" % ( resname ) )
            for key , val in sorted( resdict.iteritems() ) :
                logger.info( "\t--> '%s' : '%s'" % ( key , val ) )         
    
    def check( self ) :
        """
        Check if the resource.conf file has been configured well.
        """
        self.config.load()
        errors = self.config.check()
        if errors :
            raise Exception( "Please, review your configuration file" )
    
class Proxy( object ):
    
    def __init__( self , config , name ):
        self.config    = config
        self.resource  = self.config.resources[ name ]
        self.communicator = self.config.make_communicators()[ name ]
        self.communicator.connect()
        if self.resource.has_key( 'myproxy_server' ) :
            self.prefix = "X509_USER_PROXY=%s MYPROXY_SERVER=%s " % (
                                                                 join( REMOTE_VOS_DIR , self.resource[ 'myproxy_server' ] ) ,
                                                                 self.resource[ 'myproxy_server' ]
                                                                 )
        else :
            self.prefix = "X509_USER_PROXY=%s/${MYPROXY_SERVER} " % REMOTE_VOS_DIR
        
    def create( self , proxy_lifetime ):
        logger.debug("Creating '%s' directory to store the proxy ... " % REMOTE_VOS_DIR )
        cmd = "mkdir -p %s" % REMOTE_VOS_DIR
        logger.debug( "Executing command ... " + cmd ) 
        out, err = self.communicator.execCommand( cmd )
        if not err :
            message      = 'Insert your Grid password: '
            grid_passwd  = getpass.getpass(message)
        
            message      = 'Insert MyProxy password: '
            proxy_passwd = getpass.getpass(message)
        
            cmd = self.prefix + "myproxy-init -S --cred_lifetime %s --proxy_lifetime %s --local_proxy" % ( 
                                                                                                         proxy_lifetime ,
                                                                                                         proxy_lifetime
                                                                                                         )
            logger.debug( "Executing command ... ", cmd ) 
            out , err = self.communicator.execCommand( cmd , input = '\n'.join( [ grid_passwd, proxy_passwd ] ) )
            logger.info( out )
            if err :
                logger.info( err )
        else :
            raise Exception( err )
            
    def check( self ):
        cmd = self.prefix + "grid-proxy-info"
        logger.debug( "Executing command ... " + cmd )
        out, err = self.communicator.execCommand( cmd )
        logger.info( out )
        if err :
            logger.info( err ) 
    
    def destroy( self ):
        cmd = self.prefix + "myproxy-destroy" 
        logger.debug( "Executing command ... " + cmd )
        out , err = self.communicator.execCommand( cmd )
        logger.info( out )
        if err : 
            logger.info( err )
        cmd = self.prefix + "grid-proxy-destroy"
        logger.debug( "Executing command ... " + cmd )
        out , err = self.communicator.execCommand( cmd )
        logger.info( out )
        if err :
            logger.info( err )

help_info = """
DRM4G is an open platform, which is based on GridWay Metascheduler, to define, submit, and manage computational jobs. 
For additional information, see http://www.meteo.unican.es/trac/wiki/DRM4G .

Usage:
    drm4g daemon [ start | stop | status | restart | clear ] [ --dbg ]
    drm4g resource [ list | edit | info ] [ --dbg ] 
    drm4g resource <name> ssh-key [ info | add | delete | copy [ --public-key=<file> ] ] [ --dbg ]
    drm4g resource <name> proxy [ info | destroy | init [ --proxy-lifetime=<hours> ] ]  [ --dbg ]
    drm4g host [ list ] [ <hid> ] [ --dbg ]
    drm4g job submit [ --dep <job_id> ... ] <template> [ --dbg ]
    drm4g job info [ <job_id> ] [ --dbg ]
    drm4g job cancel <job_id> ... [ --dbg ]
    drm4g job hold <job_id> ... [ --dbg ]
    drm4g job release <job_id> ... [ --dbg ]    
    drm4g job re-schedule <job_id> ... [ --dbg ] 
    drm4g job get-history <job_id> [ --dbg ]
    drm4g help <command>

Arguments:
    <hid>                      Host identifier.
    <job_id>                   Job identifier.
    <template>                 Job template.
    <command>                  DRM4G command.

Options:
    -h --help
    --proxy-lifetime=<hours>   Duration of the proxy's lifetime [default: 168].
    --public-key=<file>        Public key file.
    --dep=<job_id> ...         Define the job dependency list of the job.
    --dbg                      Debug mode.
"""

class ManagementUtility( cmd.Cmd ):

    prompt = "drm4g > "
    intro  = """
Welcome to DRM4G interactive shell.

Type:  'help' for help with commands
       'exit' to quit
"""
    config = drm4g.core.configure.Configuration()

    @docopt_cmd
    def do_resource(self, arg):
        """
    Manage resources on DRM4G.
    
    Usage: 
        resource [ list | edit | info ] [--dbg]
        resource <name> ssh-key [ info | add | delete | copy [ --public-key=<file> ] ] [--dbg]
        resource <name> proxy [ info | destroy | init [ --proxy-lifetime=<hours> ] ] [--dbg]  

    Options:
        --proxy-lifetime=<hours>   Duration of the proxy's lifetime [default: 168].
        --public-key=<file>        Public key file.
        --dbg                      Debug mode.
        """
        if arg[ '--dbg' ] :
            logger.setLevel(logging.DEBUG)
        try :
            resource = Resource( self.config )
            resource.check_frontends( )
            if not arg['<name>'] :
                if arg['edit'] :
                    resource.edit()
                elif arg['info'] :
                    resource.features() 
                else :
                    resource.list()
            else :  
                self.config.load()
                if not self.config.resources.has_key( arg['<name>'] ):
                    raise Exception( "'%s' is not a configured resource." % ( arg['<name>'] ) )
                if arg['ssh-key']:
                    if not self.config.resources.get( arg['<name>'] ).has_key( 'private_key' ):
                        raise Exception( "'%s' does not have a 'private_key' value." % ( arg['<name>'] ) )
                    identity_file = self.config.resources.get( arg['<name>'] )[ 'private_key' ]
                    if not exists( expanduser( identity_file ) ) :
                        raise Exception( "'%s' does not exist." % ( identity_file ) )
                    if self.config.resources.get( arg['<name>'] )[ 'communicator' ] != 'ssh' :
                        raise Exception( "'ssh-key' command is only available for resources with ssh protocol" )
                    agent = Agent()
                    agent.start()
                    if arg['info']:
                        agent.list_key( identity_file )
                    elif arg['add']:
                        agent.add_key(identity_file)
                    elif arg['delete']:
                        agent.delete_key(identity_file)
                    elif arg['copy']:
                        identity_file = arg['--public-key'] if arg['--public-key'] else identity_file
                        agent.copy_key( identity_file , self.config.resources.get( arg['<name>'] )[ 'username' ] ,
                                        self.config.resources.get( arg['<name>'] )[ 'frontend' ] )
                    else:
                        agent.list_key( identity_file )
                if arg['proxy']:
                    proxy = Proxy( self.config , arg['<name>'] )
                    if arg['info'] :
                        proxy.check( )
                    elif arg['destroy'] :
                        proxy.destroy( )
                    elif arg['init'] :
                        proxy.create( arg['--proxy-lifetime'] )
                    else :
                        proxy.check( )                   
        except Exception , err :
            logger.error( str( err ) )

    @docopt_cmd
    def do_host(self, arg):
        """
    Print information about the hosts available on DRM4G.
     
    Usage: 
        host [ list ] [ <hid> ] [--dbg]
    
    Arguments:
        <hid>         Host identifier.

    Options:
        --dbg         Debug mode.        
 
    Host field information:
        HID 	      Host identifier.
        ARCH 	      Architecture.
        JOBS(R/T)     Number of jobs: R = running, T = total.
        LRMS 	      Local Resource Management System.
        HOSTNAME      Host name.
        QUEUENAME     Queue name.
        WALLT 	      Queue wall time.
        CPUT 	      Queue cpu time.
        MAXR          Max. running jobs.
        MAXQ 	      Max. queued jobs. 
        """
        if arg[ '--dbg' ] :
            logger.setLevel(logging.DEBUG)
        try :
            daemon = Daemon()
            if not daemon.is_alive() :
               raise Exception('DRM4G daemon is stopped.')
            cmd = '%s/gwhost '  % ( DRM4G_BIN )
            if arg[ '<hid>' ] :
                cmd = cmd + arg[ '<hid>' ] 
            out , err = exec_cmd( cmd )
            logger.info( out )
            if err :
                logger.info( err )
        except Exception , err :
            logger.error( str( err ) )
                
    @docopt_cmd
    def do_job(self, arg):
        """
    Submit, get status and history and cancel jobs.
    
    Usage: 
        job submit [ --dep <job_id> ... ] <template> [--dbg] 
        job info [ <job_id> ] [--dbg] 
        job cancel  <job_id> ... [--dbg]
        job hold <job_id> ... [ --dbg ]
        job release <job_id> ... [ --dbg ]    
        job re-schedule <job_id> ... [ --dbg ] 
        job get-history <job_id> [ --dbg ]
   
    Arguments:
        <job_id>               Job identifier.
        <template>             Job template.

    Options:
        --dep=<job_id> ...     Define the job dependency list of the job.
        --dbg                  Debug mode.
    
    Job field information:
        JID 	               Job identification.
        DM 	               Dispatch Manager state, one of: pend, hold, prol, prew, wrap, epil, canl, stop, migr, done, fail.
        EM 	               Execution Manager state: pend, susp, actv, fail, done.
        START 	               The time the job entered the system.
        END 	               The time the job reached a final state (fail or done).
        EXEC 	               Total execution time, includes suspension time in the remote queue system.
        XFER 	               Total file transfer time, includes stage-in and stage-out phases.
        EXIT 	               Job exit code.
        TEMPLATE               Filename of the job template used for this job.
        HOST 	               Hostname where the job is being executed.
        HID 	               Host identification.
        PROLOG 	               Total prolog (file stage-in phase) time.
        WRAPPER                Total wrapper (execution phase) time.
        EPILOG 	               Total epilog (file stage-out esphase) time.
        MIGR 	               Total migration time.
        REASON 	               The reason why the job left this host.
        QUEUE 	               Queue name. 
        """
        if arg[ '--dbg' ] :
            logger.setLevel(logging.DEBUG)
        try :
            daemon = Daemon( )
            if not daemon.is_alive() :
               raise Exception('DRM4G daemon is stopped.')
            if arg['submit']:
                resource = Resource( self.config )
                resource.check_frontends( )
                dependencies = '-d "%s"' % ' '.join( arg['--dep'] ) if arg['--dep'] else ''
                cmd = '%s/gwsubmit %s -v %s' % ( DRM4G_BIN , dependencies  , arg['<template>'] )
            elif arg['info']:
                cmd = '%s/gwps -o Jestxjh '  % ( DRM4G_BIN )
                if arg['<job_id>'] :
                    cmd = cmd + arg['<job_id>'][0] 
            elif arg['get-history']:
                cmd = '%s/gwhistory %s' % ( DRM4G_BIN , arg['<job_id>'][ 0 ] )
            elif arg['cancel']:
                cmd = '%s/gwkill -9 %s' % ( DRM4G_BIN , ' '.join( arg['<job_id>'] ) )  
            elif arg['hold']: 
                cmd = '%s/gwkill -o %s' % ( DRM4G_BIN , ' '.join( arg['<job_id>'] ) )
            elif arg['release']:
                cmd = '%s/gwkill -l %s' % ( DRM4G_BIN , ' '.join( arg['<job_id>'] ) )
            else :
                cmd = '%s/gwkill -s %s' % ( DRM4G_BIN , ' '.join( arg['<job_id>'] ) )
            out , err = exec_cmd( cmd )
            logger.info( out )
            if err :
                logger.info( err )
        except Exception , err :
            logger.error( str( err ) )
            
    @docopt_cmd
    def do_daemon(self, arg):
        """
    Manage DRM4G daemon. Keep in mind that the clear command deletes all the jobs available in DRM4G. 
    
    Usage: 
        daemon [ start | stop | status | restart | clear ] [ --dbg ] 
   
    Options:
        --dbg    Debug mode.
        """
        try:
            if arg[ '--dbg' ] :
                logger.setLevel(logging.DEBUG)
            daemon = Daemon()
            agent  = Agent()
            if arg[ 'start' ] :
                agent.start()
                daemon.start()
            elif arg[ 'stop' ] :
                agent.stop()
                daemon.stop()
            elif arg[ 'status' ] :
                daemon.status()
            elif arg[ 'restart' ] :
                daemon.stop()
                daemon.start()
            elif arg[ 'clear' ] :
                agent.start()
                daemon.clear()
            else :
                daemon.status()
        except Exception , err :
            logger.error( str( err ) )
        
    def default( self , arg ):    
        self.do_help( arg )

    def do_exit (self , arg ):
        """
        Quit the console.
        """
        sys.exit()

def execute_from_command_line( argv ):
    """
    A method that runs a ManagementUtility.
    """
    if len( argv ) > 1:
        docopt(help_info)
        ManagementUtility().onecmd( ' '.join( argv[ 1: ] ) )
    else:
        ManagementUtility().cmdloop()
    

