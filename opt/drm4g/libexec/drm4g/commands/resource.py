"""
Manage computing resources on WRF4G.
    
Usage: 
    wrf4g resource [ list | edit | check ] [ --dbg ] 
    wrf4g resource <name> id conf [ --public-key=<file> --grid-cerd=<file> --lifetime=<hours> --dbg ]
    wrf4g resource <name> id init [ --lifetime=<hours> --dbg ]
    wrf4g resource <name> id info [ --dbg ]
    wrf4g resource <name> id delete [ --dbg ]

 Options:
    -l --lifetime=<hours>   Duration of the identity's lifetime [default: 168].
    -p --public-key=<file>  Public key file.
    -g --grid-cerd=<file>   Grid certificate.
    --dbg                   Debug mode.
    
Commands:
    list                    Show resources available.    
    edit                    Configure resouces.
    check                   Check out if configured resources are accessible.
    id                      Manage identities for resources. That involves 
                            managing private/public keys and grid credentials, 
                            depending on the resource configuration.
                                         
                            With init, creates an identity for a while, by 
                            default 168 hours (1 week). Use the option --lifetime 
                            to modify this value. It adds the configured private
                            key to a ssh-agent and creates a grid proxy using 
                            myproxy server.
       
                            With conf, initializes an identity (init comnand) 
                            and appends the public key to the remote user's 
                            ~/.ssh/authorized_keys file (creating the file, and 
                            directory, if necessary). It tries to load the public 
                            key obtained by appending *.pub to the name of the 
                            configured private key file. Alternative the public 
                            key can be given by --public-key option.
                            
                            It also configures the user's grid certificate 
                            under ~/.globus directory (creating directory, 
                            if necessary) if --grid-cerd option is used.
                                  
                            With info, it gives some information about the 
                            identity status.
                                
                            With delete, the identity is removed from the 
                            ssh-agent and the myproxy server.
"""
__version__  = '2.3.1'
__author__   = 'Carlos Blanco'
__revision__ = "$Id: resource.py 2352 2015-02-24 10:23:57Z carlos $"

import logging
from drm4g.core.configure import Configuration
from drm4g.commands       import exec_cmd, Daemon, Resource, Proxy, logger 

def run( arg ) :
    if arg[ '--dbg' ] :
        logger.setLevel(logging.DEBUG)
    try :
        config = Configuration()
        daemon = Daemon()
        if not daemon.is_alive() :
           raise Exception( 'DRM4G is stopped.' )
        if not arg[ '<name>' ] :
            resource = Resource( config )
            if arg[ 'edit' ] :
                resource.edit()
            elif arg[ 'check' ] :
                resource.check_frontends( )
            else :
                resource.list()
        else :
            config.load( )
            if config.check( ) :
                raise Exception( "Review the configuration of '%s'." % ( arg['<name>'] ) )
            if not config.resources.has_key( arg['<name>'] ):
                raise Exception( "'%s' is not a configured resource." % ( arg['<name>'] ) )
            lrms         = config.resources.get( arg['<name>']  )[ 'lrms' ]
            communicator = config.resources.get( arg['<name>']  )[ 'communicator' ]
            if lrms != 'cream' and communicator != 'ssh' :
                raise Exception( "'%s' does not have an identity to configure." % ( arg['<name>'] ) )
            if lrms == 'cream' :
                proxy = Proxy( config , arg['<name>'] )
            if communicator == 'ssh' :
                if not config.resources.get( arg['<name>'] ).has_key( 'private_key' ) :
                    raise Exception( "'%s' does not have a 'private_key' value." % ( arg['<name>'] ) )
                private_key = expandvars( expanduser( config.resources.get( arg['<name>'] )[ 'private_key' ] ) )
                if not exists( private_key ) :
                    raise Exception( "'%s' does not exist." % ( private_key ) )
                agent = Agent()
            if arg[ 'conf' ] or arg[ 'init' ] :
                if communicator == 'ssh' :
                    logger.info( "--> Starting ssh-agent ... " )
                    agent.start( )
                    logger.info( "--> Adding private key to ssh-agent ... " )
                    agent.add_key( private_key, arg[ '--lifetime' ] )
                    if arg[ 'conf' ] :
                        identity = arg[ '--public-key' ] if arg[ '--public-key' ] else private_key
                        if not exists( expandvars( expanduser( identity ) ) ) :
                            raise Exception( "'%s' does not exist." % ( identity ) )
                        logger.info( "--> Copying public key on the remote frontend ... " )
                        agent.copy_key( identity , config.resources.get( arg[ '<name>' ] )[ 'username' ] ,
                                        config.resources.get( arg[ '<name>' ] )[ 'frontend' ] )
                if lrms == 'cream' :
                    if arg[ 'conf' ] :
                        logger.info( "--> Configuring grid certifitate ... " )
                        if arg[ '--grid-cerd' ] :
                            grid_cerd = expandvars( expanduser( arg[ '--grid-cerd' ] ) )
                            if not exists( grid_cerd ) :
                                raise Exception( "'%s' does not exist." % ( arg[ '--grid-cerd' ] ) )
                            proxy.configure( grid_cerd )
                        if not arg[ '--grid-cerd' ] :
                            logger.info( "\nWARNING: It is assumed that the grid certificate has been already configured\n" )
                    logger.info( "--> Creating a proxy ... " )
                    proxy.create( arg[ '--lifetime' ] )
            elif arg[ 'delete' ] :
                if communicator == 'ssh' :
                    agent.delete_key( private_key )
                if lrms == 'cream' :
                    proxy.destroy( )
            else :
                if communicator == 'ssh' :
                    logger.info( "--> Private key available on the ssh-agent" )
                    agent.list_key( private_key )
                if lrms == 'cream' :
                    logger.info( "--> Grid credentials" )
                    proxy.check( )
    except Exception , err :
        logger.error( str( err ) )

