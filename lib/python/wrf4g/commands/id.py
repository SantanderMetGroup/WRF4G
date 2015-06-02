"""
Manage identities for resources. That involves managing private/public keys 
and grid credentials, depending on the resource configuration.

Usage:  
    wrf4g id <resource_name> init   [ --dbg ] [ --lifetime=<hours> ]
    wrf4g id <resource_name> info   [ --dbg ] 
    wrf4g id <resource_name> delete [ --dbg ] 

 Options:
    -l --lifetime=<hours>   Duration of the identity's lifetime [default: 168].
    --dbg                   Debug mode.
    
Commands:
    init                    Create an identity for a while, by default 168 hours 
                            (1 week). Use the option --lifetime to modify this 
                            value. It adds the configured private key to a ssh-agent
                            and creates a grid proxy using myproxy server.
                            Append the public key to the remote user's 
                            ~/.ssh/authorized_keys file (creating the file, and 
                            directory, if necessary). It tries to load the public 
                            key obtained by appending *.pub to the name of the 
                            configured private key file. Alternative the public 
                            key can be given by public_key variable.
                            It also configures the user's grid certificate 
                            under ~/.globus directory (creating directory, 
                            if necessary) if grid_cert variable is defined.
                                  
    info                    It gives some information about the identity status.
                                
    delete                  The identity is removed from the ssh-agent and the 
                            myproxy server.
"""
__version__  = '2.0.0'
__author__   = 'Carlos Blanco'
__revision__ = "$Id$"

import logging, sys
from os.path              import expanduser, exists, expandvars
from drm4g.core.configure import Configuration
from drm4g.commands       import Daemon, Agent, Proxy

def run( arg ) :
    logging.basicConfig( format = '%(message)s',
                         level  = logging.DEBUG if arg[ '--dbg' ] else logging.INFO,
                         stream = sys.stdout )
    try :
        daemon = Daemon()
        if not daemon.is_alive() :
           raise Exception( 'DRM4G is stopped.' )
        config = Configuration()
        config.load( )
        if config.check( ) :
            raise Exception( "Review the configuration of '%s'." % ( arg['<resource_name>'] ) )
        if not config.resources.has_key( arg['<resource_name>'] ):
            raise Exception( "'%s' is not a configured resource." % ( arg['<resource_name>'] ) )
        lrms         = config.resources.get( arg['<resource_name>'] )[ 'lrms' ]
        communicator = config.resources.get( arg['<resource_name>'] )[ 'communicator' ]
        if lrms != 'cream' and communicator != 'ssh' :
            raise Exception( "'%s' does not have an identity to configure." % ( arg['<resource_name>'] ) )
        if lrms == 'cream' :
            proxy = Proxy( config.resources[ arg['<resource_name>'] ] , 
                           config.make_communicators()[ arg['<resource_name>'] ] 
                           )
        if communicator == 'ssh' :
            agent = Agent( config.resources[ arg['<resource_name>'] ] )
        if arg[ 'init' ] :
            if communicator == 'ssh' :
                agent.start( )
                agent.add_key( arg[ '--lifetime' ] )
                agent.copy_key( )
            if lrms == 'cream' :
                proxy.configure( )
                proxy.create( arg[ '--lifetime' ] )
        elif arg[ 'delete' ] :
            if lrms == 'cream' :
                proxy.destroy( )
            if communicator == 'ssh' :
                agent.delete_key( )
        else :
            if communicator == 'ssh' :
                agent.list_key( )
            if lrms == 'cream' :
                proxy.check( )
    except Exception , err :
        logging.error( str( err ) )
