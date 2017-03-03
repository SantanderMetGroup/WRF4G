#
# Copyright 2016 Universidad de Cantabria
#
# Licensed under the EUPL, Version 1.1 only (the
# "Licence");
# You may not use this work except in compliance with the
# Licence.
# You may obtain a copy of the Licence at:
#
# http://ec.europa.eu/idabc/eupl
#
# Unless required by applicable law or agreed to in
# writing, software distributed under the Licence is
# distributed on an "AS IS" basis,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either
# express or implied.
# See the Licence for the specific language governing
# permissions and limitations under the Licence.
#

"""
Manage identities for resources configuring private/public keys and grid credentials.

Usage:  
    wrf4g id <resource> init   [ --dbg ] [ --lifetime=<hours> ]
    wrf4g id <resource> info   [ --dbg ] 
    wrf4g id <resource> delete [ --dbg ] 

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
            raise Exception( "Review the configuration of '%s'." % ( arg['<resource>'] ) )
        if arg['<resource>'] not in config.resources :
            raise Exception( "'%s' is not a configured resource." % ( arg['<resource>'] ) )
        lrms         = config.resources.get( arg['<resource>'] )[ 'lrms' ]
        communicator = config.resources.get( arg['<resource>'] )[ 'communicator' ]
        if lrms != 'cream' and communicator != 'ssh' :
            raise Exception( "'%s' does not have an identity to configure." % ( arg['<resource>'] ) )
        if lrms == 'cream' :
            proxy = Proxy( config.resources[ arg['<resource>'] ] , 
                           config.make_communicators()[ arg['<resource>'] ] 
                           )
        if communicator == 'ssh' :
            agent = Agent( config.resources[ arg['<resource>'] ] )
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
    except KeyboardInterrupt :
        pass   
    except Exception as err :
        logging.error( str( err ) )
