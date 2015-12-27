"""
Manage computing resources on WRF4G.
    
Usage: 
    wrf4g resource [ --dbg ] [ list | edit | check ]
    
 Options:
    --dbg     Debug mode.
    
Commands:
    list      Show resources available.    
    edit      Configure resouces.
    check     Check out if configured resources are accessible.
"""
__version__  = '2.2.0'
__author__   = 'Carlos Blanco'
__revision__ = "$Id$"

import logging
import sys
from drm4g.core.configure import Configuration
from drm4g.commands       import Daemon, Resource

def run( arg ) :
    logging.basicConfig( format = '%(message)s',
                         level  = logging.DEBUG if arg[ '--dbg' ] else logging.INFO,
                         stream = sys.stdout )
    try :
        config = Configuration()
        daemon = Daemon()
        if not daemon.is_alive() :
           raise Exception( 'DRM4G is stopped.' )
        resource = Resource( config )
        if arg[ 'edit' ] :
            resource.edit()
        elif arg[ 'check' ] :
            resource.check_frontends( )
        else :
            resource.list()
    except KeyboardInterrupt :
        pass
    except Exception as err :
        logging.error( str( err ) )

