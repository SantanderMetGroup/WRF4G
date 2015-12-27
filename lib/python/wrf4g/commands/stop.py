"""
Stop DRM4G daemon and ssh-agent. 
    
Usage: 
    wrf4g stop [ --dbg ] 
   
Options:
   --dbg    Debug mode.
"""
__version__  = '2.2.0'
__author__   = 'Carlos Blanco'
__revision__ = "$Id$"

import logging
import sys
from drm4g.commands    import Daemon, Agent

def run( arg ) :
    try:
        logging.basicConfig( format = '%(message)s',
                         level  = logging.DEBUG if arg[ '--dbg' ] else logging.INFO,
                         stream = sys.stdout )
        Daemon().stop()
        Agent().stop()
    except KeyboardInterrupt :
        pass
    except Exception as err :
        logging.error( str( err ) )
