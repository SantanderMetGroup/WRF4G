"""
Check DRM4G daemon and ssh-agent. 
    
Usage: 
    wrf4g status [ --dbg ] 
   
Options:
   --dbg    Debug mode.
"""
__version__  = '2.2.2'
__author__   = 'Carlos Blanco'
__revision__ = "$Id$"

import logging
import sys
from drm4g.commands       import Daemon, Agent

def run( arg ) :
    try:
        logging.basicConfig( format = '%(message)s',
                         level  = logging.DEBUG if arg[ '--dbg' ] else logging.INFO,
                         stream = sys.stdout )
        Daemon().status()
        Agent().status()
    except KeyboardInterrupt :
        pass
    except Exception as err :
        logging.error( str( err ) )

