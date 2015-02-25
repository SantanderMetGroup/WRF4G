"""
Restart DRM4G daemon. 
    
Usage: 
    wrf4g restart [ --dbg ] 
   
Options:
   --dbg    Debug mode.
"""
__version__  = '2.3.1'
__author__   = 'Carlos Blanco'
__revision__ = "$Id: restart.py 2352 2015-02-24 10:23:57Z carlos $"

import logging
from time                 import sleep
from drm4g.commands       import Daemon, Agent, logger

def run( arg ) :
    try:
        if arg[ '--dbg' ] :
            logger.setLevel(logging.DEBUG)
        daemon = Daemon()
        daemon.stop()
        sleep( 2.0 )
        daemon.start()
    except Exception , err :
        logger.error( str( err ) )

