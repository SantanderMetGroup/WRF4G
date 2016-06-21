"""
Start DRM4G daemon deleting all the jobs available on DRM4G.
    
Usage: 
    wrf4g clear [ --dbg ] 
   
Options:
   --dbg    Debug mode.
"""
__version__  = '2.4.1'
__author__   = 'Carlos Blanco'
__revision__ = "$Id: clear.py 2352 2015-02-24 10:23:57Z carlos $"

import logging
from time                 import sleep
from drm4g.commands       import Daemon, logger

def run( arg ) :
    try:
        if arg[ '--dbg' ] :
            logger.setLevel(logging.DEBUG)
        daemon = Daemon()
        daemon.stop()
        sleep( 2.0 )
        daemon.clear()
    except Exception as err :
        logger.error( str( err ) )

