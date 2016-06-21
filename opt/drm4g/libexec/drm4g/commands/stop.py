"""
Stop DRM4G daemon and ssh-agent. 
    
Usage: 
    drm4g stop [ --dbg ] 
   
Options:
   --dbg    Debug mode.
"""
__version__  = '2.4.1'
__author__   = 'Carlos Blanco'
__revision__ = "$Id: stop.py 2352 2015-02-24 10:23:57Z carlos $"

import logging
from drm4g.commands    import Daemon, Agent, logger

def run( arg ) :
    try:
        if arg[ '--dbg' ] :
            logger.setLevel(logging.DEBUG)
        Daemon().stop()
        Agent().stop()
        DataBase().stop()
    except Exception as err :
        logger.error( str( err ) )
