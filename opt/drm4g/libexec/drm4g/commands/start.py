"""
Start DRM4G daemon and ssh-agent. 
    
Usage: 
    drm4g start [ --dbg ] 
   
Options:
   --dbg    Debug mode.
"""
__version__  = '2.3.1'
__author__   = 'Carlos Blanco'
__revision__ = "$Id: start.py 2352 2015-02-24 10:23:57Z carlos $"

import logging
from drm4g.commands       import Daemon, Agent, logger

def run( arg ) :
    try:
        if arg[ '--dbg' ] :
            logger.setLevel(logging.DEBUG)
        Daemon().start()
        Agent().start()
    except Exception , err :
        logger.error( str( err ) )

