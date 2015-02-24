"""
Check DRM4G daemon and ssh-agent. 
    
Usage: 
    drm4g status [ --dbg ] 
   
Options:
   --dbg    Debug mode.
"""
__version__  = '2.3.1'
__author__   = 'Carlos Blanco'
__revision__ = "$Id: status.py 2352 2015-02-24 10:23:57Z carlos $"

import logging
from drm4g.commands       import Daemon, Agent, logger

def run( arg ) :
    try:
        if arg[ '--dbg' ] :
            logger.setLevel(logging.DEBUG)
        Daemon().status()
        Agent().status()
    except Exception , err :
        logger.error( str( err ) )

