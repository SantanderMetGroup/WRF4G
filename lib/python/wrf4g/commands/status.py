"""
Check DRM4G daemon, database and ssh-agent. 
    
Usage: 
    wrf4g status [ --dbg ] 
   
Options:
   --dbg    Debug mode.
"""
__version__  = '2.0.0'
__author__   = 'Carlos Blanco'
__revision__ = "$Id$"

import logging
from wrf4g                import logger
from wrf4g.utils          import DataBase 
from drm4g.commands       import Daemon, Agent

def run( arg ) :
    try:
        if arg[ '--dbg' ] :
            logger.setLevel(logging.DEBUG)
        Daemon().status()
        Agent().status()
        DataBase().status()
    except Exception , err :
        logger.error( str( err ) )

