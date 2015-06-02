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
import sys
from wrf4g.db             import MySQLDB
from drm4g.commands       import Daemon, Agent

def run( arg ) :
    try:
        logging.basicConfig( format = '%(message)s',
                         level  = logging.DEBUG if arg[ '--dbg' ] else logging.INFO,
                         stream = sys.stdout )
        Daemon().status()
        Agent().status()
        MySQLDB().status()
    except Exception , err :
        logging.error( str( err ) )

