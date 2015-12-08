"""
Check DRM4G and MySQL daemons and ssh-agent. 
    
Usage: 
    wrf4g status [ --dbg ] 
   
Options:
   --dbg    Debug mode.
"""
__version__  = '2.1.0'
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
    except KeyboardInterrupt :
        pass
    except Exception as err :
        logging.error( str( err ) )

