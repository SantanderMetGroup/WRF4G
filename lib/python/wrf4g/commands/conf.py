"""
Configure DRM4G daemon, scheduler, database and logging parameters.
                
Usage:  
   wrf4g conf ( daemon | sched | logger | database ) [ --dbg ]

Options:
   --dbg    Debug mode
"""
__version__  = '2.2.0'
__author__   = 'Carlos Blanco'
__revision__ = "$Id$"

import logging
import sys
from wrf4g            import DB4G_CONF, WRF4G_LOGGER 
from drm4g            import DRM4G_DAEMON, DRM4G_SCHED
from wrf4g.utils.file import edit_file

def run( arg ) :
    logging.basicConfig( format = '%(message)s', 
                         level  = logging.DEBUG if arg[ '--dbg' ] else logging.INFO,
                         stream = sys.stdout )
    if arg[ 'daemon' ] :
        conf_file = DRM4G_DAEMON
    elif arg[ 'logger' ]:
        conf_file = WRF4G_LOGGER
    elif arg[ 'database' ]:
        conf_file = DB4G_CONF
    else :
        conf_file = DRM4G_SCHED
    edit_file( conf_file )
