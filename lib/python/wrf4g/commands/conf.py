"""
Configure DRM4G daemon, scheduler, database and logger parameters.
                
Usage:  
   wrf4g conf ( daemon | sched | logger | database ) [ --dbg ]

Options:
   --dbg    Debug mode
"""
__version__  = '2.0.0'
__author__   = 'Carlos Blanco'
__revision__ = "$Id$"

import logging
import os
from wrf4g import logger, DB4G_CONF, WRF4G_LOGGER 
from drm4g import DRM4G_DAEMON, DRM4G_SCHED

def run( arg ) :
    if arg[ '--dbg' ] :
        logger.setLevel(logging.DEBUG)
    if arg[ 'daemon' ] :
        conf_file = DRM4G_DAEMON
    elif arg[ 'logger' ]:
        conf_file = WRF4G_LOGGER
    elif arg[ 'database' ]:
        conf_file = DB4G_CONF
    else :
        conf_file = DRM4G_SCHED
    logger.debug( "Editing '%s' file" % conf_file )
    os.system( "%s %s" % ( os.environ.get('EDITOR', 'vi') , conf_file ) )
