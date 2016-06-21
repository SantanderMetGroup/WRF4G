"""
Configure DRM4G daemon, scheduler and logger parameters.
                
Usage:  
   drm4g conf ( daemon | sched | logger ) [ --dbg ]

Options:
   --dbg    Debug mode
"""
__version__  = '2.4.1'
__author__   = 'Carlos Blanco'
__revision__ = "$Id: conf.py 2352 2015-02-24 10:23:57Z carlos $"

import logging
import os
from drm4g                import DRM4G_DAEMON, DRM4G_LOGGER, DRM4G_SCHED
from drm4g.commands       import logger

def run( arg ) :
    if arg[ '--dbg' ] :
        logger.setLevel(logging.DEBUG)
    if arg[ 'daemon' ] :
        conf_file = DRM4G_DAEMON
    elif arg[ 'logger' ]:
        conf_file = DRM4G_LOGGER
    else :
        conf_file = DRM4G_SCHED
    logger.debug( "Editing '%s' file" % conf_file )
    os.system( "%s %s" % ( os.environ.get('EDITOR', 'vi') , conf_file ) )
