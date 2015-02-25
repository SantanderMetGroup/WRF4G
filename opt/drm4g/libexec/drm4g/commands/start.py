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
from drm4g                import DRM4G_DIR, DRM4G_DEPLOYMENT_DIR 
from drm4g.commands       import Daemon, Agent, logger

def run( arg ) :
    try:
        if arg[ '--dbg' ] :
            logger.setLevel(logging.DEBUG)
        if not exists( DRM4G_DIR ) :
            logger.info( "Creating a DRM4G local configuration in '%s'" %  DRM4G_DIR )
            abs_dir = join ( DRM4G_DIR , 'var' , 'acct' )
            logger.info( "Creating '%s' directory" % abs_dir )
            os.makedirs( abs_dir )
            from  shutil import copytree
            src  = join ( DRM4G_DEPLOYMENT_DIR , 'etc' )
            dest = join ( DRM4G_DIR            , 'etc' )
            logger.info( "Coping from '%s' to '%s'" % ( src , dest ) )
            copytree( src , dest )
        Daemon().start()
        Agent().start()
    except Exception , err :
        logger.error( str( err ) )

