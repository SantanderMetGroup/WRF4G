"""
Start DRM4G daemon and ssh-agent. 
    
Usage: 
    wrf4g start [ --dbg ] [ --clear-conf ] [ --disc-jobs ] 
            
Options:
   --dbg                Debug mode.
   --clear-conf         Clear WRF4G's settings stored in .wrf4g directory.
   --disc-jobs          All available jobs on WRF4G will be discared. 
"""
__version__  = '2.2.2'
__author__   = 'Carlos Blanco'
__revision__ = "$Id$"

import os
import sys
import socket
import logging
from os.path              import exists, join
from drm4g                import DRM4G_DIR
from drm4g.commands       import Daemon, Agent
from wrf4g                import ( WRF4G_DIR, WRF4G_DEPLOYMENT_DIR,
                                   DB4G_CONF, WRF4G_LOGGER )
from wrf4g.db             import DEFAULT_DB_CONF

def run( arg ) :
    try:
        logging.basicConfig( format = '%(message)s',
                         level  = logging.DEBUG if arg[ '--dbg' ] else logging.INFO,
                         stream = sys.stdout )
        if not exists( WRF4G_DIR ) or arg[ '--clear-conf' ] :
            from shutil import copytree, rmtree
            if exists( WRF4G_DIR ) :
                logging.debug( "Removing WRF4G local configuration in '%s'" %  WRF4G_DIR )
                rmtree( WRF4G_DIR   )
            logging.debug( "Creating a WRF4G local configuration in '%s'" %  WRF4G_DIR )
            for directory in [  'log', 'submission', 'acct' ] :
                abs_dir = join ( WRF4G_DIR , 'var' , directory )
                logging.debug( "Creating '%s' directory" % abs_dir )
                os.makedirs( abs_dir )
            src  = join ( WRF4G_DEPLOYMENT_DIR , 'etc' )
            print src
            dest = join ( WRF4G_DIR            , 'etc' )
            print dest
            logging.debug( "Coping from '%s' to '%s'" % ( src , dest ) )
            copytree( src , dest )
        if arg[ '--disc-jobs' ] :
            Daemon().clear()
        else :
            Daemon().start()
        Agent().start()
        # Update database configuration
        with open( DB4G_CONF , 'w') as f :
           f.write( DEFAULT_DB_CONF % { "WRF4G_DIR" : WRF4G_DIR } )
    except KeyboardInterrupt :
        pass
    except Exception as err :
        logging.error( err )

