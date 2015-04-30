"""
Start DRM4G and MySQL daemons and ssh-agent. 
    
Usage: 
    wrf4g start [ --dbg ] [ --clear-conf ] [ --disc-jobs ] 
                [ --ext-db ] [ --db-port=port ] [ --db-host=hostname ]
   
Options:
   --dbg                Debug mode.
   --clear-conf         Clear WRF4G's settings stored in .wrf4g directory.
   --disc-jobs          All available jobs on WRF4G will be discared. 
   --ext-db             It will be use an external MySQL database.
   --db-port=port       Port number to use for MySQL connection [default: 25000].     
   --db-host=hostname   Hostname for MySQL connection [default: localhost].
"""
__version__  = '2.0.0'
__author__   = 'Carlos Blanco'
__revision__ = "$Id$"

import logging
import os
from os.path              import exists, join
from drm4g                import DRM4G_DIR
from drm4g.commands       import Daemon, Agent
from wrf4g                import WRF4G_DIR, WRF4G_DEPLOYMENT_DIR, DB4G_CONF, WRF4G_LOGGER, logger
from wrf4g.db             import DEFAULT_DB_CONF
from wrf4g.utils          import DataBase

def run( arg ) :
    try:
        if arg[ '--dbg' ] :
            logger.setLevel(logging.DEBUG)
        if not exists( WRF4G_DIR ) or arg[ '--clear-conf' ] :
            from shutil import copytree, rmtree
            if exists( WRF4G_DIR ) :
                logger.debug( "Removing WRF4G local configuration in '%s'" %  WRF4G_DIR )
                rmtree( WRF4G_DIR   ) 
            logger.debug( "Creating a WRF4G local configuration in '%s'" %  WRF4G_DIR )
            for directory in [  'log', 'submission', 'acct' ] :
                abs_dir = join ( DRM4G_DIR , 'var' , directory )
                logger.debug( "Creating '%s' directory" % abs_dir )
                os.makedirs( abs_dir )
            src  = join ( WRF4G_DEPLOYMENT_DIR , 'etc' )
            dest = join ( WRF4G_DIR            , 'etc' )
            logger.debug( "Coping from '%s' to '%s'" % ( src , dest ) )
            copytree( src , dest )
        if arg[ '--disc-jobs' ] :
            Daemon().clear() 
        else : 
            Daemon().start()
        Agent().start()
        # Update database configuration
        with open( DB4G_CONF , 'w') as f :
            f.write( DEFAULT_DB_CONF % { "port"     : arg[ '--db-port' ] ,
                                         "hostname" : arg[ '--db-host' ] } )
        if not arg[ '--ext-db' ] :
            DataBase( int( arg[ '--db-port' ] ) ).start()
    except Exception , err :
        logger.error( err )
