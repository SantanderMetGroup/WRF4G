#
# Copyright 2016 Universidad de Cantabria
#
# Licensed under the EUPL, Version 1.1 only (the
# "Licence");
# You may not use this work except in compliance with the
# Licence.
# You may obtain a copy of the Licence at:
#
# http://ec.europa.eu/idabc/eupl
#
# Unless required by applicable law or agreed to in
# writing, software distributed under the Licence is
# distributed on an "AS IS" basis,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either
# express or implied.
# See the Licence for the specific language governing
# permissions and limitations under the Licence.
#

"""
Start DRM4G daemon and ssh-agent. 
    
Usage: 
    wrf4g start [ --dbg ] [ --clear-conf ] [ --disc-jobs ] 
            
Options:
   --dbg                Debug mode.
   --clear-conf         Clear WRF4G's settings stored in .wrf4g directory.
   --disc-jobs          All available jobs on WRF4G will be discared. 
"""

import glob
import os
import sys
import socket
import logging
from os.path              import exists, join, abspath, join, dirname
from drm4g                import DRM4G_DIR
from drm4g.commands       import Daemon, Agent
from wrf4g                import WRF4G_DIR, DB4G_CONF, WRF4G_LOGGER
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
            if 'VIRTUAL_ENV' in os.environ.keys():
                deployment_dir = glob.glob( join( WRF4G_DEPLOYMENT_DIR, 'lib/python*/site-packages/wrf4g' ) )[0]
            else:
                deployment_dir = glob.glob( join( WRF4G_DEPLOYMENT_DIR, 'local/lib/python*/dist-packages/wrf4g' ) )[0]
            src = join( deployment_dir , 'etc' )
            print src
            dest = join( WRF4G_DIR, 'etc' )
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

