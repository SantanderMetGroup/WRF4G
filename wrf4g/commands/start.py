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
from distutils.dir_util   import copy_tree
from drm4g                import DRM4G_DIR
from drm4g.commands       import Daemon, Agent
from wrf4g                import WRF4G_DEPLOYMENT_DIR, WRF4G_DIR, DB4G_CONF, WRF4G_LOGGER
from wrf4g.db             import DEFAULT_DB_CONF
import requests            
import tarfile


def run( arg ) :
        
    try:
        # El logger no funcionaba porque el import logging se hace varias veces y coge la primera. Hacemos un reload para que funcione
        # https://stackoverflow.com/questions/20240464/python-logging-file-is-not-working-when-using-logging-basicconfig
        from imp import reload
        reload(logging)
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
            
            src = join( WRF4G_DEPLOYMENT_DIR , 'data' )
            logging.debug( "Coping from '%s' to '%s'" % ( src , WRF4G_DIR ) )
            copy_tree( src , WRF4G_DIR )
            
            logging.debug('Downloading and extracting data repository')
            r = requests.get('http://personales.gestion.unican.es/fernanqv/repository2.tar.gz')
            open('repository.tar.gz', 'wb').write(r.content)
            tar = tarfile.open('repository.tar.gz')
            tar.extractall(path=WRF4G_DIR)
            tar.close()
            os.remove('repository.tar.gz')
            
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

