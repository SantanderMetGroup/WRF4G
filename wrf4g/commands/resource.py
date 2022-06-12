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
Manage computing resources on WRF4G.
    
Usage: 
    wrf4g resource [ --dbg ] [ list | edit | check ]
    
Options:
    --dbg     Debug mode.
    
Commands:
    list      Show resources available.    
    edit      Configure resouces.
    check     Check out if configured resources are accessible.
"""

import logging
import sys
from drm4g.core.configure import Configuration
from drm4g.commands       import Daemon, Resource

def run( arg ) :
    # El logger no funcionaba porque el import logging se hace varias veces y coge la primera. Hacemos un reload para que funcione
    # https://stackoverflow.com/questions/20240464/python-logging-file-is-not-working-when-using-logging-basicconfig
    from imp import reload
    reload(logging)
    logging.basicConfig( format = '%(message)s',
                     level  = logging.DEBUG if arg[ '--dbg' ] else logging.INFO,
                     stream = sys.stdout )
                     
    try :
        config = Configuration()
        daemon = Daemon()
        if not daemon.is_alive() :
           raise Exception( 'DRM4G is stopped.' )
        resource = Resource( config )
        if arg[ 'edit' ] :
            resource.edit()
        elif arg[ 'check' ] :
            resource.check_frontends( )
        else :
            resource.list()
    except KeyboardInterrupt :
        pass
    except Exception as err :
        logging.error( str( err ) )

