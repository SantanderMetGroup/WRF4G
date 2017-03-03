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

