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
Configure DRM4G daemon, scheduler, database and logging parameters.
                
Usage:  
   wrf4g conf ( daemon | sched | logger | database ) [ --dbg ]

Options:
   --dbg    Debug mode
"""

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
