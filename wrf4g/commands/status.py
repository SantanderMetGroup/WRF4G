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
Check DRM4G daemon and ssh-agent. 
    
Usage: 
    wrf4g status [ --dbg ] 
   
Options:
   --dbg    Debug mode.
"""

import logging
import sys
from drm4g.commands       import Daemon, Agent

def run( arg ) :
    try:
        logging.basicConfig( format = '%(message)s',
                         level  = logging.DEBUG if arg[ '--dbg' ] else logging.INFO,
                         stream = sys.stdout )
        Daemon().status()
        Agent().status()
    except KeyboardInterrupt :
        pass
    except Exception as err :
        logging.error( str( err ) )

