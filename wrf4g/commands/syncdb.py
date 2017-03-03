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
Create WRF4G database tables on the configured database. 
    
Usage: 
    wrf4g syncdb [ --dbg ]
            
Options:
   --dbg     Debug mode.
"""

import sys
import logging
from wrf4g.db   import init_db

def run( arg ) :
    try:
        logging.basicConfig( format = '%(message)s',
                         level  = logging.DEBUG if arg[ '--dbg' ] else logging.INFO,
                         stream = sys.stdout )
        init_db()
    except KeyboardInterrupt :
        pass
    except Exception as err :
        logging.error( err )

