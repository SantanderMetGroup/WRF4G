"""
Create WRF4G database tables on the configured database. 
    
Usage: 
    wrf4g syncdb [ --dbg ]
            
Options:
   --dbg     Debug mode.
"""
__version__  = '2.2.2'
__author__   = 'Carlos Blanco'
__revision__ = "$Id$"

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

