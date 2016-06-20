import os
import sys
from os.path      import dirname, join, abspath, exists

__version__  = '2.2.2'
__author__   = 'Carlos Blanco'
__revision__ = "$Id$"

HOME                 = os.environ.get( 'HOME' )
WRF4G_DIR            = os.environ[ 'GW_LOCATION' ] = join( os.environ.get( 'WRF4G_DIR', HOME ), '.wrf4g' )
WRF4G_DEPLOYMENT_DIR = dirname( dirname( dirname( dirname( abspath( __file__ ) ) ) ) )
DB4G_CONF            = os.environ.get( 'DB4G_CONF',  join( WRF4G_DIR , 'etc' , 'db.conf' ) )
WRF4G_LOGGER         = join( WRF4G_DIR, 'etc', 'logger.conf')

import wrf4g.orm
