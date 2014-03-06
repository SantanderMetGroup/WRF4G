import os
import sys
from os.path import dirname, expandvars, exists, abspath, join

WRF4G_LOCATION=os.getenv('WRF4G_LOCATION')
if not WRF4G_LOCATION :
    if exists( expandvars("$HOME/.wrf4g/etc/framework.conf") ) :
        WRF4G_LOCATION = expandvars("$HOME/.wrf4g")
    else:
        WRF4G_LOCATION = dirname( dirname( abspath( __file__ ) ) )
    os.environ['WRF4G_LOCATION'] = WRF4G_LOCATION
os.environ['GW_LOCATION'] = join( WRF4G_LOCATION , 'opt' , 'gw_drm4g' )


DB4G_CONF = os.environ.get('DB4G_CONF')
if not DB4G_CONF :
    DB4G_CONF = '%s/etc/db4g.conf' % WRF4G_LOCATION
    os.environ['DB4G_CONF']  = DB4G_CONF
if not exists( DB4G_CONF ):
    print 'DB4G_CONF does not exist'
    sys.exit(2)

sys.path.insert(0, join(dirname(dirname(abspath(__file__))), 'lib', 'python'))

from optparse import OptionParser
import exceptions
import re
import traceback
import logging.config

try:
    logging.config.fileConfig(join(WRF4G_LOCATION,'etc','logger.conf'),{'WRF4G_LOCATION': WRF4G_LOCATION })
except:
    pass

#If we are running in a real terminal then print colors
if sys.stdout.isatty():
    bold  = "\033[1m"
    reset = "\033[0;0m"
else:
    bold  = ""
    reset = ""

