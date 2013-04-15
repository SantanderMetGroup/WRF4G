import os
import sys

WRF4G_LOCATION=os.environ.get('WRF4G_LOCATION')
if WRF4G_LOCATION == None:
    WRF4G_LOCATION = os.path.dirname(os.path.abspath(__file__))
    os.environ['WRF4G_LOCATION'] = WRF4G_LOCATION

DB4G_CONF=os.environ.get('DB4G_CONF')
if DB4G_CONF == None:
    DB4G_CONF = '%s/etc/db4g.conf' % WRF4G_LOCATION
    os.environ['DB4G_CONF']  = DB4G_CONF
if not os.path.isfile(DB4G_CONF):
    print 'DB4G_CONF does not exist'
    sys.exit(2)

sys.path.insert(0, os.path.join(WRF4G_LOCATION, 'lib', 'python'))

from optparse import OptionParser
import exceptions
import re
import traceback
import logging.config

try:
    logging.config.fileConfig(os.path.join(WRF4G_LOCATION,'etc','logger.conf'),
                          {'WRF4G_LOCATION': WRF4G_LOCATION })
except:
    pass

#If we are running in a real terminal then print colors
if sys.stdout.isatty():
    bold  = "\033[1m"
    reset = "\033[0;0m"
else:
    bold  = ""
    reset = ""

