#!/usr/bin/env python

__version__  = '2.3.1'
__author__   = 'Carlos Blanco'
__revision__ = "$Id: drm4g_env.py 2352 2015-02-24 10:23:57Z carlos $"

import sys
import os
import traceback
import logging.config
from os.path import dirname, join

if sys.version_info < (2,5) and sys.version_info > (3,0):
    exit( 'The version number of the Python has to be > = 2.5 and < 3.0' )
try:
    sys.path.insert(0, join(dirname(dirname(os.path.abspath(__file__))), 'libexec'))
    from wrf4g import WRF4G_DIR, WRF4G_LOGGER 
    try:
        logging.config.fileConfig(WRF4G_LOGGER, {"WRF4G_DIR": WRF4G_DIR})
    except :
        pass
except Exception, e:
    traceback.print_exc(file=sys.stdout)
    exit( 'Caught exception: %s: %s' % (e.__class__, str(e)) )
from optparse import OptionParser
import exceptions

