#!/usr/bin/env python

__version__  = '2.2.0'
__author__   = 'Carlos Blanco'
__revision__ = "$Id: drm4g_env.py 2250 2014-08-27 09:04:57Z carlos $"

import sys
import os
import traceback
import logging.config
from os.path import dirname, join

if sys.version_info < (2,5) and sys.version_info > (3,0):
    exit( 'The version number of the Python has to be > = 2.5 and < 3.0' )
try:
    sys.path.insert(0, join(dirname(dirname(os.path.abspath(__file__))), 'libexec'))
    from drm4g import DRM4G_LOGGER, DRM4G_DIR  
    try:
        logging.config.fileConfig(DRM4G_LOGGER, {"DRM4G_DIR": DRM4G_DIR})
    except :
        pass
except Exception, e:
    traceback.print_exc(file=sys.stdout)
    exit( 'Caught exception: %s: %s' % (e.__class__, str(e)) )
from optparse import OptionParser
import exceptions

