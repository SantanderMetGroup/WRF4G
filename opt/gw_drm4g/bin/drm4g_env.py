#!/usr/bin/env python

import sys
import os
import traceback
import logging.config
from os.path import dirname, join

if sys.version_info < (2,5) and sys.version_info > (3,0):
    print 'The version number of the Python has to be > = 2.5 and < 3.0'
    sys.exit(-1)
try:
    sys.path.insert(0, join(dirname(dirname(os.path.abspath(__file__))), 'libexec'))
    from drm4g import FILE_LOGGER, DRM4G_DIR  
    try:
        logging.config.fileConfig(FILE_LOGGER, {"WRF4G_LOCATION": DRM4G_DIR})
    except :
        pass
except Exception, e:
    print 'Caught exception: %s' % str(e)
    traceback.print_exc(file=sys.stdout)
    sys.exit(-1)
from optparse import OptionParser
import exceptions

