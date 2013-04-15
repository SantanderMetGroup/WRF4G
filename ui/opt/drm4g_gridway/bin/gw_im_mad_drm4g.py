#!/usr/bin/env python

import sys
import os
import traceback
import logging.config
from os.path import dirname, join

if sys.version_info < (2,4) and sys.version_info > (3,0):
    print 'The version number of the Python has to be > = 2.4 and < 3.0'
    sys.exit(-1)
try:
    sys.path.insert(0, join(dirname(dirname(os.path.abspath(__file__))), 'libexec'))
    from drm4g.global_settings import PATH_LOGGER
    try:
        logging.config.fileConfig(PATH_LOGGER)
    except :
        pass
except Exception, e:
    print 'Caught exception: %s: %s' % (e.__class__, str(e))
    traceback.print_exc(file=sys.stdout)
    sys.exit(-1)
from drm4g.core.im_mad import GwImMad
from optparse import OptionParser
import exceptions

def main():
    parser = OptionParser(description = 'Information manager MAD',
            prog = 'gw_im_mad_drm4g.py', version = '0.1',
            usage = 'Usage: %prog ')
    options, args = parser.parse_args()
    try:
        GwImMad().processLine()
    except exceptions.KeyboardInterrupt, e:
        sys.exit(-1)
    except exceptions.SystemExit, e:
        print e
        sys.exit(0)
    except Exception, e:
        print 'Caught exception: %s: %s' % (e.__class__, str(e))
        traceback.print_exc(file=sys.stdout)


if __name__ == '__main__':
    main()

