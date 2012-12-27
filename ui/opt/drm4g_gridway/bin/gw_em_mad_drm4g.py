#!/usr/bin/env python

import sys
import os
import traceback
import logging.config
try:
    GW_LOCATION = os.environ['GW_LOCATION']
    WRF4G_LOCATION = os.environ['WRF4G_LOCATION']
except Exception, e:
    print 'Caught exception: %s: %s' % (e.__class__, str(e))
    sys.exit(-1)
if sys.version_info < (2,4) and sys.version_info > (3,0):
    print 'The version number of the Python has to be > = 2.4 and < 3.0'
    sys.exit(-1)
try:
    sys.path.insert(0, os.path.join(GW_LOCATION, 'libexec'))
    try:
        logging.config.fileConfig(os.path.join(WRF4G_LOCATION,'var', 'drm4g','drm4g_em.ini'))
    except :
        pass
except Exception, e:
    print 'Caught exception: %s: %s' % (e.__class__, str(e))
    traceback.print_exc(file=sys.stdout)
    sys.exit(-1)
from drm4g.core.em_mad import GwEmMad
from optparse import OptionParser
import exceptions

def main():
    parser = OptionParser(description = 'Execution manager MAD',
            prog = 'gw_em_mad_drm4g.py', version = '0.1',
            usage = 'Usage: %prog')
    options, args = parser.parse_args()
    try:
        GwEmMad().processLine()
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
