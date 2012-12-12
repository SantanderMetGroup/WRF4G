#!/usr/bin/env python

import sys
import os
import traceback
try:
    GW_LOCATION = os.environ['GW_LOCATION']
except Exception:
    print 'Please, set GW_LOCATION variable'
    sys.exit(-1)
if sys.version_info < (2,4) and sys.version_info > (3,0):
    print 'The version number of the Python must be > = 2.4 and < 3.0'
    sys.exit(-1)
try:
    sys.path.insert(0, os.path.join(GW_LOCATION, 'libexec'))
except Exception, e:
    print 'Caught exception: %s: %s' % (e.__class__, str(e))
    traceback.print_exc(file=sys.stdout)
    sys.exit(-1)
from drm4g.core.em_mad import GwEmMad
from drm4g.utils.logger import log_to_file
from optparse import OptionParser
import exceptions

def main():
    parser = OptionParser(description = 'Execution manager MAD',
            prog = 'gw_em_mad_drm4g.py', version = '0.1',
            usage = 'Usage: %prog')
    options, args = parser.parse_args()
    try:
        log_to_file(os.path.join(GW_LOCATION, 'var', '.drm4g_em.log'))
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
