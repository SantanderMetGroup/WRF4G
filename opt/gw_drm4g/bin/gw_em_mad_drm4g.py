#!/usr/bin/env python

__version__  = '2.2.0'
__author__   = 'Carlos Blanco'
__revision__ = "$Id: gw_em_mad_drm4g.py 2250 2014-08-27 09:04:57Z carlos $"

from drm4g_env import *
from drm4g.core.em_mad import GwEmMad

def main():
    parser = OptionParser(description = 'Execution manager MAD',
            prog = 'gw_em_mad_drm4g.py', version = '0.1',
            usage = 'Usage: %prog')
    options, args = parser.parse_args()
    try:
        GwEmMad().processLine()
    except exceptions.KeyboardInterrupt, e:
        sys.exit(-1)
    except Exception, e:
        traceback.print_exc(file=sys.stdout)
        exit( 'Caught exception: %s: %s' % (e.__class__, str(e)) )


if __name__ == '__main__':
    main()
