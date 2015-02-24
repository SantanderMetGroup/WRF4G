#!/usr/bin/env python

__version__  = '2.3.1'
__author__   = 'Carlos Blanco'
__revision__ = "$Id: gw_im_mad_drm4g.py 2352 2015-02-24 10:23:57Z carlos $"

from drm4g_env import *
from drm4g.core.im_mad import GwImMad

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
        traceback.print_exc(file=sys.stdout)
        exit( 'Caught exception: %s: %s' % (e.__class__, str(e)) )


if __name__ == '__main__':
    main()

