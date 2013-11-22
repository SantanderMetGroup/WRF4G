__all__ = []

__version__  = '2.0'
__author__   = 'Carlos Blanco'
__revision__ = "$Id:$"

import sys

if sys.version_info < (2,5) and sys.version_info > (3,0):
    print 'The version number of the Python has to be > = 2.5 and < 3.0'
    sys.exit(-1)
