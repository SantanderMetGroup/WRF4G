
__all__ = ["communicators", "core", "handlers", "utils"]

__version__ = '0.1'
__author__  = 'Carlos Blanco'
__revision__ = "$Id: __init__.py 1029 2011-07-07 18:12:45Z carlos $"

import os
import os.path
import sys
try:
   WRF4G_LOCATION = os.environ['WRF4G_LOCATION']
   sys.path.insert(0, os.path.join(WRF4G_LOCATION, 'lib','python'))
   import logging_wrf4g
   logging_wrf4g.Logger(os.path.join(WRF4G_LOCATION, 'var','drm4g','drm4g.ini'))
except :
   pass
