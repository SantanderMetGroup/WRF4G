import os
import sys
from os.path import dirname

WRF4G_LOCATION = dirname(dirname(os.path.abspath(__file__)))
os.environ['WRF4G_LOCATION'] = WRF4G_LOCATION
sys.path.insert( 0 , os.path.join( WRF4G_LOCATION , 'lib', 'python' ) )

from optparse import OptionParser
import exceptions
import re
import traceback
import logging.config

try:
    logging.config.fileConfig(os.path.join(WRF4G_LOCATION,'etc','logger.conf'),
                          {'WRF4G_LOCATION': WRF4G_LOCATION })
except:
    pass

#If we are running in a real terminal then print colors
if sys.stdout.isatty():
    bold  = "\033[1m"
    reset = "\033[0;0m"
else:
    bold  = ""
    reset = ""

