import os,sys

WRF4G_LOCATION=os.environ.get('WRF4G_LOCATION')
if WRF4G_LOCATION == None:
    WRF4G_LOCATION=os.path.dirname(os.path.dirname(__file__))
    os.environ['WRF4G_LOCATION']=WRF4G_LOCATION

os.environ['DB4G_CONF']='%s/etc/db4g.conf'%WRF4G_LOCATION
DB4G_CONF=os.environ['DB4G_CONF']
if not os.path.isfile(DB4G_CONF):
    print 'DB4G_CONF do not exist'
    exit(2)

sys.path.insert(0, os.path.join(WRF4G_LOCATION, 'lib', 'python'))

from sys import stderr,exit, path
from optparse import OptionParser
import WRF4G,exceptions,re,traceback,logging.config
import vcplib,vdblib

logging.config.fileConfig(os.path.join(WRF4G_LOCATION,'etc','logger.conf'))

#If we are running in a real terminal then print colors
if sys.stdout.isatty():
    bold  = "\033[1m"
    reset = "\033[0;0m"
else:
    bold  = ""
    reset = ""

