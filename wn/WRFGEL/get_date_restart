#!/usr/bin/python

from vcp import VCPURL       
from os import getenv
from commands import getstatusoutput
from optparse import OptionParser
from sys import exit   
from re import search




WRF4G_CONF_FILE_NOT_DEFINED=1
DATE_BAD_FORMED=2
TOO_MANY_DATES=3

usage="""%prog experiment realization [wrf4g.conf]

Example: %prog seasonal_nino_01 seas01 /tmp/wrf4g.conf

"""
# Check the Arguments
parser = OptionParser(usage,version="%prog 1.0")
parser.add_option("-v", "--verbose",action="store_true", dest="verbose", default=False,help="Verbose mode. Explain what is being done")
(options, args) = parser.parse_args()


if len(args) == 2 :
  experiment = args[0]
  realization = args[1]
  wrf4g_conf=getenv("WRF4G_CONF_FILE","Error")
  if wrf4g_conf == "Error" :
    print "Error: WRF4G_CONF_FILE not defined. Define the environment variable or give it as an argument to this function."
    exit(WRF4G_CONF_FILE_NOT_DEFINED)

elif len(args) == 3:
  experiment = args[0]
  realization = args[1]
  wrf4g_conf = args[2]
   
else :
  parser.error("Incorrect number of arguments")
  exit(1)

# Check the WRF4g_BASEPATH is established
exec(open(wrf4g_conf).read())

if WRF4G_BASEPATH in dir():
  print "Error: WRF4G_BASEPATH is not defined"
  exit(1)

# Load the URL into the VCPURL class
repo="%s/experiments/%s/%s/restart" % (WRF4G_BASEPATH,experiment,realization)
if verbose: print repo

list=VCPURL(repo)
restart_list=list.ls("*.nc")

if len(restart_list) == 0 :
  # There are not restart files
  print "There are not restart files"
  date=0

elif len(restart_list) == 1:
  g=search("(\d{8}T\d{6}Z)",restart_list[0])
  if g is None :
    print "Error: Date is not well formed"
    exit(DATE_BAD_FORMED)
  [date]=g.groups()  
else :
  print "Error: Problems getting date. Too many files"
  exit(TOO_MANY_DATES)     

print date

