import sys
import datetime, time
import glob

path=sys.argv[1]
try:
  ncores=int(sys.argv[2])
except:
  ncores=0

try:
  entrytime = sys.argv[3]
except:
  entrytime = ""

simlen_s = 24*3600.

files = glob.glob('%s/*/*/log/time.log' % path)
for logfile in files:
  if entrytime:
    for line in open(logfile):
      spline = line.split()
      if spline[0] == entrytime:
        break
    date1 = spline[-2]
    date2 = spline[-1]
  else:
    lines = open(logfile).readlines()
    date1 = lines[0].split()[-2]
    date2 = lines[-1].split()[-1]
  d1 = datetime.datetime(*(time.strptime(date1, '%Y%m%d%H%M%S')[0:6]))
  d2 = datetime.datetime(*(time.strptime(date2, '%Y%m%d%H%M%S')[0:6]))
  delta = d2-d1
  speed = simlen_s/delta.seconds
  print logfile,
  print speed, " (%d)" % (int(ncores*365.*24/speed),)

