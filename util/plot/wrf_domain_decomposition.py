import os
import glob
import sys
import datetime, time

path=sys.argv[1]
epsout=sys.argv[2]

files = glob.glob('%s/WRFV3/run/rsl.out.????' % path)
logfile = '%s/log/time.log' % path

for line in open(logfile):
  spline = line.split()
  if spline[0] == "wrf":
    break

d1 = datetime.datetime(*(time.strptime(spline[1], '%Y%m%d%H%M%S')[0:6]))
d2 = datetime.datetime(*(time.strptime(spline[2], '%Y%m%d%H%M%S')[0:6]))
delta = d2-d1

if not files:
  print "No rsl files available"
  sys.exit()

lines = os.popen("head -20 %s" % files[0] )
for line in lines:
  if line.startswith('  ids,ide,jds,jde'):
    spline = line.split()
    tx0, tx1, ty0, ty1 = map(int, spline[1:])
xsize = tx1+1
ysize = ty1+1

command = """
gmtset PAGE_ORIENTATION portrait
gmtset PLOT_DEGREE_FORMAT dddF
gmtset PAPER_MEDIA a4+
"""
os.system(command)
rjflag = "-R0/%d/0/%d -JX20c/%gc" % (xsize, ysize, 20*ysize/float(xsize))
os.system('psbasemap %s -B20/20:.%s:WESN -K > %s' % (rjflag,delta.seconds,epsout))
color = 0
for file in files:
  lines = os.popen("head -20 %s" % file )
  for line in lines:
    if line.startswith('taskid:'):
      machine = line.split()[-1].strip()
    elif line.startswith('  ips,ipe,jps,jpe'):
      spline = line.split()
      x0, x1, y0, y1 = map(int, spline[1:])
  out = open("kkk.xy","w")
  for i in range(x0, x1+1):
    for j in range(y0, y1+1):
      print >> out, i,j
  out.close()
  os.system('psxy kkk.xy %s -Sc0.05 -G%d -O -K >> %s' % (rjflag,color,epsout))
  color += 150
  color = color % 255
os.system('psxy /dev/null %s -O >> %s' % (rjflag,epsout))

