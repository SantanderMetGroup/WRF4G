#!/usr/bin/env python

import os, sys
import fortran_namelist as fn

def get_num_metgrid_levels():
  shcmd = "ncdump -h $(\ls -1 met_em*.nc | head -1) | grep 'num_metgrid_levels =' | sed -e 's/^\t//' | tr '=;' '  ' | awk '{print $2}'"
  return int(os.popen(shcmd).read().strip())

def get_num_metgrid_soil_levels():
  shcmd = "ncdump -h $(\ls -1 met_em*.nc | head -1) | grep 'NUM_METGRID_SOIL_LEVELS' | sed -e 's/^\t//' | tr '=:;' '   ' | awk '{print $2}'"
  return int(os.popen(shcmd).read().strip())

def get_time_step(coarse_dx, factor):
  HOUR_DIVISORS = [
    1,2,3,4,5,6,8,9,10,12,15,16,18,20,24,25,30,36,40,45,48,50,60,
    72,75,80,90,100,120,144,150,180,200,225,240,300,360,400,450,600,
    720,900,1200
  ]
  tstep = int(factor*coarse_dx/1000)
  ival = map(lambda x: x<=tstep, HOUR_DIVISORS).index(0) - 1
  return HOUR_DIVISORS[ival]

def get_latlon_dx(start_date):
  #  Try to get dx from the met_em or wrfinput files. Only
  #  required for lat-lon grids, otherwise it is available
  #  in the namelist.wps file
  dxfile = ""
  if os.path.exists("met_em.d01.%s.nc" % start_date):
    dxfile = "met_em.d01.%s.nc" % start_date
  elif os.path.exists("wrfinput_d01"):
    dxfile = "wrfinput_d01"
  if dxfile:
    shcmd = "ncdump -h %s | grep 'DX =' | sed -e 's/^\t//' | tr '=;' ' ' | awk '{printf \"%f\", $2}'" % dxfile
    rval = round(float(os.popen(shcmd).read().strip()), 2)
  else:
    raise Exception('get_latlon_dx: no met_em or wrfinput file found')
  return rval

class fake_strptime:
  "This works with any python, unlike the datetime.datetime.strptime method"
  def __init__(self, strdate, strfmt):
    """The strfmt argument is not used.
     Just there to share the prototipe of datetime.datetime.strptime"""
    spdate = strdate.replace("-", " ")
    spdate = spdate.replace("_", " ")
    spdate = spdate.replace(":", " ")
    spdate = spdate.split()
    self.year = spdate[0]
    self.month = spdate[1]
    self.day = spdate[2]
    self.hour = spdate[3]
    self.minute = spdate[4]
    self.second = spdate[5]

start_date = sys.argv[1]
end_date = sys.argv[2]
maxdom = int(sys.argv[3])
chunk_is_restart = sys.argv[4]
timestep_dxfactor = sys.argv[5]

namelistwps = "../../WPS/namelist.wps"
namelistinput = "namelist.input"
nmlw = fn.FortranNamelist(namelistwps)
nmli = fn.WrfNamelist(namelistinput)

#start_date = nmlw.getValue("start_date")[0]
#end_date = nmlw.getValue("end_date")[0]

sdate = fake_strptime(start_date, '%Y-%m-%d_%H:%M:%S')
edate = fake_strptime(end_date, '%Y-%m-%d_%H:%M:%S')

nmli.setValue("max_dom", maxdom)
for var in ["run_days", "run_hours", "run_minutes", "run_seconds"]:
  nmli.setValue(var, 0)
nmli.setMaxDomValue("start_year",  sdate.year)
nmli.setMaxDomValue("start_month", sdate.month)
nmli.setMaxDomValue("start_day",   sdate.day)
nmli.setMaxDomValue("start_hour",  sdate.hour)
nmli.setMaxDomValue("end_year",  edate.year)
nmli.setMaxDomValue("end_month", edate.month)
nmli.setMaxDomValue("end_day",   edate.day)
nmli.setMaxDomValue("end_hour",  edate.hour)
for var in [
  "parent_grid_ratio", "i_parent_start", "j_parent_start", "e_we", "e_sn"]:
  nmli.setValue(var, nmlw.getValue(var))
nmli.setValue("parent_time_step_ratio", nmlw.getValue("parent_grid_ratio"))
if os.path.exists("met_em.d01.%s.nc" % start_date):
  # If there are met_em files, we need to run real.exe. Otherwise, we
  # cannot get enough info (num_metgrid_*levels) to run real.exe
  nmli.setValue("num_metgrid_levels", get_num_metgrid_levels())
  nmli.setValue("num_metgrid_soil_levels", get_num_metgrid_soil_levels())
#
#  Compute the grid spacings. Read them from met_em files if the projection is lat-lon.
#
nmli.setValue("grid_id", range(1, maxdom+1))
pid = nmlw.getValue("parent_id")
pgr = nmlw.getValue("parent_grid_ratio")
proj = nmlw.getValue("map_proj")[0]
if proj == "lat-lon":
  thisdx = get_latlon_dx(start_date)
else:
  thisdx = nmlw.getValue("dx")[0]
alldx = [thisdx,]
for idom in range(1,maxdom):
  thisdx = alldx[pid[idom]-1]/pgr[idom]
  alldx.append(thisdx)
nmli.setValue("dx", alldx)
nmli.setValue("dy", alldx) # May be an issue for global WRF
#
# Compute the time step. 
#
if timestep_dxfactor.startswith("manual:"):
  nmli.setValue("time_step", int(timestep_dxfactor[7:]))
elif timestep_dxfactor.startswith("adaptive:"):
  nmli.setValue("use_adaptive_time_step", ".true.", "domains")
else:
  nmli.setValue("time_step",
    get_time_step(nmli.getValue("dx")[0], eval(timestep_dxfactor))
  )
nmli.setValue("restart", chunk_is_restart)
#
#  Currently, sibling domains only work with this on (?)
#
nmli.setValue("debug_level", 300)
#
#  Trim, check, overwrite the file and ... we are done!
#
nmli.trimMaxDom()
nmli.wrfCheck()
nmli.extendMaxDomVariables()
nmli.overWriteNamelist()
