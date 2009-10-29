#
#  The following additional variables can be asked for:
#
#    "U10ER", "V10ER", "MSLP", "WIND"
#
from pyclimate.JDTimeHandler import *
from pyclimate.ncstruct import *
from Scientific.IO.NetCDF import *
from Numeric import *
from glob import glob
from datetime import datetime
import sys, time

DEBUG = 1
files = glob(sys.argv[1])
files.sort()
OFILE = sys.argv[2]
vars  = sys.argv[3].split(',')
try:
  geofile = sys.argv[4]
except:
  geofile = ""

def rotate_lcc_wind(u,v,xlat,xlon,cen_lon,truelat1,truelat2):
  pii = 3.14159265
  d2r = pii/180.
  if abs(truelat1-truelat2) > 0.1:
    cone = (
      (log(cos(truelat1*d2r)) - log(cos(truelat2*d2r))) /
      (log(tan((90.-abs(truelat1))*d2r*0.5 )) - log(tan((90.-abs(truelat2))*d2r*0.5 )) )
    )
  else:
    cone = sin( abs(truelat1)*d2r )
  diff = xlon - cen_lon
  diff = where(diff > 180., diff-360., diff)
  diff = where(diff < -180., diff+360., diff)
  alpha = where(xlat < 0., -diff*cone*d2r, diff*cone*d2r)
  return v*sin(alpha)[NewAxis,:,:] + u*cos(alpha)[NewAxis,:,:],  v*cos(alpha)[NewAxis,:,:] - u*sin(alpha)[NewAxis,:,:]

def compute_mslp(p, pb, ph, phb, t , qvapor):
  """
  Pure python code by J. Fernandez to extrapolate surface pressure to sea level.
  Strategy borrowed from from_wrf_to_grads.f90 code.
  """
  # Some required physical constants:
  R=287.04
  g=9.81
  gamma=0.0065
  # Specific constants for assumptions made in this routine:
  TC=273.16+17.5
  pconst = 10000
  cp           = 7.*R/2.
  rcp          = R/cp
  p1000mb      = 100000.
  # Transpose and get full variables out of perturbations and potential T
  p = transpose(p + pb)
  ph = transpose((ph + phb) / 9.81)
  qvapor = transpose(qvapor)
  t = transpose(t)
  t = (t+300.)*(p/p1000mb)**rcp
  # populate the geopotential_height at mid_levels array with
  # averages between layers below and above
  nz = ph.shape[2]
  z = (ph[:,:,:nz-1] + ph[:,:,1:nz]) / 2.0
  # Find least zeta level that is pconst Pa above the surface.  We later use this
  # level to extrapolate a surface pressure and temperature, which is supposed
  # to reduce the effect of the diurnal heating cycle in the pressure field.
  dp = p-(p[:,:,0,:][:,:,NewAxis,:]-pconst)
  level = add.reduce(dp>0, axis=2)
  # Get temperature pconst Pa above surface.  Use this to extrapolate
  # the temperature at the surface and down to sea level.
  indic = ones(p.shape)*level[:,:,NewAxis,:]
  loidx = arange(nz-1)[NewAxis,NewAxis,:,NewAxis]==indic
  hiidx = arange(nz-1)[NewAxis,NewAxis,:,NewAxis]==(indic+1)

  plo = add.reduce(p*loidx, 2)
  phi = add.reduce(p*hiidx, 2)
  qlo = add.reduce(qvapor*loidx, 2)
  qhi = add.reduce(qvapor*hiidx, 2)
  tlo = add.reduce(t*loidx, 2)*(1. + 0.608 * qlo)
  thi = add.reduce(t*hiidx, 2)*(1. + 0.608 * qhi)
  zlo = add.reduce(z*loidx, 2)
  zhi = add.reduce(z*hiidx, 2)

  p_at_pconst = p[...,0,:] - pconst
  t_at_pconst = thi-(thi-tlo)*log(p_at_pconst/phi)*log(plo/phi)
  z_at_pconst = zhi-(zhi-zlo)*log(p_at_pconst/phi)*log(plo/phi)

  t_surf = t_at_pconst*(p[...,0,:]/p_at_pconst)**(gamma*R/g)
  t_sea_level = t_at_pconst+gamma*z_at_pconst
  # If we follow a traditional computation, there is a correction to the sea level
  # temperature if both the surface and sea level temnperatures are *too* hot.
  t_sea_level = where(t_sea_level>TC and t_surf <= TC, TC, TC - 0.005*(t_surf-TC)**2)
  z_half_lowest = z[:,:,0,:]
  sea_level_pressure = p[:,:,0,:] * exp((2.*g*z_half_lowest)/ (R*(t_sea_level+t_surf)))
  return transpose(sea_level_pressure)

def correct_negprecip(rainarr, lrec):
    # las integraciones de jp no son continuas. Esto evita los 
    # brincos en la precip.
    mask = greater_equal(rainarr, 0.0)
    if lrec == 0:
      mask2 = less_equal(rainarr, 0.1)
      mask = mask * mask2
    nbad = add.reduce(ravel(logical_not(mask)))
    if nbad: 
      print "Neg. precip at", nbad, "sites, localrec:" , lrec
      return mask * rainarr
    else:
      return rainarr

def charr2str(carr):
  # Forma totalmente cerda...
  return "".join(carr.tolist())

def str2offset(str, basedate):
  diff = datetime.strptime(str, '%Y-%m-%d_%H:%M:%S') - basedate
  return diff.days*24 + diff.seconds/3600.

def discard_suspect_files(filelist, criteria='uncommon_size'):
  total_items = len(filelist)
  file_sizes = map(os.path.getsize, filelist)
  sizes = {}
  for size in file_sizes:
    try:
      sizes[size]+= 1
    except:
      sizes[size] = 1
  lsizes = [(b/float(total_items),a) for a,b in sizes.items()]
  lsizes.sort()
  discard_sizes = []
  for item in lsizes:
    if item[0] < 0.2:
      discard_sizes.append(item[1])
  i_file=0
  rval = []
  for file in filelist:
    print os.path.basename(file), file_sizes[i_file],
    if file_sizes[i_file] in discard_sizes:
      print " X"
    else:
      rval.append(filelist[i_file])
      print
    i_file+=1
  return rval
#
#  Clone the structure of the netcdf file and get the initial time from the first file.
#
print files[0]
inc = NetCDFFile(files[0],'r')
dims=("south_north","west_east","bottom_top") #,"soil_layers_stag")
onc = nccopystruct(OFILE, inc, dims)
onc.history = "Created by %s on %s" % (sys.argv[0],time.ctime(time.time()))
onc.sync()
oncx = onc.createVariable("west_east",Numeric.Float64, ("west_east",))
oncx.long_name = "x coordinate of projection"
oncx.standard_name = "projection_x_coordinate"
oncx[:len(oncx)] = (arange(1,len(oncx)+1)-len(oncx)/2)*inc.DX
oncy = onc.createVariable("south_north",Numeric.Float64, ("south_north",))
oncy.long_name = "y coordinate of projection"
oncy.standard_name = "projection_y_coordinate"
oncy[:len(oncy)] = (arange(1,len(oncy)+1)-len(oncy)/2)*inc.DX
#
#
#
oncproj = onc.createVariable("Lambert_Conformal",Numeric.Int, ())
oncproj.grid_mapping_name = "lambert_conformal_conic"
oncproj.cone_type = "secant"
oncproj.northern_parallel = "%4.1fN" % inc.TRUELAT2
oncproj.southern_parallel = "%4.1fN" % inc.TRUELAT1
oncproj.longitude_of_central_meridian = inc.CEN_LON
oncproj.latitude_of_projection_origin = inc.CEN_LAT
#
#  Lat-lons (from geo_em file if provided)
#
if geofile:
  incgeo = NetCDFFile(geofile,'r')
  lats = incgeo.variables["XLAT_M"][0]
  lons = incgeo.variables["XLONG_M"][0]
  incgeo.close()
else:
  lats = inc.variables["XLAT"][0]
  lons = inc.variables["XLONG"][0]
onclat = onc.createVariable("lat",Numeric.Float32, ("south_north","west_east"))
onclat.long_name = "Latitudes"
onclat.standard_name = "Latitude"
onclat.units = "degrees_north"
onclat[:,:] = lats
onclon = onc.createVariable("lon",Numeric.Float32, ("south_north","west_east"))
onclon.long_name = "Longitude"
onclon.standard_name = "Longitude"
onclon.units = "degrees_east"
onclon[:,:] = lons
#
#   Get the initial date and create a new time variable
#
incTimes = inc.variables["Times"]
initialdate = datetime.strptime(charr2str(incTimes[0]), '%Y-%m-%d_%H:%M:%S')
onc.createDimension("time", None)
onctime = onc.createVariable("time",Numeric.Float64, ("time",))
onctime.long_name = "Time variable"
onctime.units = "hours since %s" % initialdate.strftime('%Y-%m-%d %H:%M:%S')
inc.close()
#
#  Loop over files extracting variables and times
#
files = discard_suspect_files(files, criteria='uncommon_size')
itime = 0
for f in files:
  if DEBUG: print "Processing file %s" % f
  inc = NetCDFFile(f,'r')
  incTimes = inc.variables["Times"]
  nrecords = len(incTimes)
  times = map(charr2str,incTimes[:nrecords])
  times = map(lambda x: str2offset(x,initialdate), times)
  onctime[itime:itime+nrecords] = times
  for varname in vars:
    if DEBUG: print "Processing var %s" % varname 
    if varname in ["U10ER", "V10ER"]:
      u = inc.variables["U10"]
      v = inc.variables["V10"]
      uer, ver = rotate_lcc_wind(u,v, onclat[:,:], onclon[:,:], inc.CEN_LON, inc.TRUELAT1, inc.TRUELAT2)
      exec("incvar=%s" % varname[0].lower()) # muy cerdo
      exec("copyval=%ser" % varname[0].lower()) # muy cerdo
      if not itime:
        oncvar = onc.createVariable(varname, Numeric.Float32, ("time",)+incvar.dimensions[1:])
        for att in incvar.__dict__.keys():
          setattr(oncvar, att, getattr(incvar, att))
        oncvar.description = oncvar.description + " (Earth relative)"
        oncvar.coordinates="lat lon"
        oncvar.grid_mapping = "Lambert_Conformal"
      else:
        oncvar = onc.variables[varname]
      oncvar[itime:itime+nrecords] = copyval[:nrecords].astype(oncvar.typecode())
    elif varname == "WIND":
      incvar = inc.variables["U10"]
      u = incvar[:]
      v = inc.variables["V10"][:]
      copyval = Numeric.sqrt(u*u+v*v)
      if not itime:
        oncvar = onc.createVariable(varname, Numeric.Float32, ("time",)+incvar.dimensions[1:])
        for att in incvar.__dict__.keys():
          setattr(oncvar, att, getattr(incvar, att))
        oncvar.description = "Wind speed at 10m"
        oncvar.coordinates="lat lon"
        oncvar.grid_mapping = "Lambert_Conformal"
      else:
        oncvar = onc.variables[varname]
      oncvar[itime:itime+nrecords] = copyval[:nrecords].astype(oncvar.typecode())
    elif varname == "MSLP":
      incvar = inc.variables['P']
      p = incvar[:]
      pb = inc.variables['PB'][:]
      ph = inc.variables['PH'][:]
      phb = inc.variables['PHB'][:]
      t = inc.variables['T'][:]
      qvapor = inc.variables['QVAPOR'][:]
      mslp = compute_mslp(p, pb, ph, phb, t , qvapor)
      if not itime:
        oncvar = onc.createVariable(varname, Numeric.Float32, ("time",)+incvar.dimensions[2:])
        oncvar.units = "Pa"
        oncvar.description = "Mean sea level pressure"
        oncvar.coordinates="lat lon"
        oncvar.grid_mapping = "Lambert_Conformal"
      else:
        oncvar = onc.variables[varname]
      oncvar[itime:itime+nrecords] = mslp[:nrecords].astype(oncvar.typecode())
    else:
      incvar = inc.variables[varname]
      if not itime:
        oncvar = onc.createVariable(varname, Numeric.Float32, ("time",)+incvar.dimensions[1:])
        for att in incvar.__dict__.keys():
          setattr(oncvar, att, getattr(incvar, att))
        oncvar.coordinates="lat lon"
        oncvar.grid_mapping = "Lambert_Conformal"
      else:
        oncvar = onc.variables[varname]
      oncvar[itime:itime+nrecords] = incvar[:nrecords]
  itime += nrecords
  inc.close()
  onc.sync()

onc.sync()
onc.close()
