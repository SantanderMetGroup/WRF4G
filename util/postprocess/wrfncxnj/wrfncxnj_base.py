from pyclimate.JDTimeHandler import *
from pyclimate.ncstruct import *
from Scientific.IO.NetCDF import *
#from netCDF3 import Dataset as NetCDFFile
from Numeric import *
from glob import glob
from datetime import datetime
import sys, time, string, csv
from wrfncxnj_cli import opt, args

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

def charr2str(carr):
  # Forma totalmente cerda...
  return "".join(carr.tolist())

def str2offset(str, basedate):
  if str == "0000-00-00_00:00:00":
    rval = 0
  else:
    diff = datetime.strptime(str, '%Y-%m-%d_%H:%M:%S') - basedate
    rval = diff.days*24 + diff.seconds/3600.
  return rval

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

def create_bare_curvilinear_CF_from_wrfnc(wrfncfile, idate, createz=None, createp=None, createsoil=None):
  inc = NetCDFFile(wrfncfile,'r')
  onc = NetCDFFile(opt.OFILE, "w")
  onc.history = "Created by %s on %s" % (sys.argv[0],time.ctime(time.time()))
  onc.sync()
  onc.createDimension("x", inc.dimensions["west_east"])
  oncx = onc.createVariable("x",Numeric.Float64, ("x",))
  oncx.axis = "X"
  oncx.long_name = "x coordinate of projection"
  oncx.standard_name = "projection_x_coordinate"
  oncx[:len(oncx)] = (arange(1,len(oncx)+1)-len(oncx)/2)*inc.DX
  onc.createDimension("y", inc.dimensions["south_north"])
  oncy = onc.createVariable("y",Numeric.Float64, ("y",))
  oncy.axis = "Y"
  oncy.long_name = "y coordinate of projection"
  oncy.standard_name = "projection_y_coordinate"
  oncy[:len(oncy)] = (arange(1,len(oncy)+1)-len(oncy)/2)*inc.DY
  onc.sync()
  if createz:
    onc.createDimension("z", inc.dimensions["bottom_top"])
    oncz = onc.createVariable("z",Numeric.Float64, ("z",))
    oncz.axis = "Z"
    oncz.long_name = "sigma at layer midpoints"
    oncz.positive = "down"
    oncz.standard_name = "atmosphere_sigma_coordinate"
    oncz.formula_terms = "sigma: z ps: ps ptop: PTOP"
    if inc.variables.has_key("ZNU"):
      oncz.assignValue(inc.variables["ZNU"][0])
    if inc.variables.has_key("P_TOP"):
      oncptop = onc.createVariable("PTOP",Numeric.Float32, ())
      oncptop.long_name = "Pressure at the top of the atmosphere"
      oncptop.units = "Pa"
      oncptop.assignValue(inc.variables["P_TOP"][0])
    onc.sync()
  if createp:
    onc.createDimension("plev", inc.dimensions["num_metgrid_levels"])
    oncz = onc.createVariable("plev",Numeric.Float64, ("plev",))
    oncz.axis = "Z"
    oncz.units = "Pa"
    oncz.long_name = "Pressure levels"
    oncz.positive = "down"
    oncz.standard_name = "air_pressure"
    if inc.variables.has_key("PLEV"):
      oncz.assignValue(inc.variables["PLEV"])
    onc.sync()
  if createsoil:
    if opt.fullfile:
      thisinc = NetCDFFile(opt.fullfile,'r')
    else:
      thisinc = inc
    onc.createDimension("slev", thisinc.dimensions["soil_layers_stag"])
    oncz = onc.createVariable("slev",Numeric.Float64, ("slev",))
    oncz.axis = "Z"
    oncz.long_name = "Soil level"
    oncz.units = "m"
    oncz.positive = "down"
    oncz.standard_name = "depth_below_surface"
    if thisinc.variables.has_key("ZS"):
      oncz.assignValue(thisinc.variables["ZS"][0])
    onc.sync()
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
  if opt.geofile:
    incgeo = NetCDFFile(opt.geofile,'r')
    lats = incgeo.variables["XLAT_M"][0]
    lons = incgeo.variables["XLONG_M"][0]
    incgeo.close()
  else:
    lats = inc.variables["XLAT"][0]
    lons = inc.variables["XLONG"][0]
  onclat = onc.createVariable("lat",Numeric.Float32, ("y","x"))
  onclat.long_name = "Latitudes"
  onclat.standard_name = "latitude"
  onclat.units = "degrees_north"
  onclat[:len(lats)] = lats
  onclon = onc.createVariable("lon",Numeric.Float32, ("y","x"))
  onclon.long_name = "Longitude"
  onclon.standard_name = "longitude"
  onclon.units = "degrees_east"
  onclon[:len(lons)] = lons
  #
  #   Get the initial date and create a new time variable
  #
  onc.createDimension("time", None)
  onctime = onc.createVariable("time",Numeric.Float64, ("time",))
  onctime.long_name = "time"
  onctime.units = "hours since %s" % idate.strftime('%Y-%m-%d %H:%M:%S')
  if opt.tbounds:
    onctime.bounds = "time_bnds"
    onc.createDimension("nv",2)
    onc.createVariable("time_bnds",Numeric.Float64, ("time","nv"))
  inc.close()
  onc.sync()
  return onc

def add_height_coordinate(onc, coorname, val):
  if not onc.variables.has_key(coorname):
    onc.createDimension(coorname,1)
    hvar = onc.createVariable(coorname, 'f', (coorname,))
    hvar.long_name = "height above the ground"
    hvar.standard_name = "height"
    hvar.units = "m"
    hvar[0] = array(val, 'f')

def add_depth_coordinate(onc, coorname, val):
  # TODO: Hay que aniadir las boundaries...
  if not onc.variables.has_key(coorname):
    onc.createDimension(coorname,1)
    hvar = onc.createVariable(coorname, 'f', (coorname,))
    hvar.long_name = "depth below the surface"
    hvar.standard_name = "depth"
    hvar.units = "m"
    hvar[0] = array(val, 'f')

dimension_mapping = {
  "south_north": "y",
  "west_east": "x",
  "bottom_top": "z",
  "soil_layers_stag": "slev",
  "num_metgrid_levels": "plev",
  "bottom_top_stag": "z", # the variables should be de-staggered before copying them to the output file
}

class Variable:
  def __repr__(self):
    return """\
abbr: %(standard_abbr)s
standard_name: %(standard_name)s
units: %(units)s
offset: %(offset)f
scale: %(scale)f
""" % self.__dict__

def stdvars(vars, vtable):
  rval = {}
  for line in csv.reader(open(vtable, "r"), delimiter=" ", skipinitialspace=True):
    if line[0][0] == "#":
      continue
    varwrf = line[0]
    if varwrf in vars:
      v = Variable()
      v.varname = varwrf
      v.standard_abbr = line[1]
      v.long_name = line[2]
      v.standard_name = line[3]
      v.units = line[4]
      v.scale = float(line[5])
      v.offset = float(line[6])
      rval[varwrf] = v
  return rval

def get_oncvar(varobj, incvar, onc, out_is_2D_but_in_3D=False, screenvar_at_2m=False, screenvar_at_10m=False):
  if not varobj.standard_abbr in onc.variables:
    # First time record, create the variable
    if out_is_2D_but_in_3D:
      cut_from = 2
    else:
      cut_from = 1
    dims = ("time",)+tuple(map(lambda x: dimension_mapping[x], incvar.dimensions[cut_from:]))
    if screenvar_at_2m:
      add_height_coordinate(onc, "height", 2)
      dims = dims[:1]+("height",)+dims[1:]
    if screenvar_at_10m:
      add_height_coordinate(onc, "heightv", 10)
      dims = dims[:1]+("heightv",)+dims[1:]
    #pdb.set_trace()
    oncvar = onc.createVariable(varobj.standard_abbr, Numeric.Float32, dims)
    oncvar.long_name = varobj.long_name
    oncvar.standard_name = varobj.standard_name
    oncvar.units = varobj.units
    oncvar.coordinates="lat lon"
    oncvar.grid_mapping = "Lambert_Conformal"
  else:
    # Otherwise, just retrieve the variable handler
    oncvar = onc.variables[varobj.standard_abbr]
  return oncvar

