from pyclimate.JDTimeHandler import *
from pyclimate.ncstruct import *
from Scientific.IO.NetCDF import *
#from netCDF3 import Dataset as NetCDFFile
from Numeric import *
import Numeric as np
from glob import glob
from datetime import datetime
import sys, time, string, csv
from wrfncxnj_cli import opt, args

class Constants:
  Rd = 287.04
  Rv = 461.5
  RdRv = Rd / Rv
  cp = 7.*Rd/2.
  epsilon_gamma = 0.62197
  es_base_bolton = 0.6112
  es_Abolton = 17.67
  es_Bbolton = 243.5
  es_base_tetens = 6.1078
  es_Atetens_vapor = 7.5
  es_Btetens_vapor = 237.3
  es_Atetens_ice = 9.5
  es_Btetens_ice = 265.5
  g = 9.81
  p1000mb = 100000.
  rcp = Rd/cp
  tkelvin = 273.15

def screenvar_at_2m(varobj, onc, wnfiles, wntimes):
  #
  # Works for any variable defined at 2m with no other transformation.
  #
  incvar = wnfiles.current.variables[varobj.varname]
  copyval = np.reshape(incvar[:],incvar.shape[:1]+(1,)+incvar.shape[1:])
  oncvar = get_oncvar(varobj, incvar, onc, screenvar_at_2m=True)
  return oncvar, copyval

def screenvar_at_10m(varobj, onc, wnfiles, wntimes):
  #
  # Works for any variable defined at 2m with no other transformation.
  #
  incvar = wnfiles.current.variables[varobj.varname]
  copyval = np.reshape(incvar[:],incvar.shape[:1]+(1,)+incvar.shape[1:])
  oncvar = get_oncvar(varobj, incvar, onc, screenvar_at_10m=True)
  return oncvar, copyval

def deaccumulate_flux(varobj, onc, wnfiles, wntimes):
  #
  # De-accumulates any variable if no other transformation is required
  #
  incvar = wnfiles.current.variables[varobj.varname]
  if not wnfiles.prv:
    lastval = incvar[0]
  else:
    lastval = wnfiles.prv.variables[varobj.varname][-1]
  lastval.shape = (1,) + lastval.shape
  copyval = incvar[:] - np.concatenate([lastval,incvar[:-1]])
  copyval = np.where(copyval<0., 0, copyval)/float(wntimes.outstep_s)
  oncvar = get_oncvar(varobj, incvar, onc)
  return oncvar, copyval

def deaccumulate(varobj, onc, wnfiles, wntimes):
  #
  # De-accumulates any variable if no other transformation is required
  #
  incvar = wnfiles.current.variables[varobj.varname]
  if not wnfiles.prv:
    lastval = incvar[0]
  else:
    lastval = wnfiles.prv.variables[varobj.varname][-1]
  lastval.shape = (1,) + lastval.shape
  copyval = incvar[:] - np.concatenate([lastval,incvar[:-1]])
  copyval = np.where(copyval<0., 0, copyval)
  oncvar = get_oncvar(varobj, incvar, onc)
  return oncvar, copyval

def fake_extreme(varobj, onc, wnfiles, wntimes):
  #
  # Extracts a variable, changes it's name and adds an attribute so extremes must be computed LATER
  # the CDO. It's called "fake extreme" because it is made to replace a extreme that's not computed by CLWRF.
  #
  incvar = wnfiles.current.variables[varobj.varname[:-4]] #[:-4]removes "MAX" or "MIN"
  copyval = incvar
  oncvar = get_oncvar(varobj, incvar, onc)
  oncvar.warning = "This is not a real extreme, extremes still need to be computed." 
  return oncvar, copyval

def rotate_uas(varobj, onc, wnfiles, wntimes):
  uvarname = varobj.varname[:-2] # remove the "ER"
  vvarname = "V" + varobj.varname[1:-2] # remove the "U" and the "ER"
  u = wnfiles.current.variables[uvarname]
  v = wnfiles.current.variables[vvarname]
  if not wnfiles.geo:
    print "I need the geo_em file to rotate winds!"
  else:
    sina = wnfiles.geo.variables["SINALPHA"][:]
    cosa = wnfiles.geo.variables["COSALPHA"][:]
  copyval = (
    u[:]*cosa[np.NewAxis,...] - v[:]*sina[np.NewAxis,...]
  )
  copyval.shape = u.shape[:1]+ (1,) + u.shape[1:]
  oncvar = get_oncvar(varobj, u, onc, screenvar_at_10m=True)
  return oncvar, copyval

def rotate_vas(varobj, onc, wnfiles, wntimes):
  uvarname = "U" + varobj.varname[1:-2] # remove the V and the "ER"
  vvarname = varobj.varname[:-2] # remove the "ER"
  u = wnfiles.current.variables[uvarname]
  v = wnfiles.current.variables[vvarname]
  if not wnfiles.geo:
    print "I need the geo_em file to rotate winds!"
  else:
    sina = wnfiles.geo.variables["SINALPHA"][:]
    cosa = wnfiles.geo.variables["COSALPHA"][:]
  copyval = (
    u[:]*sina[np.NewAxis,...] + v[:]*cosa[np.NewAxis,...]
  )
  copyval.shape = v.shape[:1]+ (1,) + v.shape[1:]
  oncvar = get_oncvar(varobj, v, onc, screenvar_at_10m=True)
  return oncvar, copyval

def compute_mslp(p, pb, ph, phb, t , qvapor):
  """
  Pure python code by J. Fernandez to extrapolate surface pressure to sea level.
  Strategy borrowed from from_wrf_to_grads.f90 code.
  """
  # Some required physical constants:
  Rd=287.04
  g=9.81
  gamma=0.0065
  # Specific constants for assumptions made in this routine:
  TC=273.16+17.5
  pconst = 10000
  cp           = 7.*Rd/2.
  rcp          = Rd/cp
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

  t_surf = t_at_pconst*(p[...,0,:]/p_at_pconst)**(gamma*Rd/g)
  t_sea_level = t_at_pconst+gamma*z_at_pconst
  # If we follow a traditional computation, there is a correction to the sea level
  # temperature if both the surface and sea level temnperatures are *too* hot.
  t_sea_level = where(t_sea_level>TC and t_surf <= TC, TC, TC - 0.005*(t_surf-TC)**2)
  z_half_lowest = z[:,:,0,:]
  sea_level_pressure = p[:,:,0,:] * exp((2.*g*z_half_lowest)/ (Rd*(t_sea_level+t_surf)))
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
  print lats.shape, onclat.shape
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
  onctime.standard_name = "time"
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
    hvar._CoordinateAxisType = "Height"
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

class WrfNcFiles:
  def __init__(self):
    self.current = None
    self.nxt = None
    self.prv = None
    self.geo = None
    self.full = None
  def loadFiles(self, filelist, previous_file=None, next_file=None):
    self.filelist = filelist
    self.nfiles = len(filelist)
    self.next_file = next_file
    self.ifile = -1
    if previous_file:
      self.current = NetCDFFile(previous_file, "r")
    else:
      self.current = None
    self.nxt = NetCDFFile(self.filelist[0])
  def assignNext(self):
    if self.ifile == self.nfiles-1:
      if self.next_file: self.nxt = NetCDFFile(self.next_file, "r")
      else: self.nxt = None
    else:
      self.nxt = NetCDFFile(self.filelist[self.ifile+1], "r")
  def cycle(self):
    self.ifile += 1
    self.prv = self.current
    self.current = self.nxt
    self.assignNext()
  def __iter__(self):  # Make this object an iterator
    return self 
  def next(self):
    if self.ifile >= self.nfiles-1:  
      raise StopIteration
    else:
      self.cycle()
      return self
  def rewind(self):
    self.loadFiles(self.filelist)
  def close(self):
    if self.current: self.current.close()
    if self.nxt: self.nxt.close()
    if self.prv: self.prv.close()
  def closeCommon(self):
    if self.geo: self.geo.close()
    if self.full: self.full.close()
  def __str__(self):
    return """
      ifile: %(ifile)i
      prev: %(prv)s
      curr: %(current)s
      next: %(nxt)s
    """ % self.__dict__

class WrfNcTime:
  def __init__(self, initialdate):
    self.is_singlerec = False
    self.nrec = 0
    self.initialdate = initialdate
    self.iini = 0
    self.iend = None
    self.outstep_s = 0
  def checkStep(self, wrfnc):
    # TODO: Check here that the next file does not have a time gap
    incTimes = wrfnc.current.variables["Times"]
    t0 = datetime.strptime(charr2str(incTimes[0]), '%Y-%m-%d_%H:%M:%S')
    if len(incTimes) > 1:
      t1 = datetime.strptime(charr2str(incTimes[1]), '%Y-%m-%d_%H:%M:%S')
      delta = t1-t0
    elif wrfnc.nxt:
      t1 = datetime.strptime(
        charr2str(wrfnc.nxt.variables["Times"][0]), '%Y-%m-%d_%H:%M:%S'
      )
      delta = t1-t0
    elif wrfnc.prv:
      tp = datetime.strptime(
        charr2str(wrfnc.prv.variables["Times"][-1]), '%Y-%m-%d_%H:%M:%S'
      )
      delta = t0-tp
    else:
      t1 = datetime.strptime(charr2str(incTimes[1]), '%Y-%m-%d_%H:%M:%S')
      delta = t1-t1
    self.outstep_s = 86400*delta.days + delta.seconds
  def getTimes(self, wrfnc, is_singlerec):
    incTimes = wrfnc.current.variables["Times"]
    self.nrec = len(incTimes)
    if is_singlerec:
      self.nrec = 1
    self.iend = self.iini + self.nrec
    times = map(charr2str,incTimes[:self.nrec])
    return map(lambda x: str2offset(x,self.initialdate), times)
  def cycle(self):
    self.iini += self.nrec
  def __str__(self):
    return """
        iini = %(iini)d
        iend = %(iend)d
        nrec = %(nrec)d
        outstep_s = %(outstep_s)d
    """ % self.__dict__

class ParseTransform:
  def __init__(self, transformstr):
    self.transformstr = transformstr
    words = ""
    lastchar="X"
    for char in transformstr:
      if char in ["+", "-"] and lastchar == "E":
        # Takes care of constants in exp notation, e.g. 4.8e-3
        words += char
      elif char in ["*", "+", "-", "(", ")", "[", "]", ":", "/"]:
        words += " "
      else:
        words += char
      lastchar = char.upper()
    words = words.split()
    self.variables = []
    self.constants = []
    self.functions = []
    for word in words:
      try:
        self.constants.append(float(word))
      except:
        if word == word.upper():
          self.variables.append(word)
        else:
            self.functions.append(word)
  def execute(self, varobj, onc, wnfiles, wntimes, vardic):
    cmdstr = self.transformstr
    if self.variables:
      for var in set(self.variables):
        cmdstr = cmdstr.replace(var, "vardic['%s']"%var)
      print "Executing -> %s" % cmdstr
      exec("copyval = %s" % cmdstr)
      oncvar = get_oncvar(
        varobj, wnfiles.current.variables[self.variables[0]], onc
      )
    else:
      print "  Using function: %s" % self.functions[0]
      process_func = globals()[self.functions[0]]
      oncvar, copyval = process_func(varobj, onc, wnfiles, wntimes)
    return oncvar, copyval
  def __str__(self):
    return """
      variables: %(variables)s
      functions: %(functions)s
      constants: %(constants)s
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
      try:
        v.transform = line[5]
      except:
        v.transform = ""
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

