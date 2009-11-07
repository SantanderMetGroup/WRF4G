#
#  The following additional variables can be asked for:
#
#    "U10ER", "V10ER", "MSLP", "WIND"
#
from pyclimate.JDTimeHandler import *
from pyclimate.ncstruct import *
from Scientific.IO.NetCDF import *
#from netCDF3 import Dataset as NetCDFFile
from Numeric import *
from glob import glob
from datetime import datetime
import sys, time, csv

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

def compute_temperature(p, pb, t):
  # Some required physical constants:
  R=287.04
  g=9.81
  cp           = 7.*R/2.
  rcp          = R/cp
  p1000mb      = 100000.
  # Transpose and get full variables out of perturbations and potential T
  p = p + pb
  return (t+300.)*(p/p1000mb)**rcp

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

def create_bare_curvilinear_CF_from_wrfnc(wrfncfile, idate, createz=None):
  inc = NetCDFFile(wrfncfile,'r')
  onc = NetCDFFile(opt.OFILE, "w")
  onc.history = "Created by %s on %s" % (sys.argv[0],time.ctime(time.time()))
  onc.sync()
  #pdb.set_trace()
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
  if createz:
    onc.createDimension("z", inc.dimensions["bottom_top"])
    oncz = onc.createVariable("z",Numeric.Float64, ("z",))
    oncz.axis = "Z"
    oncz.long_name = "sigma at layer midpoints"
    oncz.positive = "down"
    oncz.standard_name = "atmosphere_sigma_coordinate"
    oncz.formula_terms = "sigma: z ps: PSFC ptop: PTOP"
    if inc.variables.has_key("ZNU"):
      oncz.assignValue(inc.variables["ZNU"][0])
    if inc.variables.has_key("P_TOP"):
      oncptop = onc.createVariable("PTOP",Numeric.Float32, ())
      oncptop.long_name = "Pressure at the top of the atmosphere"
      oncptop.units = "Pa"
      oncptop.assignValue(inc.variables["P_TOP"][0])
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
  onclat.standard_name = "Latitude"
  onclat.units = "degrees_north"
  onclat[:len(lats)] = lats
  onclon = onc.createVariable("lon",Numeric.Float32, ("y","x"))
  onclon.long_name = "Longitude"
  onclon.standard_name = "Longitude"
  onclon.units = "degrees_east"
  onclon[:len(lons)] = lons
  #
  #   Get the initial date and create a new time variable
  #
  onc.createDimension("time", None)
  onctime = onc.createVariable("time",Numeric.Float64, ("time",))
  onctime.long_name = "Time variable"
  onctime.units = "hours since %s" % idate.strftime('%Y-%m-%d %H:%M:%S')
  inc.close()
  onc.sync()
  return onc

dimension_mapping = {
  "south_north": "y",
  "west_east": "x",
  "bottom_top": "z",
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
    varwrf = line[0]
    if varwrf in vars:
      v = Variable()
      v.standard_abbr = line[1]
      v.standard_name = line[2]
      v.units = line[3]
      v.scale = float(line[4])
      v.offset = float(line[5])
      rval[varwrf] = v
  return rval

if __name__ == "__main__":
  import pdb
  from optparse import OptionParser
  parser = OptionParser()
  parser.set_defaults(
    quiet=False
  )
  parser.add_option(
    "-f", "--files", dest="globfiles",
    help="Regular expression to be parsed by python to get the input files to process", metavar="REGEXP"
  )
  parser.add_option(
    "-v", "--variables", dest="vars",
    help="Variables to extract. Apart from those defined in the file, you can ask for any of the following derived variables: MSLP, U10ER, V10ER, WIND", metavar="VAR1[,VAR2,...]"
  )
  parser.add_option(
    "-d", "--discard-criteria", dest="discard",
    help="Enable discarding files. Currently only the uncommon_size criteria is implemented", metavar="uncommon_size"
  )
  parser.add_option(
    "-t", "--variable-table", dest="vtable",
    help="Table for translating WRF names into IPCC standard names", metavar="variable.table"
  )
  parser.add_option(
    "-a", "--attributes", dest="attributes",
    help="Table for setting the global attributes of the file", metavar="atributes.file"
  )
  parser.add_option(
    "-q", "--quiet", action="store_true",
    help="Run quietly"
  )
  parser.add_option(
    "-z", action="store_true", default=False, dest="zaxis",
    help="Create Z axis information"
  )
  parser.add_option(
    "-r", "--reference-date", dest="refdate",
    help="Reference date for the files", metavar="YYYY-MM-DD_hh:mm:ss"
  )
  parser.add_option(
    "-o", "--output", dest="OFILE", metavar="OUTPUTFILE.nc",
    help="Output file name"
  )
  parser.add_option(
    "-g", "--geofile", metavar="geo_em.d0X.nc", dest="geofile",
    help="geo_em file to be used. For instance if you already removed geographic variables from the input files"
  )
  (opt, args) = parser.parse_args()

  if not opt.OFILE:
    sys.stderr.write("Missing output file!")
    sys.exit()
  DEBUG = 1
  if opt.quiet:
    DEBUG = 0
  if opt.globfiles:
    files = glob(opt.globfiles)
    files.sort()
  else:
    files = args
  vars = opt.vars.split(',')
  vars = stdvars(vars, opt.vtable)
  #
  #  Clone the structure of the netcdf file and get the initial time from the first file.
  #
  print files[0]
  initialdate = datetime.strptime(opt.refdate, '%Y-%m-%d_%H:%M:%S')
  onc = create_bare_curvilinear_CF_from_wrfnc(files[0], initialdate, opt.zaxis)
  for line in csv.reader(open(opt.attributes, "r"), delimiter=" ", skipinitialspace=True):
    setattr(onc, line[0], line[1])
  onctime = onc.variables["time"]
  #
  #  Loop over files extracting variables and times
  #
  if opt.discard:
    files = discard_suspect_files(files, opt.discard)
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
          dims = ("time",)+map(lambda x: dimension_mapping[x], incvar.dimensions[1:])
          oncvar = onc.createVariable(varname, Numeric.Float32, dims)
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
          dims = ("time",)+tuple(map(lambda x: dimension_mapping[x], incvar.dimensions[1:]))
          oncvar = onc.createVariable(varname, Numeric.Float32, dims)
          oncvar.coordinates="lat lon"
          oncvar.grid_mapping = "Lambert_Conformal"
        else:
          oncvar = onc.variables[vars[varname].standard_abbr]
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
          dims = ("time",)+tuple(map(lambda x: dimension_mapping[x], incvar.dimensions[2:]))
          oncvar = onc.createVariable(varname, Numeric.Float32, dims)
          oncvar.units = "Pa"
          oncvar.description = "Mean sea level pressure"
          oncvar.coordinates="lat lon"
          oncvar.grid_mapping = "Lambert_Conformal"
        else:
          oncvar = onc.variables[vars[varname].standard_abbr]
        oncvar[itime:itime+nrecords] = mslp[:nrecords].astype(oncvar.typecode())
      else:
        incvar = inc.variables[varname]
        if not itime:
          dims = ("time",)+tuple(map(lambda x: dimension_mapping[x], incvar.dimensions[1:]))
          oncvar = onc.createVariable(vars[varname].standard_abbr, Numeric.Float32, dims)
          oncvar.long_name = vars[varname].standard_name
          oncvar.units = vars[varname].units
          oncvar.coordinates="lat lon"
          oncvar.grid_mapping = "Lambert_Conformal"
        else:
          oncvar = onc.variables[vars[varname].standard_abbr]
        oncvar[itime:itime+nrecords] = (incvar[:nrecords] * vars[varname].scale + vars[varname].offset).astype('f')
    itime += nrecords
    inc.close()
    onc.sync()
  
  onc.sync()
  onc.close()
