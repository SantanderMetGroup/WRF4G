from wrfncxnj_base import *
from wrfncxnj_fun import *

if not opt.OFILE:
  sys.stderr.write("Missing output file!")
  sys.exit()
DEBUG = 1
if opt.quiet:
  DEBUG = 0
if opt.globfiles:
  files = glob(opt.globfiles)
  files.sort()
elif opt.filelist:
  files = map(string.strip, open(opt.filelist, "r").readlines())
else:
  files = args
vars = opt.vars.split(',')
vars = stdvars(vars, opt.vtable)
is_geofile = False
if not files and opt.geofile:
  print "No input files provided. Trying to find the variables in the geo_em file provided."
  files = [opt.geofile,]
  is_geofile = True
#
#  Clone the structure of the netcdf file and get the initial time from the first file.
#
print files[0]
initialdate = datetime.strptime(opt.refdate, '%Y-%m-%d_%H:%M:%S')
onc = create_bare_curvilinear_CF_from_wrfnc(files[0], initialdate, opt.zaxis, opt.paxis, opt.saxis)
for line in csv.reader(open(opt.attributes, "r"), delimiter=" ", skipinitialspace=True):
  setattr(onc, line[0], line[1])
onctime = onc.variables["time"]
onclat = onc.variables["lat"]
onclon = onc.variables["lon"]
#
#  Loop over files extracting variables and times
#
if opt.discard:
  files = discard_suspect_files(files, opt.discard)
nfiles = len(files)
itime = 0
lastpr = None
for ifile in range(nfiles):
  if DEBUG: print "Processing file %s" % files[ifile]
  inc = NetCDFFile(files[ifile],'r')
  #
  #   Open the next and previous files for accumulations
  #
  incnext = None
  if ifile != nfiles - 1:
    incnext = NetCDFFile(files[ifile+1],'r')
  incprev = None
  if ifile:
    incprev = NetCDFFile(files[ifile-1],'r')
  #
  #  Get the times
  #
  incTimes = inc.variables["Times"]
  nrecords = len(incTimes)
  if opt.singlerec:
    nrecords = 1
  times = map(charr2str,incTimes[:nrecords])
  times = map(lambda x: str2offset(x,initialdate), times)
  #
  #  Set times and loop variables
  #
  onctime[itime:itime+nrecords] = times
  for varname in vars:
    if DEBUG: print "Processing var %s" % varname 
    if varname in ["U10ER", "V10ER"]:
      u = inc.variables["U10"]
      v = inc.variables["V10"]
      uer, ver = rotate_lcc_wind(u,v, onclat[:,:], onclon[:,:], inc.CEN_LON, inc.TRUELAT1, inc.TRUELAT2)
      exec("incvar=%s" % varname[0].lower()) # muy cerdo
      exec("copyval=%ser" % varname[0].lower()) # muy cerdo
      copyval = reshape(incvar[:nrecords], incvar.shape[:1]+(1,)+incvar.shape[1:])
      oncvar = get_oncvar(vars[varname], incvar, onc, screenvar_at_10m=True)
      oncvar[itime:itime+nrecords] = copyval[:nrecords].astype(oncvar.typecode())
    elif varname in ["UER", "VER"]:
      if inc.variables.has_key("UU"):
        u = inc.variables["UU"]
        v = inc.variables["VV"]
      else:   
        u = inc.variables["U"]
        v = inc.variables["V"]
      uer, ver = rotate_lcc_wind(u,v, onclat[:,:], onclon[:,:], inc.CEN_LON, inc.TRUELAT1, inc.TRUELAT2)
      exec("incvar=%s" % varname[0].lower()) # muy cerdo
      exec("copyval=%ser" % varname[0].lower()) # muy cerdo       
      oncvar = get_oncvar(vars[varname], incvar, onc)
      oncvar[itime:itime+nrecords] = copyval[:nrecords].astype(oncvar.typecode())
    elif varname in ["U10MER", "V10MER"]:
      u = inc.variables["U10MEAN"]
      v = inc.variables["V10MEAN"]
      uer, ver = rotate_lcc_wind(u,v, onclat[:,:], onclon[:,:], inc.CEN_LON, inc.TRUELAT1, inc.TRUELAT2)
      exec("incvar=%s" % varname[0].lower()) # muy cerdo
      exec("copyval=%ser" % varname[0].lower()) # muy cerdo
      copyval = reshape(incvar[:nrecords], incvar.shape[:1]+(1,)+incvar.shape[1:])
      oncvar = get_oncvar(vars[varname], incvar, onc, screenvar_at_10m=True)
      oncvar[itime:itime+nrecords] = copyval[:nrecords].astype(oncvar.typecode())
    elif varname in ["U10XER", "V10XER"]:
      u = inc.variables["U10MAX"]
      v = inc.variables["V10MAX"]
      uer, ver = rotate_lcc_wind(u,v, onclat[:,:], onclon[:,:], inc.CEN_LON, inc.TRUELAT1, inc.TRUELAT2)
      exec("incvar=%s" % varname[0].lower()) # muy cerdo
      exec("copyval=%ser" % varname[0].lower()) # muy cerdo
      copyval = reshape(incvar[:nrecords], incvar.shape[:1]+(1,)+incvar.shape[1:])
      oncvar = get_oncvar(vars[varname], incvar, onc, screenvar_at_10m=True)
      oncvar[itime:itime+nrecords] = copyval[:nrecords].astype(oncvar.typecode())
    elif varname == "WIND":
      incvar = inc.variables["U10"]
      u = incvar[:]
      v = inc.variables["V10"][:]
      copyval = Numeric.sqrt(u*u+v*v)
      oncvar = get_oncvar(vars[varname], incvar, onc)
      oncvar[itime:itime+nrecords] = copyval[:nrecords].astype(oncvar.typecode())
#    elif varname == "RAIN":
#      if inc.variables.has_key("RAINTOT"):
#        incvar = inc.variables["RAINTOT"]
#        pr = incvar[:nrecords]
#      else:
#        incvar = inc.variables["RAINNC"]
#        pr = incvar[:nrecords] + inc.variables["RAINC"][:nrecords]
#      if not lastpr:
#        copyval = concatenate([zeros((1,)+pr[0].shape, pr.typecode()), pr[1:nrecords]-pr[:-1]])
#      else:
#        copyval = pr - concatenate([lastpr,pr[:-1]])
#      copyval = where(copyval<0., 0, copyval)
#      lastpr = reshape(pr[-1], (1,)+pr[-1].shape)
#      oncvar = get_oncvar(vars[varname], incvar, onc)
#      oncvar[itime:itime+nrecords] = copyval[:nrecords].astype(oncvar.typecode())
    elif varname == "RAINFORWARD":
      if inc.variables.has_key("RAINTOT"):
        incvar = inc.variables["RAINTOT"]
        pr = incvar[:nrecords]
      else:
        incvar = inc.variables["RAINNC"]
        pr = incvar[:nrecords] + inc.variables["RAINC"][:nrecords]
      if ifile == nfiles-1:
        copyval = concatenate([pr[1:nrecords]-pr[:-1], zeros((1,)+pr[0].shape, pr.typecode())])
      else:
        if incnext.variables.has_key("RAINTOT"):
          nextpr = incnext.variables["RAINTOT"]
          nextpr = reshape(nextpr[0], (1,)+nextpr[0].shape)
        else:
          nextpr = incnext.variables["RAINNC"]
          nextpr = reshape(nextpr[0] + incnext.variables["RAINC"][0], nextpr[0].shape)
        incnext.close()
        copyval = concatenate([pr[1:nrecords], nextpr]) - pr
      copyval = where(copyval<0., 0, copyval)
      lastpr = reshape(pr[-1], (1,)+pr[-1].shape)
      oncvar = get_oncvar(vars[varname], incvar, onc)
      oncvar[itime:itime+nrecords] = copyval[:nrecords].astype(oncvar.typecode())
    elif varname == "RAINC":
      incvar = inc.variables["RAINC"]
      pr = incvar[:nrecords]
      if not lastpr:
        copyval = concatenate([zeros((1,)+pr[0].shape, pr.typecode()), pr[1:nrecords]-pr[:-1]])
      else:
        copyval = pr - concatenate([lastpr,pr[:-1]])
      copyval = where(copyval<0., 0, copyval)
      lastpr = reshape(pr[-1], (1,)+pr[-1].shape)
      oncvar = get_oncvar(vars[varname], incvar, onc)
      oncvar[itime:itime+nrecords] = copyval[:nrecords].astype(oncvar.typecode())
    elif varname in ["T2MAX", "T2MIN", "T2MEAN"]:
      incvar = inc.variables[varname]
      copyval = reshape(incvar[:nrecords], incvar.shape[:1]+(1,)+incvar.shape[1:])
      oncvar = get_oncvar(vars[varname], incvar, onc, screenvar_at_2m=True)
      oncvar[itime:itime+nrecords] = copyval[:nrecords].astype(oncvar.typecode())
    elif varname in ["U10", "V10"]:
      incvar = inc.variables[varname]
      copyval = reshape(incvar[:nrecords], incvar.shape[:1]+(1,)+incvar.shape[1:])
      oncvar = get_oncvar(vars[varname], incvar, onc, screenvar_at_10m=True)
      oncvar[itime:itime+nrecords] = copyval[:nrecords].astype(oncvar.typecode())
    elif varname == "PRES":
      incvar = inc.variables['P']
      copyval = incvar[:nrecords] + inc.variables["PB"][:nrecords]
      oncvar = get_oncvar(vars[varname], incvar, onc)  
      oncvar[itime:itime+nrecords] = copyval[:nrecords].astype(oncvar.typecode())
    elif varname == "GEOP":
      incvar = inc.variables['PH']
      copyval = incvar[:nrecords] + inc.variables["PHB"][:nrecords]
      # De-stagger the geopotential
      copyval = (copyval[:,:-1]+copyval[:,1:])/2.
      oncvar = get_oncvar(vars[varname], incvar, onc)
      oncvar[itime:itime+nrecords] = copyval[:nrecords].astype(oncvar.typecode())
    elif varname == "TEMP":
      if inc.variables.has_key("TT"):
        incvar = inc.variables['TT']
        copyval = incvar
      else:
        incvar = inc.variables['T']
        pres = inc.variables['P'][:nrecords] + inc.variables["PB"][:nrecords]
        copyval = compute_temperature(pres, incvar[:nrecords])
      oncvar = get_oncvar(vars[varname], incvar, onc)
      oncvar[itime:itime+nrecords] = copyval[:nrecords].astype(oncvar.typecode())
    elif varname == "MSLP":
      if inc.variables.has_key("MSLP"):
        incvar = inc.variables['MSLP']
        mslp = incvar[:nrecords]
        oncvar = get_oncvar(vars[varname], incvar, onc)
      else:
        incvar = inc.variables['P']
        p = incvar[:]
        pb = inc.variables['PB'][:]
        ph = inc.variables['PH'][:]
        phb = inc.variables['PHB'][:]
        t = inc.variables['T'][:]
        qvapor = inc.variables['QVAPOR'][:]
        mslp = compute_mslp(p, pb, ph, phb, t , qvapor)
        oncvar = get_oncvar(vars[varname], incvar, onc, out_is_2D_but_in_3D=True)
      oncvar[itime:itime+nrecords] = mslp[:nrecords].astype(oncvar.typecode())
    elif varname=="RSS":
      incvar = inc.variables['SWDOWN']
      copyval = incvar[:nrecords]*(1 - inc.variables['ALBEDO'][:nrecords])
      oncvar = get_oncvar(vars[varname], incvar, onc)
      oncvar[itime:itime+nrecords] = copyval[:nrecords].astype(oncvar.typecode())
    elif varname=="RLS":
      incvar = inc.variables['GLW']
      glw = incvar[:]
      emis = inc.variables['EMISS'][:]
      tskin = inc.variables['SKINTEMP'][:]
      copyval = emis[:nrecords]*glw[:nrecords] - 5.67e-8*emis[:nrecords]*tskin[:nrecords]**4
      oncvar = get_oncvar(vars[varname], incvar, onc)
      oncvar[itime:itime+nrecords] = copyval[:nrecords].astype(oncvar.typecode())
    elif varname=="MRSO":
      incvar = inc.variables['SMOIS']
      smois = incvar[:]
      layer_width = inc.variables['DZS'][:]
      smois = smois*layer_width[:,:,NewAxis,NewAxis]     
      copyval = sum(smois, axis=1)*1000
      oncvar = get_oncvar(vars[varname], incvar, onc, out_is_2D_but_in_3D=True)
      oncvar[itime:itime+nrecords] = copyval[:nrecords].astype(oncvar.typecode())
    elif varname=="MRSOS":
      incvar = inc.variables['SMOIS']
      copyval = incvar[:,0,:,:]*1000
      oncvar = get_oncvar(vars[varname], incvar, onc, out_is_2D_but_in_3D=True)
      oncvar[itime:itime+nrecords] = copyval[:nrecords].astype(oncvar.typecode())
    elif varname=="CLD":
      incvar = inc.variables['VIQC']
      copyval = incvar[:nrecords] + inc.variables['VIQI'][:nrecords]
      oncvar = get_oncvar(vars[varname], incvar, onc)
      oncvar[itime:itime+nrecords] = copyval[:nrecords].astype(oncvar.typecode())
    elif "compute_%s" % varname in locals():
      if DEBUG: print "  Using the compute_%s function" % varname
      process_func = locals()["compute_%s" % varname]
      oncvar, copyval = process_func(vars[varname], onc, inc, incprev, incnext)
      oncvar[itime:itime+nrecords] = copyval[:nrecords].astype(oncvar.typecode())
    else:
      incvar = inc.variables[varname]
      oncvar = get_oncvar(vars[varname], incvar, onc)
      oncvar[itime:itime+nrecords] = (incvar[:nrecords] * vars[varname].scale + vars[varname].offset).astype('f')
  itime += nrecords
  inc.close()
  onc.sync()

onc.sync()
onc.close()
