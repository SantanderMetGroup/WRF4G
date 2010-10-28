from wrfncxnj_base import *
from wrfncxnj_fun import *

wrfncIter = WrfNcFiles()
if opt.geofile:
  wrfncIter.geo = NetCDFFile(opt.geofile, "r")
if opt.fullfile:
  wrfncIter.full = NetCDFFile(opt.geofile, "r")

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
if not opt.vtable:
  opt.vtable = sys.argv[0].replace(".py", ".table")
vars = stdvars(vars, opt.vtable)
is_geofile = False
if not files and wrfncIter.geo:
  print "No input files provided."
  print "Trying to find the variables in the geo_em file provided."
  files = [opt.geofile,]
  is_geofile = True
#
#  Clone the structure of the netcdf file and get the initial time from the first file.
#
print files[0]
if not opt.refdate:
  opt.refdate = "1950-01-01_00:00:00"
wnt = WrfNcTime(datetime.strptime(opt.refdate, '%Y-%m-%d_%H:%M:%S'))
onc = create_bare_curvilinear_CF_from_wrfnc(files[0], wnt.initialdate, opt.zaxis, opt.paxis, opt.saxis)
if opt.attributes:
  for line in csv.reader(
    open(opt.attributes, "r"),
    delimiter=" ",
    skipinitialspace=True
  ):
    setattr(onc, line[0], line[1])
onctime = onc.variables["time"]
onclat = onc.variables["lat"]
onclon = onc.variables["lon"]
#
#  Loop over files extracting variables and times
#
if opt.discard:
  files = discard_suspect_files(files, opt.discard)
wrfncIter.loadFiles(files, opt.prevfile, opt.nextfile)
for wrfnc in wrfncIter:
  if DEBUG: print wrfnc
  if not opt.singlerec:
    wnt.checkStep(wrfnc)
  times = wnt.getTimes(wrfnc, opt.singlerec)
  if DEBUG: print wnt
  #
  #  Set times and loop variables
  #
  onctime[wnt.iini:wnt.iend] = times
  for varname in vars:
    if DEBUG: print "Processing var %s" % varname 
    if varname == "WIND":
      incvar = wrfnc.current.variables["U10"]
      u = incvar[:]
      v = wrfnc.current.variables["V10"][:]
      copyval = Numeric.sqrt(u*u+v*v)
      oncvar = get_oncvar(vars[varname], incvar, onc)
      oncvar[wnt.iini:wnt.iend] = copyval[:wnt.nrec].astype(oncvar.typecode())
    elif varname in ["U10", "V10"]:
      incvar = wrfnc.current.variables[varname]
      copyval = reshape(incvar[:wnt.nrec], incvar.shape[:1]+(1,)+incvar.shape[1:])
      oncvar = get_oncvar(vars[varname], incvar, onc, screenvar_at_10m=True)
      oncvar[wnt.iini:wnt.iend] = copyval[:wnt.nrec].astype(oncvar.typecode())
    elif varname == "PRES":
      incvar = wrfnc.current.variables['P']
      copyval = incvar[:wnt.nrec] + wrfnc.current.variables["PB"][:wnt.nrec]
      oncvar = get_oncvar(vars[varname], incvar, onc)  
      oncvar[wnt.iini:wnt.iend] = copyval[:wnt.nrec].astype(oncvar.typecode())
    elif varname == "GEOP":
      incvar = wrfnc.current.variables['PH']
      copyval = incvar[:wnt.nrec] + wrfnc.current.variables["PHB"][:wnt.nrec]
      # De-stagger the geopotential
      copyval = (copyval[:,:-1]+copyval[:,1:])/2.
      oncvar = get_oncvar(vars[varname], incvar, onc)
      oncvar[wnt.iini:wnt.iend] = copyval[:wnt.nrec].astype(oncvar.typecode())
    elif varname == "TEMP":
      if wrfnc.current.variables.has_key("TT"):
        incvar = wrfnc.current.variables['TT']
        copyval = incvar
      else:
        incvar = wrfnc.current.variables['T']
        pres = wrfnc.current.variables['P'][:wnt.nrec] + wrfnc.current.variables["PB"][:wnt.nrec]
        copyval = compute_temperature(pres, incvar[:wnt.nrec])
      oncvar = get_oncvar(vars[varname], incvar, onc)
      oncvar[wnt.iini:wnt.iend] = copyval[:wnt.nrec].astype(oncvar.typecode())
    elif varname == "MSLP":
      if wrfnc.current.variables.has_key("MSLP"):
        incvar = wrfnc.current.variables['MSLP']
        mslp = incvar[:wnt.nrec]
        oncvar = get_oncvar(vars[varname], incvar, onc)
      else:
        incvar = wrfnc.current.variables['P']
        p = incvar[:]
        pb = wrfnc.current.variables['PB'][:]
        ph = wrfnc.current.variables['PH'][:]
        phb = wrfnc.current.variables['PHB'][:]
        t = wrfnc.current.variables['T'][:]
        qvapor = wrfnc.current.variables['QVAPOR'][:]
        mslp = compute_mslp(p, pb, ph, phb, t , qvapor)
        oncvar = get_oncvar(vars[varname], incvar, onc, out_is_2D_but_in_3D=True)
      oncvar[wnt.iini:wnt.iend] = mslp[:wnt.nrec].astype(oncvar.typecode())
    elif varname=="RSS":
      incvar = wrfnc.current.variables['SWDOWN']
      copyval = incvar[:wnt.nrec]*(1 - wrfnc.current.variables['ALBEDO'][:wnt.nrec])
      oncvar = get_oncvar(vars[varname], incvar, onc)
      oncvar[wnt.iini:wnt.iend] = copyval[:wnt.nrec].astype(oncvar.typecode())
    elif varname=="MRSO":
      incvar = wrfnc.current.variables['SMOIS']
      smois = incvar[:]
      layer_width = wrfnc.current.variables['DZS'][:]
      smois = smois*layer_width[:,:,NewAxis,NewAxis]     
      copyval = sum(smois, axis=1)*1000
      oncvar = get_oncvar(vars[varname], incvar, onc, out_is_2D_but_in_3D=True)
      oncvar[wnt.iini:wnt.iend] = copyval[:wnt.nrec].astype(oncvar.typecode())
    elif varname=="CLD":
      incvar = wrfnc.current.variables['VIQC']
      copyval = incvar[:wnt.nrec] + wrfnc.current.variables['VIQI'][:wnt.nrec]
      oncvar = get_oncvar(vars[varname], incvar, onc)
      oncvar[wnt.iini:wnt.iend] = copyval[:wnt.nrec].astype(oncvar.typecode())
    elif "compute_%s" % varname in locals():
      if DEBUG: print "  Using the compute_%s function" % varname
      process_func = locals()["compute_%s" % varname]
      oncvar, copyval = process_func(vars[varname], onc, wrfnc, wnt)
      oncvar[wnt.iini:wnt.iend] = copyval[:wnt.nrec].astype(oncvar.typecode())
    elif vars[varname].transform:
      pt = ParseTransform(vars[varname].transform)
      if DEBUG: print pt
      vardic = {}
      for var in pt.variables:
        vardic[var] = wrfnc.current.variables[var][:]
      oncvar, copyval = pt.execute(vars[varname], onc, wrfnc, wnt, vardic)
      oncvar[wnt.iini:wnt.iend] = copyval[:wnt.nrec].astype(oncvar.typecode())
    else:
      incvar = wrfnc.current.variables[varname]
      oncvar = get_oncvar(vars[varname], incvar, onc)
      oncvar[wnt.iini:wnt.iend] = incvar[:wnt.nrec].astype('f')
  wnt.cycle()
  onc.sync()

onc.sync()
onc.close()
wrfncIter.closeCommon()
