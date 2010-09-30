from optparse import OptionParser
parser = OptionParser()
parser.set_defaults(quiet=False,singlerec=False)
parser.add_option(
  "-f", "--files", dest="globfiles",
  help="Regular expression to be parsed by python to get the input files to process", metavar="REGEXP"
)
parser.add_option(
  "--from-file", dest="filelist", default="",
  help="Text file containing the input files. One per row", metavar="FILELIST.txt"
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
  "--single-record", action="store_true", dest="singlerec",
  help="Save only one record. Useful to extract fixed fields (LANDMASK, LANDUSE, ...)"
)
parser.add_option(
  "-z", action="store_true", default=False, dest="zaxis",
  help="Create Z axis information"
)
parser.add_option(
  "-s", action="store_true", default=False, dest="saxis",
  help="Create soil layer axis information"
)
parser.add_option(
  "-p", action="store_true", default=False, dest="paxis",
  help="Create pressure level axis information"
)
parser.add_option(
  "--time-bounds", dest="tbounds", metavar="H1,H2",
  help="Create a time_bnds variable to specify the period of time considered in each time record. H1 is the start time in hours from the current time record and H2 is the ending time"
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
parser.add_option(
  "--fullfile", metavar="wrfout_d0X_allvars.nc", dest="fullfile",
  help="wrfout file to be used for variables not found in the data files. For instance if you removed variables as the eta or soil levels."
)
(opt, args) = parser.parse_args()
