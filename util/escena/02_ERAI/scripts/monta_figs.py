import os

masterfig="sclim.fig"
outputformat="jpg"
outputdevice="jpeg"

translate_dics = {
# "/home/chus/tareas/escena/02_ERAI/figs/sclim_tasmax.fig": {
#   "title": "Maximum temperature climatologies (1990-2003)",
# },
# "/home/chus/tareas/escena/02_ERAI/figs/sclim_tasmin.fig": {
#   "tasmax": "tasmin",
#   "title": "Minimum temperature climatologies (1990-2003)",
# },
 "/home/chus/tareas/escena/02_ERAI/figs/sclim_pr.fig": {
   "tasmax": "pr",
   "title": "Precipitation climatologies (1990-2003)",
   "degC": "mm/day",
   "EOBS025": "Spain02",
   "EOBS\\": "Spain02\\",
 },
}

thefile = open(masterfig, "r").readlines()
for ofile in translate_dics.keys():
  outfile = []
  for line in thefile:
    out=line
    for key in translate_dics[ofile].keys():
      out=out.replace(key, translate_dics[ofile][key])
    outfile.append(out)
  fp = open(ofile, "w")
  fp.writelines(outfile)
  fp.close()
  os.system("fig2dev -L%s %s %s" % (outputdevice, ofile, ofile.replace('.fig','.'+outputformat)))
  os.system("rm %s" % ofile) 
