import os, sys

outputdevice = {
  "jpg": "jpeg"
}

class FigFile:
  def __init__(self, strdef):
    self.master, self.outfig, self.outformat = strdef.strip().split()
    self.keys = []
    self.values = []

def parse_input(ifile):
  we_are_in = 0
  figfiles = []
  for line in open(ifile, 'r'):
    if line[0] == '#':
      continue
    elif line[0] != ' ':
      we_are_in = 1
      try:
        figfiles.append(current)
      except:
        pass
      current = FigFile(line)
    else:
      if we_are_in:
        if line.startswith('    '):
          current.values.append(line.strip())
        elif line.startswith('  '):
          current.keys.append(line.strip())
  figfiles.append(current)
  return figfiles

figs = parse_input(sys.argv[1])

for f in figs:
  mfile = open(f.master, "r").readlines()
  outfile = []
  for line in mfile:
    out=line
    for i in range(len(f.keys)):
      out=out.replace(f.keys[i], f.values[i])
    outfile.append(out)
  fp = open(f.outfig, "w")
  fp.writelines(outfile)
  fp.close()
  os.system("fig2dev -L%s %s %s" % (outputdevice[f.outformat], f.outfig, f.outfig.replace('.fig','.'+f.outformat)))
  os.system("rm %s" % f.outfig)

