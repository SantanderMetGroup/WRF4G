#! /bin/bash
scriptdir=$( (cd `dirname $0` && echo $PWD) )
source /software/ScientificLinux/4.6/etc/bashrc || source /home/usr/etc/bashrc
source ${scriptdir}/dirs
source ${scriptdir}/diagnostics.sh
source ${scriptdir}/plot_util.sh


function py_getxyz_curv(){
  ncfilevar=$1
  irec=$2
python << End_Of_Python
from Scientific.IO.NetCDF import *
import os, sys, time
dataset = "${ncfilevar}"   # file.nc:var
irec = ${irec}
ifile, varname = dataset.split(":")
nc = NetCDFFile(ifile, "r")
var = nc.variables[varname]
lats = nc.variables["lat"]
lons = nc.variables["lon"]
for i in range(len(lats)):
  for j in range(len(lats[0])):
    print "%9.4f %9.4f %.5e" % (lons[i,j], lats[i,j], var[irec,i,j])
End_Of_Python
}


gmtset PAPER_MEDIA a4+
gmtset PLOT_DEGREE_FORMAT dddF
gmtset PAGE_ORIENTATION portrait

RFLAG="-R-25/61/-44/46"
NEARNREG="-R-13/8/33/46"
RJFLAG="${RFLAG} -JM18c"
BFLAG="-Bf5a10/f5a10WeSn"
cres="-Dl"

XYZFILE=pepe.xyz
cptfile=pepe.cpt
FNAMEOUT=${FIGSDIR}/regions.eps

py_getxyz_curv ${scriptdir}/CORDEX_UC_WRF_SEN2CTRL_orog.nc:orog 0 > ${XYZFILE}
makecpt -C${scriptdir}/cpts/terrain.cpt -T0/3000/500 -Z > $cptfile

psxy /dev/null $RJFLAG -K > $FNAMEOUT
#psbasemap $RJFLAG -Bg10 -O -K >> $FNAMEOUT
pscoast $RJFLAG -Gc -A0/0/1 ${cres} -O -K >> $FNAMEOUT
psxy ${XYZFILE} $RJFLAG -C${cptfile} -Ss0.13c -O -K >> $FNAMEOUT
pscoast $RJFLAG -Q -O -K >> $FNAMEOUT
pscoast $RJFLAG $BFLAG -A0/0/1 ${cres} -W5 -O -K >> $FNAMEOUT
for region in AM WA-N WA-S CA-NH CA-SH SA-WN SA-WS SA-E EA EH
do
  dump_region_border $region \
    | psxy $RJFLAG -A -W4,255/0/0 -O -K >> $FNAMEOUT
  dump_region_center $region \
    | awk '{printf "%f %f 12 0 0 CM '${region}'", $1,$2}' \
    | pstext $RJFLAG -G255/0/0 -O -K >> $FNAMEOUT
done
psxy /dev/null $RJFLAG -O >> $FNAMEOUT

ps2raster -A -Tg $FNAMEOUT
rm -f $FNAMEOUT

rm -f $XYZFILE $cptfile


#bash plot_africa.gmt.sh '/home/chus/tareas/cordex/01_CDX_Sensitivity/CORDEX_UC_WRF_SEN2CTRL_orog.nc' var orog rec 0 cpt cpts/terrain.cpt is_curvilinear zmin 0 zmax 3000 dz 500 only_land




