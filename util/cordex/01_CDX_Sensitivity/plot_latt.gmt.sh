source /home/usr/etc/bashrc

NCFILE=$1

R="-R0/365/-40/40"
RJ="${R} -JX18c/8c"
B="-Bf10a30/f5a10WeSn"
var=$(basename ${NCFILE//*_/} .nc)

shift
zmin=0
zmax=0
dz=0
out=""
while test "$*"
do
  case $1 in
    label) label=$2; shift;;
    title) title=$2; shift;;
    addc) addc=$2; shift;;
    cpt) cptfile=$2; shift;;
    var) var=$2; shift;;
    zmin) zmin=$2; shift;;
    zmax) zmax=$2; shift;;
    dz) dz=$2; shift;;
    out) out=$2; shift;;
    validrange) validrange=$2; shift;;
  esac
  shift
done

test -n "${cptfile}" || cptfile="cpts/${var}.cpt"

if test -n "$out"; then
  FNAMEOUT="figs/$(basename $out .eps)${label}.eps"
else
  FNAMEOUT="figs/$(basename $NCFILE .nc)${label}.eps"
fi
XYZFILE=$(basename $NCFILE .nc)${label}.xyz

function py_getxyz(){
  ncfilevar=$1
  irec=$2
python << End_Of_Python
from Scientific.IO.NetCDF import *
import os, sys, time
dataset = "${ncfilevar}"   # file.nc:var
ifile, varname = dataset.split(":")
nc = NetCDFFile(ifile, "r")
var = nc.variables[varname]
try:
  lats = nc.variables["lat"]
except KeyError:
  lats = nc.variables["latitude"]
times = nc.variables["time"]
itime=2 # for 3-day running mean!
for i in range(len(times)):
  for j in range(len(lats)):
    print "%9.4f %9.4f %.5e" % (itime, lats[j], var[i,j])
  itime+=1
End_Of_Python
}

gmtset PAPER_MEDIA a4+
gmtset PLOT_DEGREE_FORMAT dddF
gmtset PAGE_ORIENTATION portrait

py_getxyz $NCFILE:${var} > $XYZFILE

if test -n "${addc}"; then
  awk '{print $1,$2,'${addc}'+$3}' $XYZFILE > ${XYZFILE}.tmp; mv ${XYZFILE}.tmp $XYZFILE
fi
if test -n "${validrange}"; then
  read lim1 lim2 <<< ${validrange/,/ }
  awk '$3 <= '${lim2}' && $3 >= '${lim1}'{print $1,$2,$3}' $XYZFILE > ${XYZFILE}.tmp; mv ${XYZFILE}.tmp $XYZFILE
fi

echo -n $XYZFILE; minmax $XYZFILE

if test $zmin -ne $zmax; then
  makecpt -C$cptfile -T${zmin}/$zmax/$dz -Z > pepe.cpt
  cptfile=pepe.cpt
fi


psxy /dev/null $RJ -K > $FNAMEOUT
psbasemap $RJ $B -O -K >> $FNAMEOUT
psxy ${XYZFILE} ${RJ} -Ss0.08c -C${cptfile} -O -K >> $FNAMEOUT
test -n "$title" && pstext $RJFLAG << EOF >> $FNAMEOUT
-20 -40 40 0 0 LB ${title}
EOF
psxy /dev/null $RJ -O >> $FNAMEOUT

psscale -D8c/8c/14c/1ch -C${cptfile} -E -L > ${FNAMEOUT/.eps/.scale.eps}
fixbb ${FNAMEOUT/.eps/.scale.eps} tmp.eps
mv tmp.eps ${FNAMEOUT/.eps/.scale.eps}

# Convert to JPG and drop the eps.
ps2raster -A -Tg $FNAMEOUT 
rm -f $FNAMEOUT
rm -f pepe.cpt pepe.grd ${XYZFILE} .gmt*
