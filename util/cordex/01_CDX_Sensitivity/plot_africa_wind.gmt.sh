source /home/usr/etc/bashrc

RFLAG="-R-25/61/-44/46"
NEARNREG="-R-13/8/33/46"
RJFLAG="${RFLAG} -JM18c"
BFLAG="-Bf5a10/f5a10WeSn"
NCFILE1=$1
NCFILE2=$2

shift
has_height_dim=0
is_curvilinear=0
bg_is_curvilinear=0
national_bound=0
no_nn=0
rec=0
zmin=0
zmax=0
dz=0
out=""
title="."
maxsize="1."
skip=1
while test "$*"
do
  case $1 in
    has_height_dim) has_height_dim=1;;
    is_curvilinear) is_curvilinear=1;;
    bg_is_curvilinear) bg_is_curvilinear=1;;
    national_bound) national_bound=1;;
    no_nn) no_nn=1;;
    cptreverse) cptreverse=1;;
    label) label=$2; shift;;
    title) title=$2; shift;;
    cpt) cptfile=$2; shift;;
    u) uvar=$2; shift;;
    v) vvar=$2; shift;;
    rec) rec=$2; shift;;
    zmin) zmin=$2; shift;;
    zmax) zmax=$2; shift;;
    dz) dz=$2; shift;;
    out) out=$2; shift;;
    maxsize) maxsize=$2; shift;;
    skip) skip=$2; shift;;
    bg) bg=$2; shift;;
    var) var=$2; shift;;
  esac
  shift
done

test -n "${cptfile}" || cptfile="cpts/${var}.cpt" 

if test -n "$out"; then
  FNAMEOUT="figs/$(basename $out .eps)${label}.eps"
else
  FNAMEOUT="figs/$(basename $NCFILE1 .nc)${label}.eps"
fi
XYZFILE=$(basename $NCFILE1 .nc)${label}.xyz

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
for i in range(0,len(lats),${skip}):
  for j in range(0,len(lats[0]),${skip}):
    if len(var.shape)==4:
      print "%9.4f %9.4f %.5e" % (lons[i,j], lats[i,j], var[irec,0,i,j])
    else:
      print "%9.4f %9.4f %.5e" % (lons[i,j], lats[i,j], var[irec,i,j])
End_Of_Python
}

function py_getxyz(){
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
try:
  lats = nc.variables["lat"]
  lons = nc.variables["lon"]
except KeyError:
  lats = nc.variables["latitude"]
  lons = nc.variables["longitude"]
for i in range(0,len(lons),${skip}):
  for j in range(0,len(lats),${skip}):
    if ${has_height_dim}:
      print "%9.4f %9.4f %.5e" % (lons[i], lats[j], var[irec,0,j,i])
    else:
      print "%9.4f %9.4f %.5e" % (lons[i], lats[j], var[irec,j,i])
End_Of_Python
}

gmtset PAPER_MEDIA a4+
gmtset PLOT_DEGREE_FORMAT dddF
gmtset PAGE_ORIENTATION portrait

function scale_vec(){
  awk '{print $1,$2,atan2($6,$3)*180./3.14159265,sqrt($3*$3+$6*$6)/'${maxsize}'*0.6}'
}

if [ ${no_nn} -eq 0 ]; then
  if [ ${is_curvilinear} -ne 0 ]; then
    py_getxyz_curv ${NCFILE1}:${uvar} ${rec} > s1
    py_getxyz_curv ${NCFILE2}:${vvar} ${rec} > s2
  else
    py_getxyz ${NCFILE1}:${uvar} ${rec} > s1
    py_getxyz ${NCFILE2}:${vvar} ${rec} > s2
  fi
  #paste s1 s2 | awk '{print $1,$2,sqrt($3*$3+$6*$6),atan2($6,$3)*180./3.14159265,sqrt($3*$3+$6*$6)/'${maxsize}'*0.3}' > $XYZFILE
  #awk '$1>180 {print $1-360,$2,$3,$4,$5} $1<=180 {print $1,$2,$3,$4,$5}' $XYZFILE > ${XYZFILE}.tmp; mv ${XYZFILE}.tmp $XYZFILE
  paste s1 s2 | scale_vec > $XYZFILE
  awk '$1>180 {print $1-360,$2,$3,$4} $1<=180 {print $1,$2,$3,$4}' $XYZFILE > ${XYZFILE}.tmp; mv ${XYZFILE}.tmp $XYZFILE
fi

echo -n $XYZFILE; minmax $XYZFILE

if test -n "$bg"; then
  if [ ${bg_is_curvilinear} -ne 0 ]; then
    py_getxyz_curv ${bg}:${var} 0 > pepe.bg
  else
    py_getxyz ${bg}:${var} 0 > pepe.bg
  fi
fi
awk '$1>180 {print $1-360,$2,$3,$4,$5} $1<=180 {print $1,$2,$3,$4,$5}' pepe.bg > pepe.bg.tmp; mv pepe.bg.tmp pepe.bg

cres="-Dl"

if test "$zmin" != "$zmax"; then
  makecpt -C$cptfile -T${zmin}/$zmax/$dz -Z > pepe.cpt
  cptfile=pepe.cpt
fi

if test -n "$cptreverse"; then
  makecpt -C$cptfile -I > pepe2.cpt; mv pepe2.cpt pepe.cpt
  cptfile=pepe.cpt
fi

psxy /dev/null $RJFLAG -K > $FNAMEOUT
#pscoast $RJFLAG -Gc -A0/0/1 ${cres} -O -K >> $FNAMEOUT
test -n "$bg" && psxy pepe.bg $RJFLAG -C${cptfile} -Ss0.5c -O -K >> $FNAMEOUT
psxy ${XYZFILE} $RJFLAG -Sv0.015c/0.07c/0.05 -G0 -O -K >> $FNAMEOUT
echo "55 -40 1.5 0.8" | psxy $RJFLAG -Sr -G255 -O -K >> $FNAMEOUT
echo "55 -40 10 0 0 TC $maxsize" | pstext $RJFLAG -O -K >> $FNAMEOUT
echo "55 -39.5 $maxsize 55 40 0" | scale_vec \
  | psxy $RJFLAG -Svb0.015c/0.07c/0.05 -G0 -O -K >> $FNAMEOUT
#pscoast $RJFLAG -Q -O -K >> $FNAMEOUT
pscoast $RJFLAG $BFLAG -A0/0/1 ${cres} -W5 -O -K >> $FNAMEOUT
pstext $RJFLAG << EOF >> $FNAMEOUT
-20 -40 40 0 0 LB ${title}
EOF
psxy /dev/null $RJFLAG -O >> $FNAMEOUT

psscale -D8c/8c/14c/1ch -C${cptfile} -E -L > ${FNAMEOUT/.eps/.scale.eps}
fixbb ${FNAMEOUT/.eps/.scale.eps} tmp.eps
mv tmp.eps ${FNAMEOUT/.eps/.scale.eps}

# Convert to JPG and drop the eps.
ps2raster -A -Tg $FNAMEOUT 
#mogrify -crop 900x660+286+1308 ${fpng}
#mogrify -fill white -draw 'rectangle 600,500 900,660' ${fpng}
rm -f $FNAMEOUT

rm -f ${XYZFILE} .gmt*
rm -f pepe.cpt
