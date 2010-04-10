source /home/usr/etc/bashrc

RFLAG="-R-25/61/-44/46"
NEARNREG="-R-13/8/33/46"
RJFLAG="${RFLAG} -JM18c"
BFLAG="-Bf5a10/f5a10WeSn"
NCFILE=$1
var=$(basename ${NCFILE//*_/} .nc)

shift
has_height_dim=0
is_curvilinear=0
national_bound=0
no_nn=0
rec=0
zmin=0
zmax=0
dz=0
only_land=0
out=""
while test "$*"
do
  case $1 in
    has_height_dim) has_height_dim=1;;
    is_curvilinear) is_curvilinear=1;;
    national_bound) national_bound=1;;
    only_land) only_land=1;;
    no_nn) no_nn=1;;
    label) label=$2; shift;;
    title) title=$2; shift;;
    addc) addc=$2; shift;;
    cpt) cptfile=$2; shift;;
    var) var=$2; shift;;
    rec) rec=$2; shift;;
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
    if ${has_height_dim}:
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
for i in range(len(lons)):
  for j in range(len(lats)):
    if ${has_height_dim}:
      print "%9.4f %9.4f %.5e" % (lons[i], lats[j], var[irec,0,j,i])
    else:
      print "%9.4f %9.4f %.5e" % (lons[i], lats[j], var[irec,j,i])
End_Of_Python
}

gmtset PAPER_MEDIA a4+
gmtset PLOT_DEGREE_FORMAT dddF
gmtset PAGE_ORIENTATION portrait

if [ ${no_nn} -eq 0 ]; then
  if [ ${is_curvilinear} -ne 0 ]; then
    py_getxyz_curv $NCFILE:${var} ${rec} > $XYZFILE
  else
    py_getxyz $NCFILE:${var} ${rec} > $XYZFILE
  fi
  awk '$1>180 {print $1-360,$2,$3} $1<=180 {print $1,$2,$3}' $XYZFILE > ${XYZFILE}.tmp; mv ${XYZFILE}.tmp $XYZFILE
fi

if test -n "${addc}"; then
  awk '{print $1,$2,'${addc}'+$3}' $XYZFILE > ${XYZFILE}.tmp; mv ${XYZFILE}.tmp $XYZFILE
fi
if test -n "${validrange}"; then
  read lim1 lim2 <<< ${validrange/,/ }
  awk '$3 <= '${lim2}' && $3 >= '${lim1}'{print $1,$2,$3}' $XYZFILE > ${XYZFILE}.tmp; mv ${XYZFILE}.tmp $XYZFILE
fi

echo -n $XYZFILE; minmax $XYZFILE

cres="-Dl"

function pysymbol(){
  python << end_of_py
print "%.2f %.2f M" % (-0.5+${anglectl},            -0.50)
print "%.2f %.2f D" % (            0.50, -0.5+${anglectl})
print "%.2f %.2f D" % ( 0.5-${anglectl},             0.50)
print "%.2f %.2f D" % (           -0.50,  0.5-${anglectl})
print "%.2f %.2f D" % (-0.5+${anglectl},            -0.50)
end_of_py
}
function pysymbolsq(){
  python << end_of_py
print "%.2f %.2f M" % (-0.5+${anglectl},           -0.50)
print "%.2f %.2f D" % ( 0.5-${anglectl},            -0.5)
print "%.2f %.2f D" % ( 0.5-${anglectl},             0.5)
print "%.2f %.2f D" % (-0.5+${anglectl},            0.50)
print "%.2f %.2f D" % (-0.5+${anglectl},           -0.50)
end_of_py
}

if test $zmin -ne $zmax; then
  makecpt -C$cptfile -T${zmin}/$zmax/$dz -Z > pepe.cpt
  cptfile=pepe.cpt
fi

psxy /dev/null $RJFLAG -K > $FNAMEOUT
test "$only_land" -eq 1 && pscoast $RJFLAG -Gc -A0/0/1 ${cres} -O -K >> $FNAMEOUT
  if [ ${is_curvilinear} -ne 0 ]; then
    anglectl=0.
    pysymbol > tile.def
    psxy ${XYZFILE} $RJFLAG -C${cptfile} -Sktile/0.1c -O -K >> $FNAMEOUT
  else
    anglectl=0.13
    pysymbolsq > tile.def
    psxy ${XYZFILE} $RJFLAG -C${cptfile} -Sktile/0.22c -O -K >> $FNAMEOUT
  fi
  if [ ${national_bound} -eq 1 ]; then
    pscoast $RJFLAG -A0/0/1 ${cres} -N1/5,white -O -K >> $FNAMEOUT
  fi
test "$only_land" -eq 1 && pscoast $RJFLAG -Q -O -K >> $FNAMEOUT
pscoast $RJFLAG $BFLAG -A0/0/1 ${cres} -W5 -O -K >> $FNAMEOUT
test -n "$title" && pstext $RJFLAG << EOF >> $FNAMEOUT
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

rm -f ${XYZFILE} tile.def .gmt*
rm -f pepe.cpt
