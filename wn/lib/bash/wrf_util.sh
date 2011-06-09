# wrf_util.sh
#
# Version: 0.0
#   o Risky business!
#
# Copyright Jesus Fernandez, 2007 (jesusff in_the_domain gmail com)
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

function py_wrf2xyz(){
  point_type=$1
  wrfnc=$2
  wrfvar=$3
  record=$4
  level=$5
  if [ $point_type = "_M" ]; then point_type=""; fi
  python << End_of_Python
from Scientific.IO.NetCDF import NetCDFFile
nc = NetCDFFile("$wrfnc", "r")
latsm = nc.variables["XLAT${point_type}"][0]
lonsm = nc.variables["XLONG${point_type}"][0]
ncvar = nc.variables["$wrfvar"]
if "$level":
  for iy   in range(latsm.shape[0]):
    for ix in range(latsm.shape[1]):
      print "%9.4f %9.4f %f" % (lonsm[iy,ix], latsm[iy,ix], ncvar[$record,$level,iy,ix])
else:
  for iy   in range(latsm.shape[0]):
    for ix in range(latsm.shape[1]):
      print "%9.4f %9.4f %f" % (lonsm[iy,ix], latsm[iy,ix], ncvar[$record,iy,ix])
End_of_Python
}

function py_wrf2xyzlcc(){
  point_type=$1
  wrfnc=$2
  wrfvar=$3
  record=$4
  level=$5
  pwe=${6:-nx}
  psn=${7:-ny}
  if [ $point_type = "_M" ]; then point_type=""; fi
  python << End_of_Python
from Scientific.IO.NetCDF import NetCDFFile
import sys
nc = NetCDFFile("$wrfnc", "r")
latsm = nc.variables["XLAT${point_type}"][0]
lonsm = nc.variables["XLONG${point_type}"][0]
ncvar = nc.variables["$wrfvar"]
dx = nc.DX[0][0]
pdx = nc.PARENT_GRID_RATIO[0] * dx
ips = nc.I_PARENT_START[0] or 1
jps = nc.J_PARENT_START[0] or 1
is_not_the_first = ips!=1 
xoff = (ips-1) * pdx
yoff = (jps-1) * pdx
nx = latsm.shape[1]
ny = latsm.shape[0]
x0 = xoff - pdx * (${pwe}-1)/2. - dx * is_not_the_first
y0 = yoff - pdx * (${psn}-1)/2. - dx * is_not_the_first
print >>sys.stderr, nx,ny, dx, xoff , nc.PARENT_GRID_RATIO[0], x0
if $level >= 0:
  for iy in range(ny):
    for ix in range(nx):
      print "%9.4f %9.4f %f" % (
        x0 + dx*ix,
        y0 + dx*iy,
        ncvar[$record,$level,iy,ix]
      )
else:
  for iy in range(ny):
    for ix in range(nx):
      print "%9.4f %9.4f %f" % (
        x0 + dx*ix,
        y0 + dx*iy,
        ncvar[$record,iy,ix]
      )
End_of_Python
}

function py_getborder(){
  point_type=$1
  wrfnc=$2
  depth=$3
  if [ $point_type = "_M" ]; then point_type=""; fi
  python << End_of_Python 
from Scientific.IO.NetCDF import NetCDFFile
from sys import argv
nc = NetCDFFile("${wrfnc}", "r")
latsm = nc.variables["XLAT${point_type}"][0]
lonsm = nc.variables["XLONG${point_type}"][0]
ix = ${depth}
for iy in range(${depth},latsm.shape[0]-${depth}):
  print "%9.4f %9.4f" % (lonsm[iy,ix], latsm[iy,ix])
iy = len(latsm)-1-${depth}
for ix in range(${depth},latsm.shape[1]-${depth}):
  print "%9.4f %9.4f" % (lonsm[iy,ix], latsm[iy,ix])
ix = len(latsm[0])-1-${depth}
for iy in range(latsm.shape[0]-1-${depth},${depth},-1):
  print "%9.4f %9.4f" % (lonsm[iy,ix], latsm[iy,ix])
iy = ${depth}
for ix in range(latsm.shape[1]-1-${depth},${depth},-1):
  print "%9.4f %9.4f" % (lonsm[iy,ix], latsm[iy,ix])
End_of_Python
}

function py_wrfidx2xyz(){
  # Returns only the points provided as fortran indices ilon,ilat in an idxfile
  # provided as 5th argument
  point_type=$1
  wrfnc=$2
  wrfvar=$3
  record=$4
  idxfile=$5
  python << End_of_Python
from Scientific.IO.NetCDF import NetCDFFile
from pyclimate.readdat import readcol
nc = NetCDFFile("$wrfnc", "r")
latsm = nc.variables["XLAT${point_type}"][0]
lonsm = nc.variables["XLONG${point_type}"][0]
ncvar = nc.variables["$wrfvar"]
ixs = readcol("$idxfile", 1, 'i') - 1 # Fortran to python indices
iys = readcol("$idxfile", 2, 'i') - 1
for isite in range(len(ixs)):
  ix = ixs[isite]
  iy = iys[isite]
  print "%9.4f %9.4f %f" % (lonsm[iy,ix], latsm[iy,ix], ncvar[$record,iy,ix])
End_of_Python
}

function py_ncGlobalAttrs(){
  wrfnc=$1
  python << End_of_Python
from Scientific.IO.NetCDF import NetCDFFile
from Numeric import array
nc = NetCDFFile("$wrfnc", "r")
attrs = nc.__dict__.keys()
attrs.sort()
for attr in attrs:
  val = getattr(nc, attr)
  printstr = "%s=" % (attr.replace('-','_'))
  if type(val) == type(array([0])):
    if len(val)>1: printstr += '"'
    for xval in val:
      printstr += "%s " % xval
    printstr = printstr[:-1]
    if len(val)>1: printstr += '"'
  elif type(val) == type("hello"):
    printstr += '"%s"' % val
  else:
    printstr += val
  print printstr
End_of_Python
}

function py_getR(){
  ifile=$1
  bufferfract=$2
  python << End_of_Python
llon, hlon, llat, hlat = $(minmax -C $ifile | awk '{printf "%f,%f,%f,%f", $1,$2,$3,$4}')
bf=$bufferfract
latdim=hlat-llat
londim=hlon-llon
print "-R%f/%f/%f/%f" % (
  llon - bf*londim,
  hlon + bf*londim,
  llat - bf*latdim,
  hlat + bf*latdim
)
End_of_Python
}

function py_getRlcc(){
  wrfnc=$1
  python << End_of_Python
from Scientific.IO.NetCDF import NetCDFFile
from Numeric import array
nc = NetCDFFile("$wrfnc", "r")
latsm = nc.variables["XLAT"][0]
lonsm = nc.variables["XLONG"][0]
dx = nc.DX[0][0]
nx = latsm.shape[1]
ny = latsm.shape[0]
print "-R%f/%f/%f/%f" % (
  -(nx-1)/2.*dx,
  (nx-1)/2.*dx,
  -(ny-1)/2.*dx,
  (ny-1)/2.*dx,
)
End_of_Python
}

function py_sort_mlabels(){
  #multiparams_labels="1/5/311/355"
  #multiparams_labels="normal/burrada/prueba"
  
  mlabels=$1
  python << End_of_Python
ml="$mlabels"
ml=ml.replace(' ','')
ml=ml.replace(',','_')
arr=ml.split('/')
#arr.sort()
print "/".join(arr)
End_of_Python
}

function py_getRcorners(){
  wrfnc=$1
  python << End_of_Python
from Scientific.IO.NetCDF import NetCDFFile
nc = NetCDFFile("$wrfnc", "r")
latsm = nc.variables["XLAT"][0]
lonsm = nc.variables["XLONG"][0]
print "-R%f/%f/%f/%fr" % (lonsm[0,0], latsm[0,0], lonsm[-1,-1], latsm[-1,-1])
End_of_Python
}

function py_getD(){
  ifile=$1
  python << End_of_Python
londim, latdim = $(minmax -C $ifile | awk '{printf "%f,%f", $2-$1,$4-$3}')
dim=min(latdim,londim)
if   dim < 1:
  print "-Df"
elif 5 > dim >= 1:
  print "-Dh"
elif 30 > dim >= 5:
  print "-Di"
elif 50 > dim >= 30:
  print "-Dl"
else:
  print "-Dc"
End_of_Python
}

function gmt_clean(){
  rm -f .gmtdefaults* .gmtcommands*  
}

function clean_rsl(){
  rm -f rsl.error.* rsl.out.*
}

function clean_wps(){
  rm -f GRIBFILE.[A-Z][A-Z][A-Z]      # Links to grib files
  rm -f ${global_name}*\:????-??-??_??   # Intermediate files
  rm -f ${global_name}FIX                # Intermediate files
  rm -f met_em.*                      # metgrid files
}

function clean_real(){
  rm -f met_em.d??.????-??-??_??:00:00.nc
  rm -f namelist.wps
}

function fortnml_import_record(){
  nmlfile=$1
  recname=$2
  cat $nmlfile \
    | awk '/\// {if (a==1) b=1} {if (a==1 && b!=1) print $0} /&'${recname}'/ {a=1} ' \
    | sed -e 's/^ //' \
    | tr -d ' ' \
    | sed -e 's/,\ *$//'
}

function get_num_metgrid_levels(){
  ncdump -h $(\ls -1 met_em*.nc | head -1) \
    | grep 'num_metgrid_levels =' \
    | sed -e 's/^\t//' | tr '=;' '  ' | awk '{print $2}'
}

function get_num_metgrid_soil_levels(){
  ncdump -h $(\ls -1 met_em*.nc | head -1) \
    | grep 'num_st_layers =' \
    | sed -e 's/^\t//' | tr '=;' '  ' | awk '{print $2}'
}

function get_last_frame_date(){
  wrfncfile=$1
  ncdump -v Times ${wrfncfile} \
    | tail -2 \
    | head -1 \
    | tr -cd '[0-9]'
}

function fortnml_varcopy(){
  nmlfile1=$1
  nmlfile2=$2
  param1=$3
  param2=${4:-${param1}}
  value=$(grep "^\ *${param1}\ *=" ${nmlfile1} | awk -F= '{print $2}')
  echo "Updating parameter ${param2} in file: ${nmlfile2} to: $value"
  cat << __EOF > sedcmd
    s/^\ *${param2}\ *=.*$/  ${param2} = ${value}/g
__EOF
  sed -i -f sedcmd ${nmlfile2}
  rm -f sedcmd
}

function fortnml_vardel(){
  nmlfile=$1
  param=$2
  grep -v '\ *'${param}'\ *=' ${nmlfile} > ${nmlfile}.tmp
  mv ${nmlfile}.tmp ${nmlfile}
}

function fortnml_set(){
  nmlfile=$1
  param=$2
  value=$3
  echo "Updating parameter $param in file: $nmlfile"
  sed -i -e 's/^\ *'${param}'\ *=.*$/  '${param}' = '${value}',/g' $nmlfile 
}

function fortnml_setn(){
  nmlfile=$1
  param=$2
  nitems=$3
  value=$4
  echo "Updating parameter $param in file: $nmlfile"
  fval=""
  for item in `seq $nitems`; do
    fval="$fval ${value}, "
  done
  fval="  ${param} = ${fval}"
  cat << __EOF > sedcmd
    s/^\ *${param}\ *=.*$/${fval}/g
__EOF
  sed -i -f sedcmd $nmlfile
  rm -f sedcmd
}

function fortnml_setm(){
  nmlfile=$1
  param=$2
  echo "Updating parameter $param in file: $nmlfile"
  shift 2
  fval=""
  for item in $*; do
    fval="$fval ${item}, "
  done
  fval="  ${param} = ${fval}"
  cat << __EOF > sedcmd
    s/^\ *${param}\ *=.*$/${fval}/g
__EOF
  sed -i -f sedcmd $nmlfile
  rm -f sedcmd
}

function tuple_item(){
  echo "$1" | awk -F, '{print $'$2'}'
}

function get_timestep(){
  deltax=$1
  factor=${2:-6} # defaults to 6 times dx in km
  if test "${factor:0:6}" = "manual"; then
    read text value <<< ${factor//:/ }
    echo ${value}
  else
    HOUR_DIVISORS="1 2 3 4 5 6 8 9 10 12 15 16 18 20 24 25 30 36 40 45 48 50 60 72 75 80 90 100 120 144 150 180 200 225 240 300 360 400 450 600 720 900 1200"
    tstep=$(echo "${factor}*${deltax}/1000" | bc)
    for hd in ${HOUR_DIVISORS}; do
      if test ${hd} -le ${tstep}; then
        time_step=$hd
      else
        break
      fi
    done
    echo "${time_step}"
  fi
}

function get_ptop(){
  ncdump -v PRES $(ls met_em.d??.????-??-??_??:00:00.nc | head -1) \
    | tail -2 | head -1 \
    | tr -d ',;' \
    | awk '{printf "%d", $1}'
}

function fix_ptop(){
  toppress=$(get_ptop)
  if grep p_top_requested namelist.input >& /dev/null
  then
    specptop=$(grep p_top_requested namelist.input | awk -F= '{printf "%d", $2}')
    if [ $specptop -lt $toppress ]; then
      echo "Specified p_top is ${specptop}, but your input files only reach $toppress. Fixing..."
      fortnml_set namelist.input p_top_requested ${toppress}
    fi
  else
    if [ 5000 -lt $toppress ]; then
      echo "Default p_top is 5000., but your input files only reach $toppress. Fixing..."
      sed -e 's/\(^\ *num_metgrid_levels.*$\)/\1\n p_top_requested  = '${toppress}'/' namelist.input > namelist.input.fixptop
      mv namelist.input.fixptop namelist.input
    fi
  fi
}

function get_yearmons(){
  yeari=$1
  moni=$2
  yearf=$3
  monf=$4
  yearmoni="$yeari$(echo $moni | awk '{printf "%02d",$1}')"
  yearmonf="$yearf$(echo $monf | awk '{printf "%02d",$1}')"
  for year in $(seq $yeari $yearf); do
    for month in $(seq 1 12); do
      thisyearmon="$year$(printf "%02d" $month)"
      if test $thisyearmon -ge $yearmoni -a $thisyearmon -le $yearmonf; then
        echo ${thisyearmon}
      fi
    done
  done
}

function get_yearmondays(){
  yeari=$1
  moni=$2
  dayi=$3
  yearf=$4
  monf=$5
  dayf=$6
  ymdi="$(echo $yeari $moni $dayi | awk '{printf "%4d%02d%02d",$1,$2,$3}')"
  ymdf="$(echo $yearf $monf $dayf | awk '{printf "%4d%02d%02d",$1,$2,$3}')"
  while test $ymdi -le $ymdf; do
    echo ${ymdi}
    ymdi=$(date --utc '+%Y%m%d' -d "${ymdi} 1 day")
  done  
}

function date_wrf2iso(){
  echo "${1}Z" | tr -d ':-' | tr '_' 'T'
}

function date_iso2wrf(){
  isodate=$1
  local yyyy mm dd hh min sec
  yyyy=${isodate:0:4}
  mm=${isodate:4:2}
  dd=${isodate:6:2}
  hh=${isodate:9:2}
  min=${isodate:11:2}
  sec=${isodate:13:2}
  echo "${yyyy}-${mm}-${dd}_${hh}:${min}:${sec}"
}

function date2int(){
  # Converts a wrf or iso date to integer for ease of comparison
  echo $1 | tr -d ' :_TZ-'
}

function datediff_s(){
  date1=$(date2int $1)
  date2=$(date2int $2)
  seconds1=$(echo 3600\*${date1:8:2}+60*${date1:10:2}+${date1:12:2} | bc -l)
  seconds2=$(echo 3600\*${date2:8:2}+60*${date2:10:2}+${date2:12:2} | bc -l)
  date1sec=$(date +%s -u -d "${date1:0:8} ${seconds1} seconds")
  date2sec=$(date +%s -u -d "${date2:0:8} ${seconds2} seconds")
  echo ${date1sec}-${date2sec} | bc -l
}

function wait_for_these_pids(){
  # waits for some pids to finish
  # This is more general than the standard wait commmand in the sense that
  # it waits for the pids even if they are not a child of the current process.
  # WARNING: what if the pid you are waiting for finished and the os assigned
  #          it to a different process? Bad luck. I dont see a way to overcome
  #          that situation.
  pids="$*"
  greppids=$(echo "($pids)" | sed -e 's/ /|/g')
  echo $greppids
  nrunning=1
  while true; do
    nrunning=$(ps aux | grep "^$USER" | awk '{print $2}' | grep -E "$greppids" | wc -l)
    echo "I'm waiting for $nrunning processes to finish"
    test $nrunning -eq 0 && break || sleep 30
  done
}
