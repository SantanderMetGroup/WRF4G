#! /bin/bash
thisdir=$(pwd)
scriptdir=$( (cd `dirname $0` && echo $PWD) )
basedir=$(dirname $scriptdir)
basedir="/home/chus/tareas/escena/02_ERAI" # override buggy auto-detection
source ${scriptdir}/dirs
source env.${HOSTNAME//.*/}

basinsfile=${BIGDIR}/Spain02/Spain02_basins.nc
var=$1
region=$2
FNAMEOUT="${basedir}/figs/WRF_ERAI__${pername}_mclimR${region}__${var}.eps"

function get_mclim_for_region() {
  region=$1
  ifile=$2
  cdo setctomiss,2 -setrtoc2,${region},${region},1,2 ${basinsfile} mask.nc 2> /dev/null #
  cdo remapbil,mask.nc ${ifile} tmp.nc                                     2> /dev/null #
  cdo mul tmp.nc mask.nc tmp2.nc                                           2> /dev/null #
  cdo output -fldmean tmp2.nc                                              2> /dev/null #
  rm -f tmp.nc tmp2.nc
}

wrffile="${BIGDIR}/WRF_ERAI/WRF_ERAI__1990_2008_mclim__${var}.nc"
case $var in
  pr)
    yrange="0/6"
    reffile="${BIGDIR}/Spain02/Spain02__${pername}_mclim__${var}.nc"; ref1lab="Spain02"
    reffile2="${BIGDIR}/EOBS/EOBS025__1990_2008_mclim__${var}.nc"; ref2lab="EOBS"
    ;;
  tas*)
    yrange="270/310"
    reffile="${BIGDIR}/EOBS/EOBS025__1990_2008_mclim__${var}.nc"; ref1lab="EOBS"
    reffile2="${BIGDIR}/ERAI/ERAI__1990_2008_mclim__${var}.nc"; ref2lab="ERA-Interim"
    ;;
esac

#
# GMT
#
RFLAG="-R0.5/12.5/${yrange}"
RJFLAG="${RFLAG} -JX13c"
RJov="-R0/1/0/1 -JX13c"
BFLAG="-Bf1a1/f1a2WeSn"
OK="-O -K"

xpos=0.05; xlen=0.05; xspace=0.02
ypos=0.95; dy=0.05

function legend(){
  penst=$1
  label=$2
  psxy $RJov -N ${penst} $OK >> $FNAMEOUT << EOF
        $xpos                          $ypos
        $(echo "$xpos + $xlen" | bc)   $ypos
EOF
  pstext $RJov -N $OK >> $FNAMEOUT << EOF
        $(echo "$xpos + $xlen + $xspace" | bc)   $ypos 10 0 0 ML $label
EOF
  ypos=$(echo "$ypos - $dy" | bc)
}

function title(){
  label=$1
  txpos=0.5
  typos=0.95
  pstext $RJov -N $OK >> $FNAMEOUT << EOF
    $txpos $typos 10 0 0 ML $label
EOF
}

gmtset PAPER_MEDIA a4+
gmtset PLOT_DEGREE_FORMAT dddF
gmtset PAGE_ORIENTATION portrait

psbasemap $RJFLAG $BFLAG -K > $FNAMEOUT
title "$var (region: $region)"
get_mclim_for_region ${region} ${wrffile} \
  | awk '{print NR, $1}' \
  | psxy $RJFLAG -W5 -O -K >> $FNAMEOUT
legend "-W5" "WRF_ERAI"
get_mclim_for_region ${region} ${reffile} \
  | awk '{print NR, $1}' \
  | psxy $RJFLAG -W3,255/0/0 -O -K >> $FNAMEOUT
legend "-W3,255/0/0" "${ref1lab}"
get_mclim_for_region ${region} ${reffile2} \
  | awk '{print NR, $1}' \
  | psxy $RJFLAG -W3,0/0/255 -O -K >> $FNAMEOUT
legend "-W3,0/0/255" "${ref2lab}"

psxy /dev/null $RJFLAG -O >> $FNAMEOUT

echo "$FNAMEOUT generated."
