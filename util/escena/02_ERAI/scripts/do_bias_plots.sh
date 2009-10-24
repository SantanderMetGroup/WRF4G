#! /bin/bash
thisdir=$(pwd)
scriptdir=$( (cd `dirname $0` && echo $PWD) )
basedir=$(dirname $scriptdir)
source ${scriptdir}/dirs
source env.${HOSTNAME//.*/}

function var_range(){
  case $1 in
    tasmax|tasmin|tas) echo "zmin -5 zmax 5 dz 1" ;;
    pr) echo "zmin -9 zmax 9 dz 1" ;;
  esac
}

#
# WRF_ERAI - Spain 02
#
bname="${BIGDIR}/${expname}__${pername}"
bnameref="${BIGDIR}/Spain02/Spain02__${pername}"
oname="${basedir}/figs/WRF_ERAI_SP02_bias__${pername}"; echo $oname
symbols="anglectl 0.12 sysize 0.22"
for var in pr; do
  cdo remapbil,${bnameref}_clim__${var}.nc ${bname}_clim__${var}.nc tmp.nc
  bash plot_escena_data.gmt.sh tmp.nc var $var cpt cpts/bias.cpt $(var_range $var) ${symbols} national_bound out ${oname}_clim__${var}.eps
  cdo remapbil,${bnameref}_sclim__${var}.nc ${bname}_sclim__${var}.nc tmp.nc
  irec=0
  for seas in DJF MAM JJA SON; do
    bash plot_escena_data.gmt.sh tmp.nc \
      out ${oname}_sclim${seas}__${var}.eps \
      var $var \
      rec $irec \
      label $seas \
      cpt cpts/bias.cpt \
      $symbols \
      $(var_range $var) \
      national_bound
    let irec++
  done
  rm tmp.nc
done
##
##  WRF 
##
#bname="${BIGDIR}/${expname}__${pername}"
#oname="${basedir}/figs/${expname}__${pername}"
#symbols="anglectl 0.32 sysize 0.33"
#for var in tasmax tasmin tas pr; do
#  bash plot_escena_data.gmt.sh ${bname}_clim__${var}.nc var $var cpt cpts/orig.cpt $(var_range $var) ${symbols} national_bound is_curvilinear out ${oname}_clim__${var}.eps
#  irec=0
#  for seas in DJF MAM JJA SON; do
#    bash plot_escena_data.gmt.sh ${bname}_sclim__${var}.nc \
#      out ${oname}_sclim${seas}__${var}.eps \
#      var $var \
#      rec $irec \
#      label $seas \
#      cpt cpts/orig.cpt \
#      $symbols \
#      $(var_range $var) \
#      national_bound is_curvilinear
#    let irec++
#  done
#done
##
## EOBS 025
##
#bname="${BIGDIR}/EOBS/EOBS025__${pername}"
#oname="${basedir}/figs/EOBS025__${pername}"
#symbols="anglectl 0.13 sysize 0.28"
#for var in tasmax tasmin tas; do
#  bash plot_escena_data.gmt.sh ${bname}_clim__${var}.nc var $var cpt cpts/orig.cpt $(var_range $var) ${symbols} national_bound out ${oname}_clim__${var}.eps
#  irec=0
#  for seas in DJF MAM JJA SON; do
#    bash plot_escena_data.gmt.sh ${bname}_sclim__${var}.nc \
#      out ${oname}_sclim${seas}__${var}.eps \
#      var $var \
#      rec $irec \
#      label $seas \
#      cpt cpts/orig.cpt \
#      $symbols \
#      $(var_range $var) \
#      national_bound
#    let irec++
#  done
#done
##
## ERA Interim
##
#bname="${BIGDIR}/ERAI/ERAI__${pername}"
#oname="${basedir}/figs/ERAI__${pername}"
#symbols="anglectl 0.13 sysize 0.28"
#for var in tasmax tasmin tas; do
#  bash plot_escena_data.gmt.sh ${bname}_clim__${var}.nc var $var cpt cpts/orig.cpt $(var_range $var) ${symbols} national_bound out ${oname}_clim__${var}.eps
#  irec=0
#  for seas in DJF MAM JJA SON; do
#    bash plot_escena_data.gmt.sh ${bname}_sclim__${var}.nc \
#      out ${oname}_sclim${seas}__${var}.eps \
#      var $var \
#      rec $irec \
#      label $seas \
#      cpt cpts/orig.cpt \
#      $symbols \
#      $(var_range $var) \
#      national_bound
#    let irec++
#  done
#done
 

