#! /bin/bash
thisdir=$(pwd)
scriptdir=$( (cd `dirname $0` && echo $PWD) )
basedir=$(dirname $scriptdir)
basedir="/home/chus/tareas/escena/02_ERAI" # override buggy auto-detection
source ${scriptdir}/dirs
source env.${HOSTNAME//.*/}

do_spain02=0
do_wrf=0
do_eobs=1
do_erai=0

function var_range(){
  case $1 in
    tas) echo "zmin 265 zmax 302 dz 1" ;;
    tasmax) echo "zmin 271 zmax 312 dz 1" ;;
    tasmin) echo "zmin 260 zmax 296 dz 1" ;;
    pr) echo "zmin 0 zmax 9 dz 1" ;;
  esac
}

#
# Spain 02
#
if test ${do_spain02} -ne 0; then
  bname="${BIGDIR}/Spain02/Spain02__${pername}"
  oname="${basedir}/figs/Spain02__${pername}"; echo $oname
  symbols="anglectl 0.12 sysize 0.22"
  for var in pr; do
    bash plot_escena_data.gmt.sh ${bname}_clim__${var}.nc var $var cpt cpts/orig.cpt $(var_range $var) ${symbols} national_bound out ${oname}_clim__${var}.eps
    irec=0
    for seas in DJF MAM JJA SON; do
      bash plot_escena_data.gmt.sh ${bname}_sclim__${var}.nc \
        out ${oname}_sclim${seas}__${var}.eps \
        var $var \
        rec $irec \
        label $seas \
        cpt cpts/orig.cpt \
        $symbols \
        $(var_range $var) \
        national_bound
      let irec++
    done
  done
fi
#
#  WRF 
#
if test ${do_wrf} -ne 0; then
  bname="${BIGDIR}/${expname}/${expname}__${pername}"
  oname="${basedir}/figs/${expname}__${pername}"
  symbols="anglectl 0.32 sysize 0.33"
  for var in tasmax tasmin pr # tas
  do
    bash plot_escena_data.gmt.sh ${bname}_clim__${var}.nc var $var cpt cpts/orig.cpt $(var_range $var) ${symbols} national_bound is_curvilinear out ${oname}_clim__${var}.eps
    irec=0
    for seas in DJF MAM JJA SON; do
      bash plot_escena_data.gmt.sh ${bname}_sclim__${var}.nc \
        out ${oname}_sclim${seas}__${var}.eps \
        var $var \
        rec $irec \
        label $seas \
        cpt cpts/orig.cpt \
        $symbols \
        $(var_range $var) \
        national_bound is_curvilinear
      let irec++
    done
  done
fi
#
# EOBS 025
#
if test ${do_eobs} -ne 0; then
  bname="${BIGDIR}/EOBS/EOBS025__${pername}"
  oname="${basedir}/figs/EOBS025__${pername}"
  symbols="anglectl 0.13 sysize 0.28"
#  for var in tasmax tasmin tas pr; do
  for var in pr; do
    bash plot_escena_data.gmt.sh ${bname}_clim__${var}.nc var $var cpt cpts/orig.cpt $(var_range $var) ${symbols} national_bound out ${oname}_clim__${var}.eps
    irec=0
    for seas in DJF MAM JJA SON; do
      bash plot_escena_data.gmt.sh ${bname}_sclim__${var}.nc \
        out ${oname}_sclim${seas}__${var}.eps \
        var $var \
        rec $irec \
        label $seas \
        cpt cpts/orig.cpt \
        $symbols \
        $(var_range $var) \
        national_bound
      let irec++
    done
  done
fi
#
# ERA Interim
#
if test ${do_erai} -ne 0; then
  bname="${BIGDIR}/ERAI/ERAI__${pername}"
  oname="${basedir}/figs/ERAI__${pername}"
  symbols="anglectl 0.12 sysize 0.8"
  for var in tasmax tasmin # tas
  do
    bash plot_escena_data.gmt.sh ${bname}_clim__${var}.nc var $var cpt cpts/orig.cpt $(var_range $var) ${symbols} national_bound out ${oname}_clim__${var}.eps
    irec=0
    for seas in DJF MAM JJA SON; do
      bash plot_escena_data.gmt.sh ${bname}_sclim__${var}.nc \
        out ${oname}_sclim${seas}__${var}.eps \
        var $var \
        rec $irec \
        label $seas \
        cpt cpts/orig.cpt \
        $symbols \
        $(var_range $var) \
        national_bound
      let irec++
    done
  done
fi 

