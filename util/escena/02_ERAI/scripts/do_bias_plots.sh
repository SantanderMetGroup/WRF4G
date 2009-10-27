#! /bin/bash
thisdir=$(pwd)
scriptdir=$( (cd `dirname $0` && echo $PWD) )
basedir=$(dirname $scriptdir)
basedir="/home/chus/tareas/escena/02_ERAI" # override buggy auto-detection
source ${scriptdir}/dirs
source env.${HOSTNAME//.*/}

do_wrf_spain02=0
do_wrf_eobs=0
do_wrf_erai=1

function var_range(){
  case $1 in
    tasmax|tasmin|tas) echo "cpt cpts/bias_tas.cpt" ;;
    pr) echo "cpt cpts/bias.cpt" ;;
  esac
}

#
# WRF_ERAI - Spain 02
#
if test ${do_wrf_spain02} -ne 0; then
  bname="${BIGDIR}/${expname}/${expname}__${pername}"
  bnameref="${BIGDIR}/Spain02/Spain02__${pername}"
  oname="${basedir}/figs/WRF_ERAI_SP02_bias__${pername}"; echo $oname
  symbols="anglectl 0.12 sysize 0.22"
  for var in pr; do
    cdo remapbil,${bnameref}_clim__${var}.nc ${bname}_clim__${var}.nc tmp2.nc
    cdo sub tmp2.nc ${bnameref}_clim__${var}.nc tmp.nc
    bash plot_escena_data.gmt.sh tmp.nc var $var $(var_range $var) ${symbols} national_bound out ${oname}_clim__${var}.eps
    cdo remapbil,${bnameref}_sclim__${var}.nc ${bname}_sclim__${var}.nc tmp2.nc
    cdo sub tmp2.nc ${bnameref}_sclim__${var}.nc tmp.nc
    irec=0
    for seas in DJF MAM JJA SON; do
      bash plot_escena_data.gmt.sh tmp.nc \
        out ${oname}_sclim${seas}__${var}.eps \
        var $var \
        rec $irec \
        label $seas \
        $symbols \
        $(var_range $var) \
        national_bound
      let irec++
    done
    rm tmp.nc
  done
fi
#
# WRF_ERAI - EOBS
#
if test ${do_wrf_eobs} -ne 0; then
  bname="${BIGDIR}/${expname}/${expname}__${pername}"
  bnameref="${BIGDIR}/EOBS/EOBS025__${pername}"
  oname="${basedir}/figs/WRF_ERAI_EOBS_bias__${pername}"; echo $oname
  symbols="anglectl 0.13 sysize 0.28"
  for var in tasmax tasmin pr; do
    cdo remapbil,${bnameref}_clim__${var}.nc ${bname}_clim__${var}.nc tmp2.nc
    cdo sub tmp2.nc ${bnameref}_clim__${var}.nc tmp.nc
    bash plot_escena_data.gmt.sh tmp.nc var $var $(var_range $var) ${symbols} national_bound out ${oname}_clim__${var}.eps
    cdo remapbil,${bnameref}_sclim__${var}.nc ${bname}_sclim__${var}.nc tmp2.nc
    cdo sub tmp2.nc ${bnameref}_sclim__${var}.nc tmp.nc
    irec=0
    for seas in DJF MAM JJA SON; do
      bash plot_escena_data.gmt.sh tmp.nc \
        out ${oname}_sclim${seas}__${var}.eps \
        var $var \
        rec $irec \
        label $seas \
        $symbols \
        $(var_range $var) \
        national_bound
      let irec++
    done
    rm tmp.nc
  done
fi
#
# WRF_ERAI - ERAI
#
if test ${do_wrf_erai} -ne 0; then
  bname="${BIGDIR}/${expname}/${expname}__${pername}"
  bnameref="${BIGDIR}/ERAI/ERAI__${pername}"
  oname="${basedir}/figs/WRF_ERAI_ERAI_bias__${pername}"; echo $oname
  symbols="anglectl 0.12 sysize 0.8"
  for var in tasmax tasmin; do
    cdo remapbil,${scriptdir}/grid.erai ${bname}_clim__${var}.nc tmp2.nc
    cdo sub tmp2.nc ${bnameref}_clim__${var}.nc tmp.nc
    bash plot_escena_data.gmt.sh tmp.nc var $var $(var_range $var) ${symbols} national_bound out ${oname}_clim__${var}.eps
    cdo remapbil,${scriptdir}/grid.erai ${bname}_sclim__${var}.nc tmp2.nc
    cdo sub tmp2.nc ${bnameref}_sclim__${var}.nc tmp.nc
    irec=0
    for seas in DJF MAM JJA SON; do
      bash plot_escena_data.gmt.sh tmp.nc \
        out ${oname}_sclim${seas}__${var}.eps \
        var $var \
        rec $irec \
        label $seas \
        $symbols \
        $(var_range $var) \
        national_bound
      let irec++
    done
    rm tmp.nc
  done
fi
