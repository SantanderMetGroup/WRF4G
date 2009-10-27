#! /bin/bash
thisdir=$(pwd)
scriptdir=$(dirname $0)
basedir=$(dirname $(dirname $0))
source ${scriptdir}/dirs
source env.${HOSTNAME//.*/}

do_tetis_post=0    # Only 1 in tetis!
do_climatol_wrf=0  # Only 1 in tetis!
do_climatol_sp02=0 # Only 1 in oceano
do_climatol_eobs=0 # Only 1 in oceano
do_climatol_erai=0 
do_plots=1

read yeari yearf <<< ${pername//_/ }
#
#  Post-processing in tetis
#
if test ${do_tetis_post} -ne 0; then
  cd ${BIGDIR} || exit
    bash ${scriptdir}/post1.in_tetis.sh
    # Clean manually possible spinup duplicated months in $POSTDIR before running post2
    # Be sure that ${year}* refers to exacly the 12 monthly files in correct order
    bash ${scriptdir}/post2.in_tetis.sh
    # Not used. the whole period is concatenated down there...
    #bash ${scriptdir}/post3.in_tetis.sh
  cd ${scriptdir}
fi
#
#  WRF climatologies
#
if test ${do_climatol_wrf} -ne 0; then
  mkdir -p ${BIGDIR}/${expname}
  bname="${BIGDIR}/${expname}/${expname}__${pername}"
  for var in tasmax tasmin; do
    # TODO: we need to avoid some day this VERY UNSAFE method to concatenate the files!
    ncrcat -O ${POST2DIR}/${expname}__????_DM__${var}.nc tmp.nc
    cdo -settaxis,${yeari}-01-01,00:00,1day tmp.nc ${bname}_DM__${var}.nc
  done
  # Build tas
  cdo chname,tasmax,tas -mulc,0.5 -add ${bname}_DM__tasmax.nc ${bname}_DM__tasmin.nc ${bname}_DM__tas.nc
  for var in pr; do
    # TODO: we need to avoid some day this VERY UNSAFE method to concatenate the files!
    ncrcat -O ${POST2DIR}/${expname}__????_DM__${var}.nc tmp.nc
    cdo settaxis,${yeari}-01-01,00:00,1day tmp.nc ${bname}_DM__${var}.nc
  done
  for var in tasmax tasmin tas pr; do
    cdo ymonmean  ${bname}_DM__${var}.nc ${bname}_mclim__${var}.nc
    cdo yseasmean ${bname}_DM__${var}.nc ${bname}_sclim__${var}.nc
    cdo timmean   ${bname}_DM__${var}.nc ${bname}_clim__${var}.nc
    cdo monmean   ${bname}_DM__${var}.nc ${bname}_MM__${var}.nc
    cdo ymonstd   ${bname}_MM__${var}.nc ${bname}_mstd__${var}.nc
  done
  rm tmp.nc
fi
#
#  Spain02 climatology
#
if test ${do_climatol_sp02} -ne 0; then
  #for var in tasmax tasmin; do
  #  cdo seldate,${yeari}-01-01,${yearf}-12-31 ${SP02DIR}/Spain02D_${var}.nc Spain02__${pername}_DM__${var}.nc
  #  cdo yseasmean Spain02D_${var}.${pername}.nc tmp.nc
  #    cdo addc,-273.15 tmp.nc Spain02__${pername}_sclim__${var}.nc
  #  cdo timmean Spain02__${pername}_sclim__${var}.nc Spain02__${pername}_clim__${var}.nc
  #done
  bname="${BIGDIR}/Spain02/Spain02__${pername}"
  for var in pr; do
    cdo -r setrtomiss,1e30,1e40 -seldate,${yeari}-01-01,${yearf}-12-31 ${SP02DIR}/Spain02D_${var}.nc ${bname}_DM__${var}.nc
    cdo ymonmean  ${bname}_DM__${var}.nc ${bname}_mclim__${var}.nc
    cdo yseasmean ${bname}_DM__${var}.nc ${bname}_sclim__${var}.nc
    cdo timmean   ${bname}_DM__${var}.nc ${bname}_clim__${var}.nc
    cdo monmean   ${bname}_DM__${var}.nc ${bname}_MM__${var}.nc
    cdo ymonstd   ${bname}_MM__${var}.nc ${bname}_mstd__${var}.nc
  done
fi
#
# EOBS Climatology
#
if test ${do_climatol_eobs} -ne 0; then
  bash do_clim_eobs.sh
fi
#
# ERAI Climatology
#
if test ${do_climatol_erai} -ne 0; then
  bash do_clim_erai.sh
fi
#
# Plots
#
if test ${do_plots} -ne 0; then
#  # Check the plotting bits of these 2:
#  bash do_plots.sh
#  bash do_bias_plots.sh
#  python monta_figs.py monta_figs.sclim
  for var in tasmax tasmin pr; do
    for region in $(seq 1 11); do
      bash do_mclim.sh $var $region
    done
  done
  python monta_figs.py monta_figs.mclim
fi

