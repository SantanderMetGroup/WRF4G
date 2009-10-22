#! /bin/bash
thisdir=$(pwd)
basedir=$(dirname $0)
source ${basedir}/dirs

do_tetis_post=0
do_climatol_wrf=0
do_climatol_sp02=0
do_climatol_eobs=0
do_climatol_erai=0
do_plots=0
#
#  Post-processing in tetis
#
if test ${do_tetis_post} -ne 0; then
  cd ${BIGDIR} || exit
    bash ${basedir}/post1.in_tetis.sh
    bash ${basedir}/post2.in_tetis.sh
    bash ${basedir}/post3.in_tetis.sh
  cd ${basedir}
fi
#
#  WRF climatologies
#
if test ${do_climatol_wrf} -ne 0; then
  for var in tasmax tasmin; do
    cdo addc,-273.15 tmp.nc ${expname}_${pername}.${var}_sclim.nc
  done
  for var in pr; do
    cdo setrtomiss,1e30,1e40 ${expname}_${pername}.${var}.nc ${expname}_${par}_${pername}.${var}_clim.nc
  done
  for var in tasmax tasmin pr; do
    cdo ymonmean  ${expname}__${pername}_DM__${var}.nc ${expname}__${pername}_mclim__${var}.nc
    cdo yseasmean ${expname}__${pername}_DM__${var}.nc ${expname}__${pername}_sclim__${var}.nc
    cdo timmean   ${expname}__${pername}_DM__${var}.nc ${expname}__${pername}_clim__${var}.nc
  done
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
  for var in pr; do
    cdo setrtomiss,1e30,1e40 -seldate,${yeari}-01-01,${yearf}-12-31 ${SP02DIR}/Spain02D_${var}.nc Spain02__${pername}_DM__${var}.nc
    cdo yseasmean Spain02__${pername}_DM__${var}.nc    Spain02__${pername}_sclim__${var}.nc
    cdo timsum    Spain02__${pername}_sclim__${var}.nc Spain02__${pername}_clim__${var}.nc
  done
fi
