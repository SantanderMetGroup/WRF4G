#! /bin/bash

source dirs
ERAIDIR=/oceano/gmeteo/DATA/ECMWF/INTERIM/Global0.7
year=1998

for vardes in 850_133,hus 850_131,ua 850_132,va 850_130,ta SFC_167,tas
do
  read lvlcode var <<< ${vardes/,/ }
  read lvl code <<< ${lvlcode/_/ }
  cdo -t ecmwf cat ${ERAIDIR}/${year}/G07_INTERIM_${year}??_${lvlcode}.128.grb pp
  test "$lvl" = "SFC" && lvl=""
  cdo -r -f nc -sellonlatbox,-26,61,-46,46 pp ${OBSDIR}/ERAIN_6H_${year}_${var}${lvl}.nc
  cdo daymean ${OBSDIR}/ERAIN_6H_${year}_${var}${lvl}.nc ${OBSDIR}/ERAIN_DM_${year}_${var}${lvl}.nc
  rm -f pp
done
