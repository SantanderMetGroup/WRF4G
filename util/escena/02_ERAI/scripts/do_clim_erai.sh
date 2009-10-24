#! /bin/bash
thisdir=$(pwd)
scriptdir=$(dirname $0)
basedir=$(dirname $(dirname $0))
source ${scriptdir}/dirs
source env.${HOSTNAME//.*/}

read yeari yearf <<< ${pername//_/ }
#
# Run this only on oceano!
#
mkdir -p ${BIGDIR}/ERAI
bname="${BIGDIR}/ERAI/ERAI__${pername}"

cdo cat /oceano/gmeteo/DATA/ECMWF/INTERIM/escena/*/INTERIM_*_SFC_167.128.grb tmp.grb
cdo -r -f nc -t ecmwf seldate,${yeari}-01-01,${yearf}-12-31 tmp.grb ${bname}_6H__tas.nc
rm tmp.grb
cdo chname,T2M,tasmax -daymax ${bname}_6H__tas.nc ${bname}_DM__tasmax.nc
cdo chname,T2M,tasmin -daymin ${bname}_6H__tas.nc ${bname}_DM__tasmin.nc
# Build daily tas
cdo chname,tasmax,tas -mulc,0.5 -add ${bname}_DM__tasmax.nc ${bname}_DM__tasmin.nc ${bname}_DM__tas.nc

for var in tasmax tasmin tas; do
  cdo sellonlatbox,-10,5,35,45 ${bname}_DM__${var}.nc tmp2.nc
  cdo ymonmean  tmp2.nc    ${bname}_mclim__${var}.nc
  cdo yseasmean tmp2.nc    ${bname}_sclim__${var}.nc
  cdo timmean   ${bname}_mclim__${var}.nc ${bname}_clim__${var}.nc
  cdo setday,15 -monmean   tmp2.nc        ${bname}_MM__${var}.nc
  cdo ymonstd   ${bname}_MM__${var}.nc    ${bname}_mstd__${var}.nc
  rm tmp2.nc
  rm tmp.nc
done
