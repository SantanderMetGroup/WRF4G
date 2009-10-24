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
mkdir -p ${BIGDIR}/EOBS
cdo seldate,${yeari}-01-01,${yearf}-12-31 ${EOBSDIR}/tx_0.25deg_CRU_version2.0.nc ${BIGDIR}/EOBS/EOBS025__${pername}_DM__tasmax.nc
cdo seldate,${yeari}-01-01,${yearf}-12-31 ${EOBSDIR}/tn_0.25deg_CRU_version2.0.nc ${BIGDIR}/EOBS/EOBS025__${pername}_DM__tasmin.nc

bname="${BIGDIR}/EOBS/EOBS025__${pername}"
for var in tasmax tasmin; do
  cdo sellonlatbox,-10,5,35,45 ${bname}_DM__${var}.nc tmp2.nc
  ncap2 -O -s ''${var}'=data/100.' tmp2.nc tmp.nc
  ncks -O -x -v data tmp.nc tmp2.nc
  cdo setrtomiss,-100,-99 tmp2.nc ${bname}_DM__${var}.nc
  cdo ymonmean  ${bname}_DM__${var}.nc    ${bname}_mclim__${var}.nc
  cdo yseasmean ${bname}_DM__${var}.nc    ${bname}_sclim__${var}.nc
  cdo timmean   ${bname}_mclim__${var}.nc ${bname}_clim__${var}.nc
  cdo setday,15 -monmean   ${bname}_DM__${var}.nc    ${bname}_MM__${var}.nc
  cdo ymonstd   ${bname}_MM__${var}.nc    ${bname}_mstd__${var}.nc
  rm tmp2.nc
  rm tmp.nc
done

