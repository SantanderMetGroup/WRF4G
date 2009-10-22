#! /bin/bash

thisdir=$(pwd)
basedir=$(dirname $0)
source ${basedir}/dirs

read yeari yearf <<< ${pername//_/ }
#
# Run this only on oceano!
#
mkdir -p ${BIGDIR}/EOBS
cdo seldate,${yeari}-01-01,${yearf}-12-31 ${EOBSDIR}/tx_0.25deg_CRU_version1.1.nc ${BIGDIR}/EOBS/EOBS025__${pername}_DM__tasmax.nc
cdo seldate,${yeari}-01-01,${yearf}-12-31 ${EOBSDIR}/tn_0.25deg_CRU_version1.1.nc ${BIGDIR}/EOBS/EOBS025__${pername}_DM__tasmin.nc

for var in tasmax tasmin; do
  cdo sellonlatbox,-10,5,35,45 ${BIGDIR}/EOBS/EOBS025__${pername}_DM__${var}.nc tmp2.nc
  ncap2 -O -s ''${var}'=data/100.' tmp2.nc tmp.nc
  ncks -O -x -v data tmp.nc tmp2.nc
  cdo setrtomiss,-100,-99 tmp2.nc tmp.nc
  cdo ymonmean tmp.nc ${BIGDIR}/EOBS/EOBS025__${pername}_mclim__${var}.nc
  cdo yseasmean tmp.nc ${BIGDIR}/EOBS/EOBS025__${pername}_sclim__${var}.nc
  cdo timmean ${BIGDIR}/EOBS/EOBS025__${pername}_mclim__${var}.nc ${BIGDIR}/EOBS/EOBS025__${pername}_clim__${var}.nc
  rm tmp2.nc
  rm tmp.nc
done

