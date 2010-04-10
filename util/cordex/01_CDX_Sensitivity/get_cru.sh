#! /bin/bash
scriptdir=$( (cd `dirname $0` && echo $PWD) )
source /software/ScientificLinux/4.6/etc/bashrc || source /home/usr/etc/bashrc
source ${scriptdir}/dirs

CRUDIR=/oceano/gmeteo/WORK/CRU/TS3.0
OBSDIR=${CRUDIR}

for varon in tmp_tas tmn_tasmin tmx_tasmax
do
  read var newvar <<< ${varon/_/ }
  cdo -chname,${var},${newvar} -selyear,1998 ${CRUDIR}/cru_ts_3_00.1901.2006.${var}.nc pp
  # ncatted -O -a long_name,lat,o,c,latitude -a long_name,lon,o,c,longitude pp
  cdo sellonlatbox,-25,60.5,-45,45 pp ${OBSDIR}/CRUTS30AFR_MM_1998-1998_${newvar}.nc
done

cdo -chname,pre,pr -divdpm -selyear,1998 ${CRUDIR}/cru_ts_3_00.1901.2006.pre.nc pp
#ncatted -O -a long_name,lat,o,c,latitude -a long_name,lon,o,c,longitude -a units,pr,o,c,mm/day pp
ncatted -O -a units,pr,o,c,mm/day pp
cdo sellonlatbox,-25,60.5,-45,45 pp ${OBSDIR}/CRUTS30AFR_MM_1998-1998_pr.nc


