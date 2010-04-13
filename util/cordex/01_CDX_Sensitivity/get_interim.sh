#! /bin/bash
scriptdir=$( (cd `dirname $0` && echo $PWD) )
source /software/ScientificLinux/4.6/etc/bashrc || source /home/usr/etc/bashrc
source ${scriptdir}/dirs

ERAIDIR=/oceano/gmeteo/DATA/ECMWF/INTERIM/Global0.7
year=1998
period=1998-1998

for vardes in SFC_167,tas # 850_133,hus 850_131,ua 850_132,va 850_130,ta
do
  rm -f pp
  read lvlcode var <<< ${vardes/,/ }
  read lvl code <<< ${lvlcode/_/ }
  cdo cat ${ERAIDIR}/${year}/G07_INTERIM_${year}??_${lvlcode}.128.grb pp
  test "$lvl" = "SFC" && lvl=""
  cdo -r -f nc chname,var${code},${var} $(test "$var" = "tas" && echo "-addc,-273.15") -sellonlatbox,-26,61,-46,46 pp ${OBSDIR}/ERAIN_6H_${period}_${var}${lvl}.nc
  cdo daymean ${OBSDIR}/ERAIN_6H_${period}_${var}${lvl}.nc ${OBSDIR}/ERAIN_DM_${period}_${var}${lvl}.nc
  cdo monmean ${OBSDIR}/ERAIN_DM_${period}_${var}${lvl}.nc ${OBSDIR}/ERAIN_MM_${period}_${var}${lvl}.nc
  rm -f pp
done
