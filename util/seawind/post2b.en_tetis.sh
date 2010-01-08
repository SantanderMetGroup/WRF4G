#! /bin/bash
#PBS -q tetis

source /software/ScientificLinux/4.6/etc/bashrc

exp="Pswh2009"
fullexp=SeaWind_I1540RF06

postdir="/vols/tetis/escena/SEAWIND/SeaWind_I1540RF06__1989_2009"
#postdir="/oceano/gmeteo/SCRATCH/ASNA/SEAWIND/SeaWind_I1540RF06__1989_2009"
datadir="/vols/tetis/escena/METEO4G/WRF/experiments/${exp}"
#datadir="/oceano/gmeteo/SCRATCH/MDM.UC/experiments/${exp}"

mkdir -p $postdir
cd $postdir

weajdir="/oceano/gmeteo/WORK/chus/wrf4g/util/postprocess"
  weajpy="${weajdir}/wrfnc_extract_and_join.py"
  weajtbl="${weajdir}/wrfnc_extract_and_join.table"
  weajatt="${weajdir}/wrfnc_extract_and_join.gattr_ISEAWIND"
geofile="/vols/tetis/escena/METEO4G/WRF/domains/Europe_15k/geo_em.d01.nc"
geofile="/oceano/gmeteo/WORK/MDM.UC/WRF/domains/Europe_15k/geo_em.d01.nc"

for dir in ${datadir}/${exp}__*
do
  read expname dates trash <<< ${dir//__/ }
  read datei datef <<< ${dates//_/ }
  expname=$(basename ${expname})
  files=$(\ls -1 ${dir}/output/wrfout*.nc_PLEV)
  outname=${expname}__${datei}_plev.nc
  if test -f "${datei:0:4}/${fullexp}__${datei}.nc"; then
    echo "There is a full file for this date ($datei). Won't waste my time"
    continue
  fi
  if test -f "${outname}"; then
    echo "Cowardly refusing to overwrite $outname"
    continue 
  else
    echo Writing $outname
  fi
  python ${weajpy} -p \
    -v PSFC,GHT,Q2,T2,U10ER,V10ER,MSLP,RAIN,RH,TEMP \
    -r 1940-01-01_00:00:00 \
    -t ${weajtbl} -a ${weajatt} \
    -g ${geofile} \
    -o ${outname} \
    ${files}
done
#    -v PSFC,Q2,T2,U10ER,V10ER,MSLP,RAIN,RH,TEMP \
