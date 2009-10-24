#! /bin/bash
thisdir=$(pwd)
scriptdir=$(dirname $0)
basedir=$(dirname $(dirname $0))
source ${scriptdir}/dirs
source env.${HOSTNAME//.*/}
source /oceano/gmeteo/WORK/chus/wrf4g/wn/lib/bash/wrf_util.sh

basedir=/vols/tetis/escena/METEO4G

dom=d01

rea=scn5
datei=200501
datef=200601

rea=scn6__2005122900_2009122400
datei=200801
datef=200812

rea=scn6__1996010100_1999122700
datei=199801
datef=199901

rea=scn3
datei=199501
datef=199601

test -f geo_em.${dom}.nc || cp /oceano/gmeteo/WORK/MDM.UC/WRF/domains/ESCENA-UCLM2/geo_em.${dom}.nc .
mkdir -p $POSTDIR

read exp param <<< $(echo $rea | sed -e 's/__/ /g')

for yearmon in $(get_yearmons ${datei:0:4} ${datei:4:2} ${datef:0:4} ${datef:4:2}); do
  files="${basedir}/WRF/experiments/${exp}/${rea}/output/????/wrfout_${dom}_${yearmon}"
  echo "Processing $files"
  test "$(\ls -1 ${files}* | wc | awk '{print $1}')" -ge 28 || continue
  python ${WRFNCXJPY} \
    ${files}'*.nc' \
    ${POSTDIR}/${expname}_${yearmon}_${rea}.nc \
    RAINC,RAINNC,T2,U10ER,V10ER \
    geo_em.${dom}.nc \
    >& ${expname}_${yearmon}_${rea}.log
done

rm geo_em.${dom}.nc
