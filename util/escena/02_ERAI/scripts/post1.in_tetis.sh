#! /bin/bash
source /software/ScientificLinux/4.6/etc/bashrc
source /oceano/gmeteo/WORK/chus/wrf4g/wn/lib/bash/wrf_util.sh

basedir=/vols/tetis/escena/METEO4G
bname=WRF_ERAI

dom=d01

rea=scn5
datei=200501
datef=200512

rea=scn6__2005122900_2009122400
datei=200601
datef=200712

rea=scn6__1996010100_1999122700
datei=199601
datef=199712

test -f geo_em.${dom}.nc || cp /oceano/gmeteo/WORK/MDM.UC/WRF/domains/ESCENA-UCLM2/geo_em.${dom}.nc .
mkdir -p data/post log

read exp param <<< $(echo $rea | sed -e 's/__/ /g')

for yearmon in $(get_yearmons ${datei:0:4} ${datei:4:2} ${datef:0:4} ${datef:4:2}); do
  files="${basedir}/WRF/experiments/${exp}/${rea}/output/????/wrfout_${dom}_${yearmon}"
  echo "Processing $files"
  test "$(\ls -1 ${files}* | wc | awk '{print $1}')" -ge 28 || continue
  python wrfnc_extract_and_join.py \
    ${files}'*.nc' \
    data/post/${bname}_${yearmon}_${rea}.nc \
    RAINC,RAINNC,T2,U10ER,V10ER \
    geo_em.${dom}.nc \
    >& log/${bname}_${yearmon}_${rea}.log
done
