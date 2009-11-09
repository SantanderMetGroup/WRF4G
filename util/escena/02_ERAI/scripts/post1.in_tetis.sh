#! /bin/bash
thisdir=$(pwd)
scriptdir=$( (cd `dirname $0` && echo $PWD) )
basedir=$(dirname $scriptdir)
source ${scriptdir}/dirs
source env.${HOSTNAME//.*/}

dom=d01

test -f geo_em.${dom}.nc || cp /oceano/gmeteo/WORK/MDM.UC/WRF/domains/ESCENA-UCLM2/geo_em.${dom}.nc .
mkdir -p $POSTDIR

for year in 1989 1999; do
  python ${WRFNCXJPY} \
    --from-file ${scriptdir}/filelists/ESCENA_ERAI_files.${year} \
    -o ${POSTDIR}/${expname}_3H_${year}_prtas.nc \
    -v RAIN,T2 \
    -r "1950-01-01_00:00:00" \
    -a ${WRFNCXJATTR} \
    -t ${WRFNCXJTBL} \
    -g geo_em.${dom}.nc \
    >& ${expname}_3H_${year}_prtas.log
done

python ${WRFNCXJPY} \
  $(head -1 ${scriptdir}/filelists/ESCENA_ERAI_files.1989) \
  -o ${POSTDIR}/${expname}_FIX_sftlf.nc \
  -v LANDMASK \
  -r "1950-01-01_00:00:00" --single-record \
  -a ${WRFNCXJATTR} \
  -t ${WRFNCXJTBL} \
  -g geo_em.${dom}.nc \
  >& sftlf.log

python ${WRFNCXJPY} \
  $(head -1 ${scriptdir}/filelists/ESCENA_ERAI_files.1989) \
  -o ${POSTDIR}/${expname}_FIX_orog.nc \
  -v HGT \
  -r "1950-01-01_00:00:00" --single-record \
  -a ${WRFNCXJATTR} \
  -t ${WRFNCXJTBL} \
  -g geo_em.${dom}.nc \
  >& sftlf.log

rm geo_em.${dom}.nc
