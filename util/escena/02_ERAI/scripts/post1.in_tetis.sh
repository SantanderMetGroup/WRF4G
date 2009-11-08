#! /bin/bash
thisdir=$(pwd)
scriptdir=$(dirname $0)
basedir=$(dirname $(dirname $0))
source ${scriptdir}/dirs
source env.${HOSTNAME//.*/}

dom=d01

test -f geo_em.${dom}.nc || cp /oceano/gmeteo/WORK/MDM.UC/WRF/domains/ESCENA-UCLM2/geo_em.${dom}.nc .
mkdir -p $POSTDIR

for year in $(seq 1989 2008); do
  python ${WRFNCXJPY} \
    --from-file filelists/ESCENA_ERAI_files.${yr} \
    -o ${POSTDIR}/${expname}_3H_${year}_prtas.nc \
    -v RAIN,T2 \
    -r "1950-01-01_00:00:00" \
    -a ${WRFNCXJATTR} \
    -t ${WRFNCXJTBL} \
    -g geo_em.${dom}.nc \
    >& ${expname}_3H_${year}_prtas.log
done

rm geo_em.${dom}.nc
