#! /bin/bash
thisdir=$(pwd)
scriptdir=$( (cd `dirname $0` && echo $PWD) )
basedir=$(dirname $scriptdir)
source ${scriptdir}/dirs
source env.${HOSTNAME//.*/}

dom=d01

test -f geo_em.${dom}.nc || cp /oceano/gmeteo/WORK/MDM.UC/WRF/domains/ESCENA-UCLM2/geo_em.${dom}.nc .
mkdir -p $POSTDIR

for year in $(seq 1990 2007); do
  python ${WRFNCXJPY} \
    --from-file ${scriptdir}/filelists/ESCENA_ERAI_files.${year} \
    -o ${POSTDIR}/${expname}_3H_${year}_uvqps.nc \
    -v U10ER,V10ER,Q2,PSFC \
    -r "1950-01-01_00:00:00" \
    -a ${WRFNCXJATTR} \
    -t ${WRFNCXJTBL} \
    -g geo_em.${dom}.nc \
    >& ${expname}_3H_${year}_uvqps.log
done

rm geo_em.${dom}.nc
