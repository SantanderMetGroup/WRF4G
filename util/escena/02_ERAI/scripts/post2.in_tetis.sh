#! /bin/bash
thisdir=$(pwd)
scriptdir=$( (cd `dirname $0` && echo $PWD) )
basedir=$(dirname $scriptdir)
source ${scriptdir}/dirs
source env.${HOSTNAME//.*/}

mkdir -p ${POST2DIR}

function procesa(){
  yr=$1
  inname="${POSTDIR}/${expname}_3H_${yr}_prtas.nc"
  bnamed=${expname}_DM_${yr}
  cdo -r settime,00:00 -chname,tas,tasmax -daymax -selvar,tas ${inname} ${POST2DIR}/${bnamed}_tasmax.nc
  cdo -r settime,00:00 -chname,tas,tasmin -daymin -selvar,tas ${inname} ${POST2DIR}/${bnamed}_tasmin.nc
  cdo -r settime,00:00                    -daysum -selvar,pr  ${inname} ${POST2DIR}/${bnamed}_pr.nc
}

for year in $(seq 1989 2008); do
  procesa $year
done

cd ${POST2DIR}
for var in pr tasmax tasmin; do
  cdo cat ${expname}_DM_????_${var}.nc ${expname}_DM_1989-2008_${var}.nc
  # Monthly Dic'89-Nov'08
  cdo -setday,15 -monmean -seldate,1989-12-01T00:00,2008-11-30T23:59 ${expname}_DM_1989-2008_${var}.nc ${expname}_MM_1989-2008_${var}.nc
done
