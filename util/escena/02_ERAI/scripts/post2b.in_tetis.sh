#! /bin/bash
thisdir=$(pwd)
scriptdir=$( (cd `dirname $0` && echo $PWD) )
basedir=$(dirname $scriptdir)
source ${scriptdir}/dirs
source env.${HOSTNAME//.*/}

mkdir -p ${POST2DIR}

function procesa(){
  yr=$1
  inname="${POSTDIR}/${expname}_3H_${yr}_uvqps.nc"
  bnamed=${expname}_DM_${yr}
  bname3=${expname}_3H_${yr}
  cdo -r sqr -selvar,uas ${inname} u2
  cdo -r sqr -selvar,vas ${inname} v2
  cdo -r settime,00:00 -chname,uas,wssmax -daymax -sqrt -add u2 v2 ${POST2DIR}/${bnamed}_wssmax.nc
  rm -f u2 v2
  cdo selvar,ps ${inname} ${POST2DIR}/${bname3}_ps.nc
  cdo selvar,huss ${inname} ${POST2DIR}/${bname3}_huss.nc
  cdo selvar,uas ${inname} ${POST2DIR}/${bname3}_uas.nc
  cdo selvar,vas ${inname} ${POST2DIR}/${bname3}_vas.nc
}

#for year in $(seq 1989 2008); do
#  procesa $year
#done
#
cd ${POST2DIR}
#for var in wssmax; do
#  cdo cat ${expname}_DM_????_${var}.nc ${expname}_DM_1989-2008_${var}.nc
#done

periodos3h="1989-1993 1994-1998 1999-2003 2004-2008"
for var in ps huss uas vas; do
  for per in ${periodos3h}; do
    files=""
    for yr in $(seq `echo $per | tr '-' ' '`); do
      files="${files} ${expname}_3H_${yr}_${var}.nc"
    done
    cdo cat ${files} ${expname}_3H_${per}_${var}.nc
  done
done
