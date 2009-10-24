#! /bin/bash
thisdir=$(pwd)
scriptdir=$( (cd `dirname $0` && echo $PWD) )
basedir=$(dirname $scriptdir)
source ${scriptdir}/dirs
source env.${HOSTNAME//.*/}

function get_nc_timerecords(){
  ncdump -h $1 | grep UNLIMITED | tr -d -c '0-9'
}

function cdo_shifted(){
  thedate=$(ncdump -h $1 | awk '/time:units/ {print $(NF-2), $(NF-1)}' | cut -c-13 | awk '{printf "%s %d hours\n", $1,$2+17}')
  date +'%Y-%m-%d,%H,3hour' -d "${thedate}"
}

function procesa(){
  yr=$1
  ncrcat -O ${POSTDIR}/${expname}_${yr}*.nc tmp.nc
  cdo settaxis,${yr}-01-01,00:00,3hours tmp.nc tmp2.nc
  ntimes=$(get_nc_timerecords tmp2.nc)
  ncap2 -O -s 'RAIN=RAINNC+RAINC;RAINSTEP=RAIN(1:,:,:)-RAIN(:'$(($ntimes -2))',:,:);' tmp2.nc tmp.nc 
  bname=${expname}__${yr}_3H
  bnamed=${expname}__${yr}_DM
  ncks -O -x -v RAIN,RAINC,RAINNC tmp.nc ${POST2DIR}/${bname}.nc
  cdo -r settime,00:00 -chname,T2,tasmax -daymax -selvar,T2 ${POST2DIR}/${bname}.nc ${POST2DIR}/${bnamed}__tasmax.nc
  cdo -r settime,00:00 -chname,T2,tasmin -daymin -selvar,T2 ${POST2DIR}/${bname}.nc ${POST2DIR}/${bnamed}__tasmin.nc
  cdo -r settime,00:00 -chname,RAINSTEP,pr -daysum -setrtomiss,1e30,1e40 -setrtoc,-1e8,0,0 \
    -settaxis,$(cdo_shifted ${POST2DIR}/${bname}.nc) \
    -selvar,RAINSTEP ${POST2DIR}/${bname}.nc ${POST2DIR}/${bnamed}__pr.nc
  rm -f tmp.nc tmp2.nc
}

for year in $(seq 1999 1999); do
  procesa $year
done
