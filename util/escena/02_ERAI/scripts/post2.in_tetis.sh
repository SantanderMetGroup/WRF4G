export PATH=/software/ScientificLinux/4.6/netcdf/3.6.3/pgf716_gcc/bin/:$PATH
export PATH=/software/ScientificLinux/4.6/cdo/1.3.0/bin:$PATH
export PATH=/software/ScientificLinux/4.6/grib_api/1.7.0/bin:$PATH

expname="WRF_ERAI"
parname=""

function get_nc_timerecords(){
  ncdump -h $1 | grep UNLIMITED | tr -d -c '0-9'
}

function cdo_shifted(){
  thedate=$(ncdump -h $1 | awk '/time:units/ {print $(NF-2), $(NF-1)}' | cut -c-13 | awk '{printf "%s %d hours\n", $1,$2+17}')
  date +'%Y-%m-%d,%H,3hour' -d "${thedate}"
}

function procesa(){
  yr=$1
  ncrcat -O data/post/${expname}_${yr}*${parname}.nc tmp.nc
  cdo settaxis,${yr}-01-01,00:00,3hours tmp.nc tmp2.nc
  ntimes=$(get_nc_timerecords tmp2.nc)
  ncap2 -O -s 'RAIN=RAINNC+RAINC;RAINSTEP=RAIN(1:,:,:)-RAIN(:'$(($ntimes -2))',:,:);' tmp2.nc tmp.nc 
  bname=${expname}__${yr}_3H
  bnamed=${expname}__${yr}_DM
  ncks -O -x -v RAIN,RAINC,RAINNC tmp.nc data/post2/${bname}.nc
  cdo -r settime,00:00 -chname,T2,tasmax -daymax -selvar,T2 data/post2/${bname}.nc data/post2/${bnamed}__tasmax.nc
  cdo -r settime,00:00 -chname,T2,tasmin -daymin -selvar,T2 data/post2/${bname}.nc data/post2/${bnamed}__tasmin.nc
  cdo -r settime,00:00 -chname,RAINSTEP,pr -daysum -setrtomiss,1e30,1e40 \
    -settaxis,$(cdo_shifted data/post2/${bname}.nc) \
    -selvar,RAINSTEP data/post2/${bname}.nc data/post2/${bnamed}__pr.nc
  rm -f tmp.nc tmp2.nc
}

for year in $(seq 1989 1994); do
  procesa $year
done
