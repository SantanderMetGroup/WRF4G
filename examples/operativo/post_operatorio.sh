#! /bin/bash
#
#PBS -q macc
#PBS -N post_operatorio

sdate=$1
syy=${sdate:0:4}
smm=${sdate:4:2}
sdd=${sdate:6:2}

cd /oceano/gmeteo/WORK/chus/experimentos/operativo

source /software/ScientificLinux/4.6/etc/bashrc
source /software/etc/bashrc
wrfncpost="python /oceano/gmeteo/users/chus/usr/lib/python/wrfnc_extract_and_join.py"
outdir="/oceano/gmeteo/DATA/UC/OPERWRF12/${sdate}"
mkdir -p ${outdir}

cat << EOF > grid15.cdo
gridtype = lonlat
xsize = 97
ysize = 58
xfirst = -10.
xinc = 0.15
yfirst = 35.5
yinc = 0.15
EOF

function get_nc_timerecords(){
  ncdump -h $1 | grep UNLIMITED | tr -d -c '0-9'
}

for rea in /oceano/gmeteo/WORK/MDM.UC/WRF/experiments/oper${sdate}/*; do
  echo "Processing: $rea"
  ${wrfncpost} ${rea}'/output/wrfout_d01*.nc' tmp.nc "T2,U10,V10,RAINC,RAINNC"
  cdo remapbil,grid15.cdo tmp.nc tmp2.nc
  ntimes=$(get_nc_timerecords tmp2.nc)
  ncap2 -O -s 'RAIN=RAINNC+RAINC;RAINSTEP=RAIN(1:,:,:)-RAIN(:'$(($ntimes -2))',:,:);' tmp2.nc tmp.nc 
  ncks -O -x -v RAIN,RAINC,RAINNC tmp.nc ${outdir}/$(basename ${rea}).nc
  cdo -r settime,00:00 -daymax -selvar,T2 ${outdir}/$(basename ${rea}).nc ${outdir}/$(basename ${rea}).t2x.nc
  cdo -r settime,00:00 -daymin -selvar,T2 ${outdir}/$(basename ${rea}).nc ${outdir}/$(basename ${rea}).t2n.nc
  cdo -r settime,00:00 -daysum \
    -settaxis,${syy}-${smm}-${sdd},5,1hour \
    -selvar,RAINSTEP ${outdir}/$(basename ${rea}).nc ${outdir}/$(basename ${rea}).pr.nc
done

rm -f grid15.cdo
rm -f tmp.nc tmp2.nc
