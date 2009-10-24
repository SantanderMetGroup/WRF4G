#! /bin/bash
thisdir=$(pwd)
scriptdir=$(dirname $0)
basedir=$(dirname $(dirname $0))
source ${scriptdir}/dirs
source env.${HOSTNAME//.*/}

read yeari yearf <<< ${pername//_/ }
#
# Run this only on oceano!
#
mkdir -p ${BIGDIR}/EOBS
bname="${BIGDIR}/EOBS/EOBS025__${pername}"
versname="_0.25deg_reg_v2.0.nc"
cdo seldate,${yeari}-01-01,${yearf}-12-31 ${EOBSDIR}/tx${versname} ${bname}_DM__tasmax.nc
cdo seldate,${yeari}-01-01,${yearf}-12-31 ${EOBSDIR}/tn${versname} ${bname}_DM__tasmin.nc
# Build tas
cdo mulc,0.5 -add ${bname}_DM__tasmax.nc ${bname}_DM__tasmin.nc ${bname}_DM__tas.nc
cdo seldate,${yeari}-01-01,${yearf}-12-31 ${EOBSDIR}/rr${versname} ${bname}_DM__pr.nc

for var in tasmax tasmin tas pr; do
  cdo sellonlatbox,-10,5,35,45 ${bname}_DM__${var}.nc tmp2.nc
  case ${var} in
    pr)
#      ncap2 -O -s ''${var}'=rr/10.' tmp2.nc tmp.nc
#      ncks -O -x -v data tmp.nc tmp2.nc
#      cdo setrtomiss,-1000,-999 tmp2.nc ${bname}_DM__${var}.nc
       cdo chname,rr,pr tmp2.nc ${bname}_DM__${var}.nc
      ;;
    tasmax|tas)
#      ncap2 -O -s ''${var}'=tx/100.' tmp2.nc tmp.nc
#      ncks -O -x -v data tmp.nc tmp2.nc
      cdo addc,273.15 -chname,tx,${var} tmp2.nc ${bname}_DM__${var}.nc
      ;;
    tasmin)
#      ncap2 -O -s ''${var}'=tn/100.' tmp2.nc tmp.nc
#      ncks -O -x -v data tmp.nc tmp2.nc
      cdo addc,273.15 -chname,tn,${var} tmp2.nc ${bname}_DM__${var}.nc
      ;;
  esac
  cdo ymonmean  ${bname}_DM__${var}.nc    ${bname}_mclim__${var}.nc
  cdo yseasmean ${bname}_DM__${var}.nc    ${bname}_sclim__${var}.nc
  cdo timmean   ${bname}_mclim__${var}.nc ${bname}_clim__${var}.nc
  cdo setday,15 -monmean   ${bname}_DM__${var}.nc    ${bname}_MM__${var}.nc
  cdo ymonstd   ${bname}_MM__${var}.nc    ${bname}_mstd__${var}.nc
  rm tmp2.nc
  rm tmp.nc
done
