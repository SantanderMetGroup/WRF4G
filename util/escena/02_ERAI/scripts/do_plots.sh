#! /bin/bash
thisdir=$(pwd)
scriptdir=$( (cd `dirname $0` && echo $PWD) )
basedir=$(dirname $scriptdir)
source ${scriptdir}/dirs
source env.${HOSTNAME//.*/}


function var_range(){
  case $1 in
    tas) echo "zmin 265 zmax 302 dz 1" ;;
    tasmax) echo "zmin 0 zmax 40 dz 5" ;;
    tasmin) echo "zmin -10 zmax 30 dz 5" ;;
    pr) echo "zmin 0 zmax 3000 dz 200" ;;
  esac
}

ncdir="/vols/tetis/escena/ESCENA/scn1"

# Spain 02

#  for var in tasmax tasmin; do
#    bash plot_wrf_data.gmt.sh ${ncdir}/Spain02D_${var}_clim.2001.nc var $var cpt cpts/tas.cpt $(var_range $var)  national_bound out Spain02D_2001.${var}_clim.eps
#    irec=0
#    for seas in DJF MAM JJA SON; do
#      bash plot_wrf_data.gmt.sh ${ncdir}/Spain02D_${var}_sclim.2001.nc \
#        out Spain02D_2001.${var}_sclim.eps \
#        var $var \
#        rec $irec \
#        label $seas \
#        cpt cpts/tas.cpt \
#        $(var_range $var) \
#        national_bound
#      let irec++
#    done
#  done
#  for var in pr; do
#    bash plot_wrf_data.gmt.sh ${ncdir}/Spain02D_${var}_clim.2001.nc var $var cpt cpts/pr.cpt $(var_range $var) national_bound out Spain02D_2001.${var}_clim.eps
#    irec=0
#    for seas in DJF MAM JJA SON; do
#      bash plot_wrf_data.gmt.sh ${ncdir}/Spain02D_${var}_sclim.2001.nc \
#        out Spain02D_2001.${var}_sclim.eps \
#        var $var \
#        rec $irec \
#        label $seas \
#        cpt cpts/pr.cpt \
#        $(var_range $var) \
#        national_bound
#      let irec++
#    done
#  done
##
##  WRF scn1
##
#for par in 111 155 311 355; do
#  for var in tasmax tasmin; do
#    bash plot_wrf_data.gmt.sh ${ncdir}/SCN1_${par}_2001.${var}_clim.nc var $var cpt cpts/tas.cpt $(var_range $var) national_bound is_curvilinear
#    irec=0
#    for seas in DJF MAM JJA SON; do
#      bash plot_wrf_data.gmt.sh ${ncdir}/SCN1_${par}_2001.${var}_sclim.nc \
#        var $var \
#        rec $irec \
#        label $seas \
#        cpt cpts/tas.cpt \
#        $(var_range $var) \
#        national_bound is_curvilinear
#      let irec++
#    done
#  done
#  for var in pr; do
#    bash plot_wrf_data.gmt.sh ${ncdir}/SCN1_${par}_2001.${var}_clim.nc var $var cpt cpts/pr.cpt $(var_range $var) national_bound is_curvilinear
#    irec=0
#    for seas in DJF MAM JJA SON; do
#      bash plot_wrf_data.gmt.sh ${ncdir}/SCN1_${par}_2001.${var}_sclim.nc \
#        var $var \
#        rec $irec \
#        label $seas \
#        cpt cpts/pr.cpt \
#        $(var_range $var) \
#        national_bound is_curvilinear
#      let irec++
#    done
#  done
#done

#
# EOBS 025
#
bname="${BIGDIR}/EOBS/EOBS025__${pername}"
oname="${basedir}/figs/EOBS025__${pername}"
for var in tasmax tasmin; do
  bash plot_escena_data.gmt.sh ${bname}_clim__${var}.nc var $var cpt cpts/orig.cpt $(var_range $var) national_bound out ${oname}_clim__${var}.eps
exit
  irec=0
  for seas in DJF MAM JJA SON; do
    bash plot_escena_data.gmt.sh ${bname}_sclim__${var}.nc \
      out ${oname}_sclim${seas}__${var}.eps \
      var $var \
      rec $irec \
      label $seas \
      cpt cpts/tas.cpt \
      $(var_range $var) \
      national_bound
    let irec++
  done
done
 

