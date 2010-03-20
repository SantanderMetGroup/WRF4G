#! /bin/bash
source /software/ScientificLinux/4.6/etc/bashrc
scriptdir=$( (cd `dirname $0` && echo $PWD) )

EXPDIR="/vols/tetis/meteo4g/WRF/experiments"
POSTDIR="/vols/tetis/escena/CORDEX/post"
mkdir -p ${POSTDIR}

wxajcmd="python /oceano/gmeteo/WORK/chus/wrf4g/util/postprocess/wrfnc_extract_and_join.py -a /oceano/gmeteo/WORK/chus/wrf4g/util/postprocess/wrfnc_extract_and_join.gattr_CORDEX -t /oceano/gmeteo/WORK/chus/wrf4g/util/postprocess/wrfnc_extract_and_join.table -g /oceano/gmeteo/DATA/WRF/domains/CORDEX_Africa50_SMG1/geo_em.d01.nc -r 1950-01-01_00:00:00"
tag=$(date +'%Y%m%d%H%M%S')

function get_years(){
  idir=$1
  find ${idir} -name wrfout_d01_\*.nc \
    | sed -e 's/^.*wrfout_d01_\(.*\)....T......Z.nc$/\1/' \
    | sort -n | uniq
}

function process_realization() {
  #
  #  function to process one single realization
  #
  prettyname=$1
  reaname=$(awk '/'$prettyname'/{print $2}' ${scriptdir}/simulation_map.txt)
  wrfoutpath="${EXPDIR}/${reaname}/output"
  for year in $(get_years ${wrfoutpath})
  do
    oname="${prettyname}_3H_sfc_${year}.nc"
#    if test -f ${oname}; then
#      echo "Cowardly refusing to overwrite ${oname}. Delete it yourself (if you dare!)"
#      continue
#    fi
#    echo "Writing ${oname}"
#    ${wxajcmd} -v T2,RAIN,U10ER,V10ER -o ${POSTDIR}/${oname} ${wrfoutpath}/wrfout_d01_${year}*.nc
    cdo settime,00:00 -daysum -selvar,pr ${POSTDIR}/${oname} ${prettyname}_DM_pr_${year}.nc
    cdo settime,00:00 -chname,tas,tasmax -daymax -selvar,tas ${POSTDIR}/${oname} ${prettyname}_DM_tasmax_${year}.nc
    cdo settime,00:00 -chname,tas,tasmin -daymin -selvar,tas ${POSTDIR}/${oname} ${prettyname}_DM_tasmin_${year}.nc
    cdo settime,00:00 -daymean -selvar,tas ${POSTDIR}/${oname} ${prettyname}_DM_tas_${year}.nc
  done
}

function clim_and_diff (){
  for sim in CUKF BLMY BLPX MPW6 RARR LSRU
  do
    cdo timmean ${POSTDIR}/CORDEX_UC_WRF_SENS${sim}_3H_sfc_1998.nc ${POSTDIR}/CORDEX_UC_WRF_SENS${sim}_3H_sfc_1998_clim.nc
    cdo sub ${POSTDIR}/CORDEX_UC_WRF_SENS${sim}_3H_sfc_1998_clim.nc ${POSTDIR}/CORDEX_UC_WRF_SENSCTRL_3H_sfc_1998_clim.nc \
      ${POSTDIR}/CORDEX_UC_WRF_SENS${sim}_3H_sfc_1998_diff.nc
  done
}

function spread(){
  cdo ensmax ${POSTDIR}/CORDEX_UC_WRF_SENS????_3H_sfc_1998_clim.nc s1
  cdo ensmin ${POSTDIR}/CORDEX_UC_WRF_SENS????_3H_sfc_1998_clim.nc s2
  cdo sub s1 s2 ${POSTDIR}/CORDEX_UC_WRF_SENS_spread.nc
}

process_realization CORDEX_UC_WRF_SENSCTRL
process_realization CORDEX_UC_WRF_SENSCUKF
process_realization CORDEX_UC_WRF_SENSBLMY
process_realization CORDEX_UC_WRF_SENSBLPX
process_realization CORDEX_UC_WRF_SENSMPW6
process_realization CORDEX_UC_WRF_SENSRARR
process_realization CORDEX_UC_WRF_SENSLSRU
#clim_and_diff
#spread

