#! /bin/bash
source /software/ScientificLinux/4.6/etc/bashrc
source dirs
scriptdir=$( (cd `dirname $0` && echo $PWD) )

wxajcmd="python /oceano/gmeteo/WORK/chus/wrf4g/util/postprocess/wrfnc_extract_and_join.py -a /oceano/gmeteo/WORK/chus/wrf4g/util/postprocess/wrfnc_extract_and_join.gattr_CORDEX -t /oceano/gmeteo/WORK/chus/wrf4g/util/postprocess/wrfnc_extract_and_join.table -g /oceano/gmeteo/DATA/WRF/domains/CORDEX_Africa50_SMG1/geo_em.d01.nc -r 1950-01-01_00:00:00"
tag=$(date +'%Y%m%d%H%M%S')

function get_years(){
  idir=$1
  find ${idir} -name wrfout_d01_\*.nc \
    | sed -e 's/^.*wrfout_d01_\(.*\)....T......Z.nc$/\1/' \
    | sort -n | uniq
}

function get_years(){
  echo 1998
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
    if test -f ${oname}; then
      echo "Cowardly refusing to overwrite ${oname}. Delete it yourself (if you dare!)"
      continue
    fi
    echo "Writing ${oname}"
    ${wxajcmd} -v T2,RAIN,U10ER,V10ER -o ${POSTDIR}/${oname} ${wrfoutpath}/wrfout_d01_${year}*.nc
    cdo settime,00:00 -daysum -selvar,pr ${POSTDIR}/${oname} ${POSTDIR}/${prettyname}_DM_pr_${year}.nc
    cdo settime,00:00 -chname,tas,tasmax -daymax -selvar,tas ${POSTDIR}/${oname} ${POSTDIR}/${prettyname}_DM_tasmax_${year}.nc
    cdo settime,00:00 -chname,tas,tasmin -daymin -selvar,tas ${POSTDIR}/${oname} ${POSTDIR}/${prettyname}_DM_tasmin_${year}.nc
    cdo settime,00:00 -daymean -selvar,tas ${POSTDIR}/${oname} ${POSTDIR}/${prettyname}_DM_tas_${year}.nc
    cdo settime,00:00 -daymean -selvar,uas ${POSTDIR}/${oname} ${POSTDIR}/${prettyname}_DM_uas_${year}.nc
    cdo settime,00:00 -daymean -selvar,vas ${POSTDIR}/${oname} ${POSTDIR}/${prettyname}_DM_vas_${year}.nc
  done
}

function clim_and_diff (){
  for sim in CUBM # CTRL CUKF BLMY MPW6 RARR LSRU # BLPX
  do
    for var in uas vas pr tas tasmax tasmin
    do
      cdo timmean ${POSTDIR}/CORDEX_UC_WRF_SENS${sim}_DM_${var}_1998.nc ${POSTDIR}/CORDEX_UC_WRF_SENS${sim}_clim_${var}_1998.nc
      test ${sim} != "CTRL" && \
        cdo sub ${POSTDIR}/CORDEX_UC_WRF_SENS${sim}_clim_${var}_1998.nc ${POSTDIR}/CORDEX_UC_WRF_SENSCTRL_clim_${var}_1998.nc \
          ${POSTDIR}/CORDEX_UC_WRF_SENS${sim}_diff_${var}_1998.nc
    done
  done
}

function seasclim_and_diff (){
  months=$1
  label=$2
  for sim in CTRL CUBM CUKF BLMY MPW6 RARR LSRU BLPX
  do
    for var in uas vas pr tas tasmax tasmin
    do
      cdo timmean -selmon,${months} ${POSTDIR}/CORDEX_UC_WRF_${experiment}${sim}_DM_${var}_1998.nc \
        ${POSTDIR}/CORDEX_UC_WRF_${experiment}${sim}_sclim${label}_${var}_1998.nc
      test ${sim} != "CTRL" && \
        cdo sub ${POSTDIR}/CORDEX_UC_WRF_${experiment}${sim}_sclim${label}_${var}_1998.nc ${POSTDIR}/CORDEX_UC_WRF_${experiment}CTRL_sclim${label}_${var}_1998.nc \
          ${POSTDIR}/CORDEX_UC_WRF_${experiment}${sim}_sdiff${label}_${var}_1998.nc
    done
  done
}

function spread(){
  for var in pr tas tasmax tasmin
  do
    cdo ensmax ${POSTDIR}/CORDEX_UC_WRF_${experiment}???[LMFY6RU]_clim_${var}_1998.nc s1
    cdo ensmin ${POSTDIR}/CORDEX_UC_WRF_${experiment}???[LMFY6RU]_clim_${var}_1998.nc s2
    cdo sub s1 s2 ${POSTDIR}/CORDEX_UC_WRF_${experiment}_spread_${var}.nc
  done
}

experiment="SEN2"
#process_realization CORDEX_UC_WRF_${experiment}CTRL
#process_realization CORDEX_UC_WRF_${experiment}CUBM
#process_realization CORDEX_UC_WRF_${experiment}CUKF
#process_realization CORDEX_UC_WRF_${experiment}BLMY
#process_realization CORDEX_UC_WRF_${experiment}BLPX
#process_realization CORDEX_UC_WRF_${experiment}MPW6
#process_realization CORDEX_UC_WRF_${experiment}RARR
#process_realization CORDEX_UC_WRF_${experiment}LSRU
#process_realization CORDEX_UC_WRF_${experiment}CTL2
#clim_and_diff
seasclim_and_diff 1,2,3 JFM
#spread

