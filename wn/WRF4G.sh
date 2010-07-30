#! /bin/bash
#
# WRF4G.sh
#
test -n "${ROOTDIR}"  || ROOTDIR=$(pwd)
test -n "${LOCALDIR}" || LOCALDIR=${ROOTDIR}
#
#  Load functions and set the PATH
#
source ${ROOTDIR}/lib/bash/wrf_util.sh
source ${ROOTDIR}/lib/bash/wrf4g_exit_codes.sh
export PATH="${ROOTDIR}/bin:${ROOTDIR}/WRFGEL:${ROOTDIR}/lib/bash:$PATH"
export LD_LIBRARY_PATH="${ROOTDIR}/lib/shared_libs:${LD_LIBRARY_PATH}"
#
#  Move all executables out of LOCALDIR
#
mv ${LOCALDIR}/WPS/ungrib/ungrib.exe   ${ROOTDIR}/bin/
mv ${LOCALDIR}/WPS/metgrid/metgrid.exe ${ROOTDIR}/bin/
mv ${LOCALDIR}/WRFV3/run/real.exe      ${ROOTDIR}/bin/
mv ${LOCALDIR}/WRFV3/run/wrf.exe       ${ROOTDIR}/bin/
chmod +x ${ROOTDIR}/bin/*
chmod +x ${ROOTDIR}/WRFGEL/*
umask 002
#
#  Some default values
#
save_wps=1
clean_after_run=1
#
#  Load wrf4g.conf, wrf.chunk and wrf.input
#
source ${ROOTDIR}/wrf4g.conf                           || exit ${ERROR_MISSING_WRF4GCNF}
sed -e 's/\ *=\ */=/' ${ROOTDIR}/wrf.chunk > source.it || exit ${ERROR_MISSING_WRFCHUNK}
source source.it && rm source.it
sed -e 's/\ *=\ */=/' ${ROOTDIR}/wrf.input > source.it || exit ${ERROR_MISSING_WRFINPUT}
source source.it && rm source.it
#
#  Export variables
#
export WRF4G_CONF_FILE="${ROOTDIR}/wrf4g.conf"
export WRF4G_EXPERIMENT="${experiment_name}"
export WRF4G_REALIZATION="${realization_name}"
export WRFGEL_SCRIPT="echo"
#
# Running WRF
#
echo "Previous__________"
ulimit -a
ulimit -s unlimited
echo "After_____________"
ulimit -a
function setup_namelist_input(){
  ln -s ../../WPS/namelist.wps
  fortnml_set    namelist.input run_days    0
  fortnml_set    namelist.input run_hours   0
  fortnml_set    namelist.input run_minutes 0
  fortnml_set    namelist.input run_seconds 0
  fortnml_setn    namelist.input start_year  ${max_dom} ${ryy}
  fortnml_setn    namelist.input start_month ${max_dom} ${rmm}
  fortnml_setn    namelist.input start_day   ${max_dom} ${rdd}
  fortnml_setn    namelist.input start_hour  ${max_dom} ${rhh}
  fortnml_setn    namelist.input end_year    ${max_dom} ${fyy}
  fortnml_setn    namelist.input end_month   ${max_dom} ${fmm}
  fortnml_setn    namelist.input end_day     ${max_dom} ${fdd}
  fortnml_setn    namelist.input end_hour    ${max_dom} ${fhh}
  fortnml_varcopy namelist.wps   namelist.input parent_grid_ratio
  fortnml_varcopy namelist.wps   namelist.input parent_grid_ratio parent_time_step_ratio
  fortnml_varcopy namelist.wps   namelist.input i_parent_start
  fortnml_varcopy namelist.wps   namelist.input j_parent_start
  fortnml_varcopy namelist.wps   namelist.input e_we
  fortnml_varcopy namelist.wps   namelist.input e_sn
  numml=$(get_num_metgrid_levels)
  if test -n "${numml}"; then
    fortnml_set     namelist.input num_metgrid_levels ${numml}
  fi
  nummsl=$(get_num_metgrid_soil_levels)
  if test -n "${nummsl}"; then
    fortnml_set     namelist.input num_metgrid_soil_levels ${nummsl}
  fi
  fortnml_import_record namelist.wps geogrid > to.source
  source to.source && rm -f to.source
  alldx=""
  thisdx=${dx}
  for i in $(seq $max_dom)
  do
    thispgr=$(tuple_item $parent_grid_ratio ${i})
    thisdx=$(echo "scale=5;${thisdx}/${thispgr}" | bc)
    alldx="${alldx} ${thisdx}"
  done
  fortnml_setm namelist.input dx        $alldx
  fortnml_setm namelist.input dy        $alldx # This could be an issue for global WRF
  fortnml_set  namelist.input time_step $(get_timestep $dx ${timestep_dxfactor})
  fortnml_set  namelist.input max_dom   ${max_dom}
  fortnml_set  namelist.input restart   ${chunk_is_restart}
  #
  # A final sanity check...
  #
  fortnml --wrf --trim --overwrite -f namelist.input
}

test -n "${logdir}" || logdir=${LOCALDIR}/log;
mkdir -p ${logdir}
VCPDEBUG="-v"

function timelog_clean(){
  rm -f ${logdir}/time.log
}

function timelog_end(){
  echo "$(date +%Y%m%d%H%M%S)" > ${timelog_item}.end && vcp ${timelog_item}.end ${WRF4G_BASEPATH}/experiments/${experiment_name}/${realization_name}/
  date +%Y%m%d%H%M%S >> ${logdir}/time.log
}

function timelog_init(){
  timelog_item=${1// /_}
  create_output_structure
  echo -e "$(date +%Y%m%d%H%M%S)\n$(hostname --fqdn):$(pwd)" > ${timelog_item}.init && vcp ${timelog_item}.init ${WRF4G_BASEPATH}/experiments/${experiment_name}/${realization_name}/
  echo -n "$(printf "%20s" "$timelog_item") $(date +%Y%m%d%H%M%S) " >> ${logdir}/time.log
}

function wrf4g_exit(){
  excode=$1
  #
  #  This is the way out of this script. So close the timing info, move the
  #  logs to a safe place and leave
  #
  test "${excode}" -eq 0 || timelog_end
  echo "exit $excode" >> ${logdir}/time.log
  ls -l >& ${logdir}/ls.wrf
  if test -e rsl.out.0000; then
    mkdir -p rsl_wrf
    mv rsl.* rsl_wrf/
    mv rsl_wrf ${logdir}/
  fi
  case $excode in
    ${ERROR_UNGRIB_FAILED})
      ls -lR >& ${logdir}/ls.wps
      ;;
  esac
  create_output_structure
  test -f namelist.input && cp namelist.input ${logdir}/
  tar czf log.tar.gz ${logdir} && vcp log.tar.gz ${WRF4G_BASEPATH}/experiments/${experiment_name}/${realization_name}/
  test "${LOCALDIR}" != "${ROOTDIR}" && mv ${logdir} ${ROOTDIR}/
  exit ${excode}
}

timelog_clean
#
#  Get the (re)start (i**) and end (f**) dates for this chunk
#
read iyy imm idd ihh trash <<< $(echo ${chunk_start_date} | tr '_:T-' '    ')
read ryy rmm rdd rhh trash <<< $(echo ${chunk_restart_date} | tr '_:T-' '    ')
read fyy fmm fdd fhh trash <<< $(echo ${chunk_end_date}   | tr '_:T-' '    ')
#
#   Must WPS run or are the boundaries available?
#
set -v
if test "$(exist_wps $(date_wrf2iso ${chunk_start_date}))" -eq "1"; then
  wps_ran=0
  cd ${LOCALDIR}/WPS || exit
    vcp ${VCPDEBUG} ${WRF4G_DOMAINPATH}/${domain_name}/namelist.wps . || exit ${ERROR_VCP_FAILED}
  cd ${LOCALDIR}
  cd ${LOCALDIR}/WRFV3/run || exit
    setup_namelist_input 
    timelog_init "wps get"
      download_file wps $(date_wrf2iso ${chunk_start_date})
    timelog_end
  cd ${LOCALDIR}
else
  wps_ran=1
  cd ${LOCALDIR}/WPS || exit
    clean_wps
    #
    #   Get geo_em files and namelist.wps
    #
    vcp ${VCPDEBUG} ${WRF4G_DOMAINPATH}/${domain_name}/'*' . || exit ${ERROR_VCP_FAILED}
    #
    #   Modify the namelist
    #
    set +v
    fortnml_setn namelist.wps start_date ${max_dom} "'${chunk_start_date}'"
    fortnml_setn namelist.wps end_date   ${max_dom} "'${chunk_end_date}'"
    fortnml_set  namelist.wps interval_seconds      ${global_interval}
    fortnml_set  namelist.wps max_dom               ${max_dom}
    fortnml_set  namelist.wps prefix                "'${global_name}'"
    set -v
    #
    #   Preprocessor
    #
    timelog_init "get boundaries"
      if test -z "${global_preprocessor}"; then
        echo "Linking global data from: ${global_path}"
        mkdir -p grbData
        for yearmon in $(get_yearmons $iyy $imm $fyy $fmm) 
        do
          year=${yearmon:0:4}
          vcp ${VCPDEBUG} ${global_path}/${year}/'*'${yearmon}'*'.grb ln://`pwd`/grbData 
        done
      else
        preprocessor.${global_preprocessor} ${global_path} ${chunk_start_date} ${chunk_end_date}
      fi
      ./link_grib.csh grbData/*.grb
    timelog_end
    timelog_init "ungrib"
      ln -sf ungrib/Variable_Tables/Vtable.${global_name} Vtable
      ${ROOTDIR}/bin/ungrib.exe \
        >& ${logdir}/ungrib_${global_name}_${iyy}${imm}${idd}${ihh}.out \
        || wrf4g_exit ${ERROR_UNGRIB_FAILED}
      cat ${logdir}/ungrib_${global_name}_${iyy}${imm}${idd}${ihh}.out \
        | grep -q -i 'Successful completion of ungrib' \
        || wrf4g_exit ${ERROR_UNGRIB_FAILED}
    timelog_end
    #
    #   Check for other input namelists and apply them
    #
    for vtname in ${global_vtable_other}
    do
      fortnml --overwrite -f namelist.wps -s prefix@ungrib          ${vtname}
      fortnml --overwrite -f namelist.wps -s end_date               ${chunk_start_date}  # single time step!!
      fortnml --overwrite -f namelist.wps -a constants_name@metgrid ${vtname}:${iyy}-${imm}-${idd}_${ihh}
      ln -sf ungrib/Variable_Tables/Vtable.${vtname} Vtable
      rm -rf grbData/*.grb
      if ! which cdo; then
        thisdir=$(pwd)
        cd `cat ../rootdir`
          if test -e /software/ScientificLinux/4.6/etc/bashrc; then
            cp /oceano/gmeteo/WORK/MDM.UC/Apps/cdo.tar.gz .
          else
            vcp gsiftp://ce01.macc.unican.es:2812/oceano/gmeteo/WORK/MDM.UC/Apps/cdo.tar.gz .
          fi
          tar xzf cdo.tar.gz && rm cdo.tar.gz
        cd ${thisdir}
      fi
      cdo setdate,${iyy}-${imm}-${idd} FAKESOIL.grb grbData/FAKESOIL.grb
      ./link_grib.csh grbData/*.grb
      ${ROOTDIR}/bin/ungrib.exe >& ${logdir}/ungrib_${vtname}_${iyy}${imm}${idd}${ihh}.out || wrf4g_exit ${ERROR_UNGRIB_FAILED}
      cat ${logdir}/ungrib_${vtname}_${iyy}${imm}${idd}${ihh}.out \
        | grep -q -i 'Successful completion of ungrib' \
        || wrf4g_exit ${ERROR_UNGRIB_FAILED}
    done
    #
    #                     Fix fields
    #
    if [ -e Vtable.${global_name}FIX ]; then
      sed -e 's/@start_date@/'${syy}-${smm}-${sdd}_${shh}:00:00'/g' \
          -e 's/@end_date@/'${syy}-${smm}-${sdd}_${shh}:00:00'/g' \
          -e 's/@interval_seconds@/'${interval_seconds}'/g' \
          namelist.wps.in > namelist.wps.infix
      cat <<- End_of_nmlungrib > namelist.ungrib
	&ungrib
	 out_format = 'WPS'
	 prefix = '${global_name}FIX'
	/
	End_of_nmlungrib
      ln -sf Vtable.${global_name}FIX Vtable
      cpp -P namelist.wps.infix > namelist.wps
      ./ungrib/ungrib.exe >& ${logdir}/ungrib_${global_name}FIX_${iyy}${imm}${idd}${ihh}.out || exit 203
      mv ${global_name}FIX* ${global_name}FIX || exit 71
    fi
    #
    #   Run metgrid
    #
    timelog_init "metgrid"
      set +v
      fortnml_vardel namelist.wps opt_output_from_metgrid_path
      fortnml_vardel namelist.wps opt_output_from_geogrid_path
      fortnml_vardel namelist.wps opt_metgrid_tbl_path
      fortnml_vardel namelist.wps opt_geogrid_tbl_path
      fortnml_setn namelist.wps start_date ${max_dom} "'${chunk_start_date}'"
      fortnml_setn namelist.wps end_date   ${max_dom} "'${chunk_end_date}'"
      fortnml -o -f namelist.wps -s fg_name ${global_name}
      set -v
      ${LAUNCHER_METGRID} ${ROOTDIR}/bin/metgrid.exe \
        >& ${logdir}/metgrid_${iyy}${imm}${idd}${ihh}.out \
        || wrf4g_exit ${ERROR_METGRID_FAILED}
      cat ${logdir}/metgrid_${iyy}${imm}${idd}${ihh}.out \
        | grep -q -i 'Successful completion of metgrid' \
        || wrf4g_exit ${ERROR_METGRID_FAILED}
      # Clean
      if test "${clean_after_run}" -eq 1; then
        rm -f GRIBFILE.*
        rm -rf grbData
        rm -rf ${global_name}\:*
      fi
    timelog_end
  cd ${LOCALDIR}/WRFV3/run || exit
    #------------------------------------------------------------------
    #                              REAL
    #------------------------------------------------------------------
    timelog_init "real"
      clean_real
      ln -s ../../WPS/met_em.d??.????-??-??_??:00:00.nc .
      ls -l ########################################################## borrar
      fix_ptop
      setup_namelist_input
      ${LAUNCHER_REAL} ${ROOTDIR}/bin/real.exe \
        >& ${logdir}/real_${iyy}${imm}${idd}${ihh}.out \
        || wrf4g_exit ${ERROR_REAL_FAILED}
      cat ${logdir}/real_${iyy}${imm}${idd}${ihh}.out \
        | grep -q -i 'SUCCESS COMPLETE REAL_EM' \
        || wrf4g_exit ${ERROR_REAL_FAILED}
      # Clean
      if test -e rsl.out.0000; then
        mkdir -p rsl_real
        mv rsl.* rsl_real/
        mv rsl_real ${logdir}/
      fi
      if test "${clean_after_run}" -eq 1; then
        rm -f met_em*
        rm -f ../../WPS/met_em*
      fi
    timelog_end
    #
    #  Upload the wpsout files (create the output structure if necessary):
    #
    #    wrfinput_d0?
    #    wrfbdy_d0?
    #    wrflowinp_d0?
    #    wrffdda_d0?
    #
    if test "${save_wps}" -eq 1; then
      timelog_init "wps put"
        create_output_structure
        ${ROOTDIR}/WRFGEL/post_and_register --no-bg wps "${chunk_start_date}"
      timelog_end
    fi
  cd ${LOCALDIR} || exit
fi

#------------------------------------------------------------------
#                              WRF
#------------------------------------------------------------------
cd ${LOCALDIR}/WRFV3/run || exit
  if test -n "${icbcprocessor}"; then
    timelog_init "icbcprocessor"
      icbcprocessor.${icbcprocessor} >&  ${logdir}/icbcproc_${ryy}${rmm}${rdd}${rhh}.out
    timelog_end
  fi
  timelog_init "wrf"
    ls -l ########################################################## borrar
    sleep 1
    echo "${LAUNCHER_WRF} ${ROOTDIR}/bin/wrf_wrapper.exe >& ${logdir}/wrf_${ryy}${rmm}${rdd}${rhh}.out"
    ls -la ${LAUNCHER_WRF}
    ls -la ${ROOTDIR}/bin/wrf_wrapper.exe
    ls -la ${logdir}
    ${LAUNCHER_WRF} ${ROOTDIR}/bin/wrf_wrapper.exe >& ${logdir}/wrf_${ryy}${rmm}${rdd}${rhh}.out &
    # Wait enough time to allow 'wrf_wrapper.exe' create 'wrf.pid'
    # This time is also useful to  to copy the wpsout data
    sleep 10 
    ps -ef | grep wrf.exe
    ${ROOTDIR}/WRFGEL/wrf4g_monitor $(cat wrf.pid) >& ${logdir}/monitor.log &
    echo $! > monitor.pid   
    wait $(cat monitor.pid)
  timelog_end
  # Clean the heavy stuff
  if test "${clean_after_run}" -eq 1; then
    rm -f CAM_ABS_DATA wrf[bli]* ${ROOTDIR}/bin/real.exe ${ROOTDIR}/bin/wrf.exe \
        ${ROOTDIR}/bin/metgrid.exe ${ROOTDIR}/bin/ungrib.exe
  fi
  wrf4g_exit 0
cd ${LOCALDIR}
