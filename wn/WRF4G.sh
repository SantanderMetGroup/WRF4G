#! /bin/bash
#
# WRF4G.sh
#
ROOTDIR=$(pwd)
#
#  Load functions and set the PATH
#
source ${ROOTDIR}/lib/bash/wrf_util.sh
source ${ROOTDIR}/lib/bash/wrf4g_exit_codes.sh
export PATH="${ROOTDIR}/bin:$PATH"
#
#  Load wrf4g.conf, wrf.chunk and wrf.input
#
source wrf4g.conf                                  || exit ${ERROR_MISSING_WRF4GCNF}
sed -e 's/\ *=\ */=/' wrf.chunk > source.it        || exit ${ERROR_MISSING_WRFCHUNK}
source source.it && rm source.it
sed -e 's/\ *=\ */=/' wrf.input > source.it        || exit ${ERROR_MISSING_WRFINPUT}
source source.it && rm source.it
#
#  Export variables
#
export WRF4G_CONF_FILE="${ROOTDIR}/wrf4g.conf"
export WRF4G_EXPERIMENT="${experiment_name}"
export WRF4G_REALIZATION="${realization_name}"
export WRFGEL_SCRIPT="${ROOTDIR}/WRFGEL/register_file"
#
# Running WRF
#
ulimit -s unlimited

function setup_namelist_input(){
  ln -s ../../WPS/namelist.wps
  fortnml_set    namelist.input run_days    0
  fortnml_set    namelist.input run_hours   0
  fortnml_set    namelist.input run_minutes 0
  fortnml_set    namelist.input run_seconds 0
  fortnml_setn    namelist.input start_year  ${max_dom} ${iyy}
  fortnml_setn    namelist.input start_month ${max_dom} ${imm}
  fortnml_setn    namelist.input start_day   ${max_dom} ${idd}
  fortnml_setn    namelist.input start_hour  ${max_dom} ${ihh}
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
  fortnml_set     namelist.input num_metgrid_levels $(get_num_metgrid_levels)
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
  fortnml_setm namelist.input dy        $alldx
  fortnml_set  namelist.input time_step $(get_timestep $dx)
  fortnml_set  namelist.input max_dom   ${max_dom}
}

logdir=${ROOTDIR}/log; mkdir -p ${logdir}
VCPDEBUG="-v"

function timelog_clean(){
  rm -f ${logdir}/time.log
}

function timelog_end(){
  date +%Y%m%d%H%M%S >> ${logdir}/time.log
}

function timelog_init(){
  item=$1
  echo -n "$(printf "%20s" "$item") $(date +%Y%m%d%H%M%S) " >> ${logdir}/time.log
}

timelog_clean
#
#  Get the 'a-priori' start (i**) and end (f**) dates for this chunk
#
read iyy imm idd ihh trash <<< $(echo ${chunk_start_date} | tr '_:T-' '    ')
read fyy fmm fdd fhh trash <<< $(echo ${chunk_end_date}   | tr '_:T-' '    ')
#
#   Must WPS run or are the boundaries available?
#
#if test "$(exists_wps $(date_wrf2iso ${chunk_start_date}))" -eq "1"; then
if false; then  #  exists_wps is not yet ready...
  wps_ran=0
  cd ${ROOTDIR}/WRFV3/run || exit
    ${ROOTDIR}/WRFGEL/download_file wps $(date_wrf2iso ${chunk_start_date})
  cd ${ROOTDIR}
else
  wps_ran=1
  cd WPS || exit
    clean_wps
    #
    #   Get geo_em files and namelist.wps
    #
    vcp ${VCPDEBUG} ${WRF4G_DOMAINPATH}/${domain_name}/'*' . || exit ${ERROR_VCP_FAILED}
    #
    #   Modify the namelist
    #
    fortnml_setn namelist.wps start_date ${max_dom} "'${chunk_start_date}'"
    fortnml_setn namelist.wps end_date   ${max_dom} "'${chunk_end_date}'"
    fortnml_set  namelist.wps interval_seconds      ${global_interval}
    fortnml_set  namelist.wps max_dom               ${max_dom}
    fortnml_set  namelist.wps prefix                "'${global_name}'"
    #
    #   Preprocessor
    #
    timelog_init "get boundaries"
      echo "Linking global data from: ${global_path}"
      mkdir -p grbData
      for yearmon in $(get_yearmons $iyy $imm $fyy $fmm) 
      do
        year=${yearmon:0:4}
        vcp ${VCPDEBUG} ${global_path}/${year}/'*'${yearmon}'*'.grb ln://`pwd`/grbData 
      done
      ./link_grib.csh grbData/*.grb
    timelog_end
    timelog_init "ungrib"
      ln -sf ungrib/Variable_Tables_WRF4G/Vtable.${global_name} Vtable
      ./ungrib/ungrib.exe >& ${logdir}/ungrib_${global_name}_${iyy}${imm}${idd}${ihh}.out || exit ${ERROR_UNGRIB_FAILED}
    timelog_end
    #
    #   Check for other input namelists and apply them
    #   TODO: This is not working in wrf4G
    #
    for ext in TSK SST SFC
    do
      if [ -e Vtable.${global_name}${ext} ]; then
        cat <<- End_of_nmlungrib > namelist.ungrib
	&ungrib
	 out_format = 'WPS'
	 prefix = '${global_name}${ext}'
	/
	End_of_nmlungrib
        ln -sf Vtable.${global_name}${ext} Vtable
        cpp -P namelist.wps.in1 > namelist.wps
        ./ungrib/ungrib.exe >& ${logdir}/ungrib_${global_name}${ext}.out || exit 202
      fi
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
      fortnml_vardel namelist.wps opt_output_from_metgrid_path
      fortnml_vardel namelist.wps opt_output_from_geogrid_path
      fortnml_vardel namelist.wps opt_metgrid_tbl_path
      fortnml_vardel namelist.wps opt_geogrid_tbl_path
      fortnml_set  namelist.wps fg_name               "'${global_name}'"
      fortnml_setn namelist.wps start_date ${max_dom} "'${chunk_start_date}'"
      fortnml_setn namelist.wps end_date   ${max_dom} "'${chunk_end_date}'"
      ./metgrid/metgrid.exe >& ${logdir}/metgrid_${iyy}${imm}${idd}${ihh}.out || exit ${ERROR_METGRID_FAILED}
    timelog_end
  cd ${ROOTDIR}/WRFV3/run || exit
    #------------------------------------------------------------------
    #                              REAL
    #------------------------------------------------------------------
    timelog_init "real"
      clean_real
      ln -s ../../WPS/met_em.d??.????-??-??_??:00:00.nc .
      fix_ptop
      setup_namelist_input
      ${LAUNCHER_REAL} ./real.exe >& ${logdir}/real_${iyy}${imm}${idd}${ihh}.out || exit ${ERROR_REAL_FAILED}
    timelog_end
    #
    #  Upload the wpsout files (create the output structure if necessary):
    #
    #    wrfinput_d0?
    #    wrfbdy_d0?
    #    wrflowinp_d0?
    #
    ${ROOTDIR}/WRFGEL/create_output_structure
    ${WRFGEL_SCRIPT} wps "${chunk_start_date}"
  cd ${ROOTDIR} || exit
fi

#------------------------------------------------------------------
#                              WRF
#------------------------------------------------------------------
cd ${ROOTDIR}/WRFV3/run || exit
  if test ${wps_ran} -eq 0; then
    restart_date=$(${ROOTDIR}/WRFGEL/get_date_restart)
    if test "${restart_date}" = "-1"; then
      if test "${chunk_is_restart}" = ".T."; then
        echo "Something went wrong! (the restart file is not available and the chunk is a restart...)"
        exit ${EXIT_RESTART_MISMATCH}
      fi
      fortnml_set namelist.input restart ${chunk_is_restart}
    elif test "$(date2int ${restart_date})" -ge "$(date2int ${chunk_end_date})"; then
      exit ${EXIT_CHUNK_ALREADY_FINISHED}
    elif test "$(date2int ${restart_date})" -lt "$(date2int ${chunk_start_date})"; then
      exit ${EXIT_CHUNK_CANNOT_RUN}
    else
      timelog_init "rst download"
        ${ROOTDIR}/WRFGEL/download_file rst ${restart_date} || exit ${ERROR_RST_DOWNLOAD_FAILED}
        read iyy imm idd ihh trash <<< $(date_iso2wrf ${restart_date} | tr '_:T-' '    ')
        fortnml_set namelist.input restart .T.
      timelog_end
    fi
    #
    #  Set the start and end dates in the namelist
    #
    setup_namelist_input 
  else
    if test "${chunk_is_restart}" = ".T."; then
      ${ROOTDIR}/WRFGEL/download_file rst $(date_wrf2iso ${chunk_start_date}) || exit ${ERROR_RST_DOWNLOAD_FAILED}
    fi
    fortnml_set namelist.input restart ${chunk_is_restart}
  fi
  timelog_init "wrf"
    ${LAUNCHER_WRF} ./wrf.exe >& ${logdir}/wrf_${iyy}${imm}${idd}${ihh}.out || exit ${ERROR_WRF_FAILED}
  timelog_end
cd ${ROOTDIR}
