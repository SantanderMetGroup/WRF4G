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
# Running WRF
#
ulimit -s unlimited

logdir=${ROOTDIR}/log; mkdir -p ${logdir}
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

function clean_rsl(){
  rm -f rsl.error.* rsl.out.*
}

function clean_wps(){
  rm -f GRIBFILE.[A-Z][A-Z][A-Z]      # Links to grib files
  rm -f ${global_name}*\:????-??-??_??   # Intermediate files
  rm -f ${global_name}FIX                # Intermediate files
  rm -f met_em.*                      # metgrid files
}

function clean_real(){
  rm -f met_em.d??.????-??-??_??:00:00.nc
  rm -f namelist.wps
}

timelog_clean
#
#  Get the 'a-priori' start (i**) and end (f**) dates for this chunk
#
read iyy imm idd ihh trash <<< $(echo ${chunk_start_date} | tr '_:T-' '    ')
read fyy fmm fdd fhh trash <<< $(echo ${chunk_end_date}   | tr '_:T-' '    ')

cd WPS || exit
  #
  #   Must WPS run or are the boundaries available?
  #
  ####### TODO
  clean_wps
  #
  #   Get geo_em files and namelist.wps
  #
  vcp -r ${WRF4G_DOMAINPATH}/${domain_name}/'*' . 
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
      vcp ${global_path}/${year}/'*'${yearmon}'*'.grb ln://`pwd`/grbData 
      #ln -s ${global_path}/${year}/*${yearmon}*.grb grbData/ 
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
    fortnml_vardel namelist.wps opt_metgrid_tbl_path
    fortnml_set  namelist.wps fg_name               "'${global_name}'"
    fortnml_setn namelist.wps start_date ${max_dom} "'${chunk_start_date}'"
    fortnml_setn namelist.wps end_date   ${max_dom} "'${chunk_end_date}'"
    ./metgrid/metgrid.exe >& ${logdir}/metgrid_${iyy}${imm}${idd}${ihh}.out || exit ${ERROR_METGRID_FAILED}
  timelog_end
cd ${ROOTDIR}/WRFV3/run || exit
  #------------------------------------------------------------------
  #                              WRF
  #------------------------------------------------------------------
  timelog_init "real"
    clean_real
    ln -s ../../WPS/met_em.d??.????-??-??_??:00:00.nc .
    fix_ptop
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

    $launcher ./real.exe >& ${logdir}/real_${iyy}${imm}${idd}${ihh}.out || exit ${ERROR_REAL_FAILED}
#    if [ "$is_restart" = ".true." ]; then
#      fortnml_set namelist.input restart .true.
#    fi
  timelog_end
  timelog_init "wrf"
    $launcher ./wrf.exe >& ${logdir}/wrf_${iyy}${imm}${idd}${ihh}.out || exit ${ERROR_WRF_FAILED}
  timelog_end
cd ${ROOTDIR}
