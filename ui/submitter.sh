rootsh=`pwd`

function qsub(){
  echo $RANDOM
} 
#
#  Load wrf.input
#
sed -e 's/\ *=\ */=/' wrf.input > source.it        || exit ${ERROR_MISSING_WRFINPUT}
source source.it && rm source.it
source scheduler_headers/sched_headers.${job_type} || exit ${ERROR_MISSING_HEADERS}
source ${foresthome}/bats/wrf_util.sh              || exit ${ERROR_MISSING_WRFUTIL}

echo "0000" > ${rootsh}/DATErun.inf
daterun=`cat ${rootsh}/DATErun.inf`
if test $daterun = '0000'; then
  daterun=$start_date
  echo $experiment_name > ${rootsh}/dates.inf 
fi
#
#  Preexisting simulated periods
#
function exists_experiment(){
  expname=$1
  test -e ${rootsh}/Iteration_state.inf
}

function get_experiment_status(){
  expname=$1
  datefiter=`(tail -n 1 ${rootsh}/Iteration_state.inf | awk '{print $3}')`
  if test $datefiter = '@endWRF.exe@'; then
    echo ${STATUS_BROKEN_CHUNK}
  else
    echo ${STATUS_FULL_CHUNKS}
  fi
}

function get_restart_date(){
  expname=$1
  dateiiter=`(tail -n 1 ${rootsh}/Iteration_state.inf | awk '{print $2}')`
  datefiter=`(tail -n 1 ${rootsh}/Iteration_state.inf | awk '{print $3}')`
  if test $datefiter = '@endWRF.exe@'
  then
    daterun=$dateiiter
  else
    dateicorrect=$dateiiter
    datefcorrect=$datefiter
    dateiiterYMD=`(expr substr $dateiiter 1 8)`
    hoursrun=`(expr substr $dateiiter 9 2)`
    hoursnext=`(expr $hoursrun + $simulation_interval_h)`
    daterun=`(date +%Y-%m-%d_%H:%M -u -d"$dateiiterYMD $hoursnext hours")`
  fi
  echo $daterun
}

function independent_chunks(){
  test "${is_continuous}" -eq 0
}

function get_nin_vars(){
  set | grep '^NIN_' | sed -e 's/^NIN_\(.*\)=.*$/\1/'
}

function get_nim_vars(){
  set | grep '^NIM_' | sed -e 's/^NIM_\(.*\)=.*$/\1/'
}


function cycle(){
  realization_name=$1
  if $(exists_experiment ${realization_name}); then
    expst=$(get_experiment_status ${realization_name})
    case ${expst} in
      ${STATUS_DONE})    exit ${EXIT_SIMULATION_FINISHED} ;;
      ${STATUS_RUNNING}) exit ${EXIT_SIMULATION_ALREADY_RUNNING} ;;
      ${STATUS_BROKEN_CHUNK})
        daterun=$(get_restart_date ${realization_name})
        ;;
      ${STATUS_FULL_CHUNKS})
        daterun=$(get_restart_date ${realization_name})
        ;;
      *) exit ${EXIT_UNKNOWN_EXPERIMENT_STATUS} ;;
    esac
  else
    daterun=${start_date}
  fi
  
  read syy smm sdd shh trash <<< $(echo ${start_date} | tr '_:T-' '    ')
  read eyy emm edd ehh trash <<< $(echo ${end_date} | tr '_:T-' '    ')
  
  TOTsecSIM=$(seconds_between_dates ${start_date} ${end_date})
  simulation_interval_s=$(expr $simulation_interval_h '*' 3600)
  
  total_chunks=`expr $TOTsecSIM / $simulation_interval_s`
  total_chunks=`expr $total_chunks + 1`
  
  echo "Total number of simulations: "$total_chunks
  if test "$is_restart" -ne "0"
  then
    echo "RESTART simulation RESTART simulation RESTART simulation"
  fi
  read iyy imm idd ihh trash <<< $(echo ${daterun} | tr '_:T-' '    ')
  while test ${iyy}${imm}${idd}${ihh} -le ${eyy}${emm}${edd}${ehh}
  do
    hours=`(expr ${ihh} + ${simulation_length_h})`
    final_date=`(date +%Y-%m-%d_%H:%M -u -d"${iyy}${imm}${idd} $hours hours")`
    read fyy fmm fdd fhh trash <<< $(echo ${final_date} | tr '_:T-' '    ')
    
    echo $daterun" "${final_date} >> ${rootsh}/dates.inf
    #
    #  Restart issues
    #
    if independent_chunks; then
      fortnml_set namelist.input restart .F.
    else
      fortnml_set namelist.input restart .T.
    fi
    if test "${iyy}${imm}${idd}${ihh}" -eq "${syy}${smm}${sdd}${shh}"; then
      fortnml_set namelist.input restart .F. 
    fi
    #
    # Writting namelists 
    #
    fortnml_setn namelist.wps   start_date  ${max_dom} "'${daterun}'" 
    fortnml_setn namelist.wps   end_date    ${max_dom} "'${final_date}'" 
    fortnml_setn namelist.input start_year  ${max_dom} ${iyy} 
    fortnml_setn namelist.input start_month ${max_dom} ${imm} 
    fortnml_setn namelist.input start_day   ${max_dom} ${idd} 
    fortnml_setn namelist.input start_hour  ${max_dom} ${ihh} 
    fortnml_setn namelist.input end_year    ${max_dom} ${fyy} 
    fortnml_setn namelist.input end_month   ${max_dom} ${fmm} 
    fortnml_setn namelist.input end_day     ${max_dom} ${fdd} 
    fortnml_setn namelist.input end_hour    ${max_dom} ${fhh} 
    
    mkdir -p $run_path || exit ${ERROR_RUNHOME_NOT_WRITABLE}
    
    wps_phase_path="${jobs_path}/${realization_name}/${iyy}${imm}${idd}${ihh}_${fyy}${fmm}${fdd}${fhh}_WPS"
    mkdir -p ${wps_phase_path}
    cp namelist.input ${wps_phase_path}/
    cp namelist.wps   ${wps_phase_path}/
    echo "wrf_bit=6" > ${wps_phase_path}/deck.input
  
    wrf_phase_path="${jobs_path}/${realization_name}/${iyy}${imm}${idd}${ihh}_${fyy}${fmm}${fdd}${fhh}_WRF"
    mkdir -p ${wrf_phase_path}
    cp namelist.input ${wrf_phase_path}/
    cp namelist.wps   ${wrf_phase_path}/
    echo "wrf_bit=8" > ${wrf_phase_path}/deck.input
    #
    #  WPS Job
    #
    cd ${wps_phase_path}
      wait_ids=${lastwpsid}                                 # Build the submitter
      np=2 # ${number_of_nodes}                             # Send real.exe to 2 nodes maximum (slows down otherwise)
      ppn=${processes_per_node}
      job_name=${realization_name}
      if test "${iyy}${imm}${idd}${ihh}" -eq "${syy}${smm}${sdd}${shh}" -o independent_chunks; then
        # First chunk, no deps   ALSO   independent chunks, no deps
        SUBMITTER=$(eval echo ${ASYNC_SUBMITTER_TPL}) 
      else
        if test -n "${beforelastwrfid}";then
          # Avoid running too fast, wait for the wrf jobs to finish.
          wait_ids="${wait_ids},${beforelastwrfid}"
        fi
        SUBMITTER=$(eval echo ${SUBMITTER_TPL})
      fi
      echo "np=${np}" >> deck.input                         # Save values for the launcher
      echo "ppn=${ppn}" >> deck.input
      wpsid=$(${SUBMITTER} ${rootsh}/wrf.deck)  
    cd ${rootsh}
    #
    #  WRF Job
    #
    cd ${wrf_phase_path}
      wait_ids=${wpsid}                                     # Build the submitter
      np=${number_of_nodes}
      ppn=${processes_per_node}
      job_name=${realization_name}
      if test -n "${lastwrfid}"; then
        if test ! independent_chunks; then
          # Wait for the previous wrf job (if there was one...)
          wait_ids="${wait_ids},${lastwrfid}"
        fi
      fi
      SUBMITTER=$(eval echo ${SUBMITTER_TPL})
      echo "np=${np}" >> deck.input                         # Save values for the launcher
      echo "ppn=${ppn}" >> deck.input
      echo "wrfinput_path=${rootsh}/wrf.input" \
        | cat ${rootsh}/scheduler_headers/sched_headers.${job_type} - ${rootsh}/wrf.deck \
        > wrf.deck
      wrfid=$(${SUBMITTER} ./wrf.deck)              # submit wrf.deck and get the job ID
    cd ${rootsh}
    echo $daterun" "${final_date} >> ${rootsh}/dates.inf
    #
    #   Cycle dates
    #
    hoursnext=`(expr ${ihh} + ${simulation_interval_h})`
    daterun=`(date +%Y-%m-%d_%H:%M -u -d"${iyy}${imm}${idd} $hoursnext hours")`
    echo $daterun > ${rootsh}/DATErun.inf
    read iyy imm idd ihh trash <<< $(echo ${daterun} | tr '_:T-' '    ')
    #
    #  Cycle job ids
    #
    beforelastwrfid=${lastwrfid}
    lastwrfid=${wrfid}
    beforelastwpsid=${lastwpsid}
    lastwpsid=${wpsid}
  done
}
#
#  Initial override of namelist values
#
cp ${WRFhome}/run/namelist.input namelist.input.base
cp ${WPShome}/namelist.wps       namelist.wps.base
for var in $(get_nim_vars); do
  fortnml_setm namelist.input.base $var $(eval echo \$NIM_${var})
done
for var in $(get_nin_vars); do
  fortnml_setn namelist.input.base $var $max_dom $(eval echo \$NIN_${var})
done
#
#  Multiphysics support. Physical parameters overwritten!
#
if test "${is_multiphysics}" -ne "0"; then
  for mpid in $(echo ${multiphysics_combinations} | tr '/' ' '); do
    cp namelist.input.base namelist.input
    cp namelist.wps.base   namelist.wps
    iphys=1
    for var in $(echo ${multiphysics_variables} | tr ',' ' '); do
      fortnml_setn namelist.input $var $max_dom $(tuple_item ${mpid} ${iphys})
      let iphys++
    done
    cycle "${experiment_name}/${mpid}"
  done
else
  cp namelist.input.base namelist.input
  cp namelist.wps.base   namelist.wps
  cycle "${experiment_name}" ${is_continuous}
fi
