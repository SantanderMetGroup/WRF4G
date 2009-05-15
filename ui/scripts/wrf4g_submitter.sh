#! /bin/bash
#
# wrf4g_submitter
#
# function get_nin_vars()
# function get_nim_vars()
# function cycle_chunks(ALGO_name, ALGO_start_date, ALGO_end_date)
# function cycle_hindcasts(realization_name, start_date, end_date)
# function cycle_time(realization_name, start_date, end_date)
#
userdir=`pwd`
wrf4g_root=$(dirname $(dirname $(dirname $0)))
#
#  Load wrf.input et al.
#
sed -e 's/\ *=\ */=/' wrf4g.conf > source.it        || exit ${ERROR_MISSING_WRF4GCNF}
source source.it && rm source.it
sed -e 's/\ *=\ */=/' wrf.input > source.it        || exit ${ERROR_MISSING_WRFINPUT}
source source.it && rm source.it
#source ${wrf4g_root}/scheduler_headers/sched_headers.${job_type} || exit ${ERROR_MISSING_HEADERS}
source ${wrf4g_root}/wn/lib/bash/wrf_util.sh       || exit ${ERROR_MISSING_WRFUTIL}

function get_nin_vars(){
  set | grep '^NIN_' | sed -e 's/^NIN_\(.*\)=.*$/\1/'
}

function get_nim_vars(){
  set | grep '^NIM_' | sed -e 's/^NIM_\(.*\)=.*$/\1/'
}

function cycle_chunks(){
  ALGO_name=$1
  ALGO_start_date=$2
  ALGO_end_date=$3
  local eyy emm edd ehh 
  local cyy cmm cdd chh current_date
  local fyy fmm fdd fhh final_date
  echo "---> cycle_chunks: ${ALGO_name} ${ALGO_start_date} ${ALGO_end_date}"
  #
  mkdir -p ALGOs/${ALGO_name}
  current_date=${ALGO_start_date}
  read cyy cmm cdd chh trash <<< $(echo ${current_date} | tr '_:T-' '    ')
  read eyy emm edd ehh trash <<< $(echo ${ALGO_end_date} | tr '_:T-' '    ')
  chunkno=1
  while test ${cyy}${cmm}${cdd}${chh} -lt ${eyy}${emm}${edd}${ehh}
  do
    chunkdir="${userdir}/ALGOs/${ALGO_name}/$(printf "%04d" ${chunkno})"
    mkdir -p ${chunkdir}
    hours=`(expr ${chh} + ${chunk_size_h})`
    final_date=`(date +%Y-%m-%d_%H:%M:%S -u -d"${cyy}${cmm}${cdd} $hours hours")`
    read fyy fmm fdd fhh trash <<< $(echo ${final_date} | tr '_:T-' '    ')
    if test ${fyy}${fmm}${fdd}${fhh} -gt ${eyy}${emm}${edd}${ehh}; then
      final_date=${ALGO_end_date}
      read fyy fmm fdd fhh trash <<< $(echo ${final_date} | tr '_:T-' '    ')
    fi
    echo "  ---> chunk: ${chunkno} - ${current_date} -> ${final_date}"
    cat << EOF > ${chunkdir}/wrf.chunk
realization_name="${ALGO_name}"
chunk_start_date="${current_date}"
chunk_end_date="${final_date}"
EOF
    #
    #  Create the sandbox file
    #
    if test ${chunkno} -eq 1; then
      fortnml_set namelist.input restart .F.
    else
      fortnml_set namelist.input restart .T.
    fi
    mkdir -p ${chunkdir}/WRFV3/run
    ln -s ${userdir}/namelist.input ${chunkdir}/WRFV3/run/namelist.input
    ln -s ${userdir}/wrf.input      ${chunkdir}/wrf.input
    ln -s ${userdir}/wrf4g.conf     ${chunkdir}/wrf4g.conf
    mkdir ${chunkdir}/bin
    ln -s ${wrf4g_root}/wn/bin/vcp  ${chunkdir}/bin/vcp
    ln -s ${wrf4g_root}/wn/WRF4G.sh ${chunkdir}/WRF4G.sh
    cd ${chunkdir}
      tar czhf sandbox.tar.gz *
      #
      #   Submit the job
      #
      ln -s ${wrf4g_root}/ui/scripts/WRF4G_ini.sh .
      chunkjid=$(${wrf4g_root}/ui/scripts/wrf4g_submit.${JOB_TYPE} WRF4G_ini.sh ${chunkjid})
    cd ${userdir}
    #
    #  Cycle dates and jobids
    #
    current_date=${final_date}
    read cyy cmm cdd chh trash <<< $(echo ${current_date} | tr '_:T-' '    ')
    let chunkno++
  done
}

function cycle_hindcasts(){
  realization_name=$1
  start_date=$2
  end_date=$3
  echo "---> cycle_hindcasts: $1 $2 $3"
  current_date=${start_date}
  read cyy cmm cdd chh trash <<< $(echo ${current_date} | tr '_:T-' '    ')
  read eyy emm edd ehh trash <<< $(echo ${end_date} | tr '_:T-' '    ')
  hours=`(expr ${chh} + ${simulation_length_h})`
  final_date=`(date +%Y-%m-%d_%H:%M:%S -u -d"${cyy}${cmm}${cdd} $hours hours")`
  read fyy fmm fdd fhh trash <<< $(echo ${final_date} | tr '_:T-' '    ')
  while test ${cyy}${cmm}${cdd}${chh} -le ${eyy}${emm}${edd}${ehh}
  do
    hours=`(expr ${chh} + ${simulation_length_h})`
    final_date=`(date +%Y-%m-%d_%H:%M:%S -u -d"${cyy}${cmm}${cdd} $hours hours")`
    read fyy fmm fdd fhh trash <<< $(echo ${final_date} | tr '_:T-' '    ')
    if test ${fyy}${fmm}${fdd}${fhh} -gt ${eyy}${emm}${edd}${ehh}; then
      final_date=${end_date}
      read fyy fmm fdd fhh trash <<< $(echo ${final_date} | tr '_:T-' '    ')
    fi
    cycle_chunks ${realization_name}__${cyy}${cmm}${cdd}${chh}_${fyy}${fmm}${fdd}${fhh} ${current_date} ${final_date}
    #
    #  Cycle dates
    #
    hoursnext=`(expr ${chh} + ${simulation_interval_h})`
    current_date=`(date +%Y-%m-%d_%H:%M:%S -u -d"${cyy}${cmm}${cdd} $hoursnext hours")`
    read cyy cmm cdd chh trash <<< $(echo ${current_date} | tr '_:T-' '    ')
  done
}

function cycle_time(){
  realization_name=$1
  start_date=$2
  end_date=$3
  case ${is_continuous} in
    0)
      echo "---> Hindcast run"
      cycle_hindcasts ${realization_name} ${start_date} ${end_date}
      ;;
    1)
      echo "---> Continuous run"
      cycle_chunks ${realization_name} ${start_date} ${end_date}
      ;;
  esac
}

#
#  Initial override of namelist values
#
cp ${userdir}/namelist.input ${userdir}/namelist.input.base
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
  echo "---> Multi-physics run"
  for mpid in $(echo ${multiphysics_combinations} | tr '/' ' '); do
    cp namelist.input.base namelist.input
    iphys=1
    for var in $(echo ${multiphysics_variables} | tr ',' ' '); do
      fortnml_setn namelist.input $var $max_dom $(tuple_item ${mpid} ${iphys})
      let iphys++
    done
    cycle_time "${experiment_name}__${mpid/,/_}" ${start_date} ${end_date}
  done
else
  echo "---> Single physics run"
  cp namelist.input.base namelist.input
  cycle_time "${experiment_name}" ${start_date} ${end_date}
fi
