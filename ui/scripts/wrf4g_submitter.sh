#! /bin/bash
#
# wrf4g_submitter
#
# function get_ni_vars()
# function get_nin_vars()
# function get_nim_vars()
# function cycle_chunks(realization_name, realization_start_date, realization_end_date)
# function cycle_hindcasts(realization_name, start_date, end_date)
# function cycle_time(realization_name, start_date, end_date)
#
isdry="no"
justone="no"
owforce="no"
waitsec=0
if test ${#} -ge 1; then
  while test "$*"; do
    case $1 in
      --dry-run)
          isdry="yes"
        ;;
      --run-just-one)
          justone="yes"
        ;;
      --force)
          owforce="yes"
        ;;
      --wait)
          waitsec=$2; shift
        ;;
      *)
        echo "Unknown argument: $1"
        exit
        ;;
    esac
    shift
  done
fi

userdir=`pwd`
wrf4g_root=$(dirname $(dirname $(dirname $0)))
export PATH="${wrf4g_root}/wn/bin:${PATH}"
#
#  Load wrf.input et al.
#
sed -e 's/\ *=\ */=/' wrf4g.conf > source.it       || exit ${ERROR_MISSING_WRF4GCNF}
source source.it && rm source.it
sed -e 's/\ *=\ */=/' wrf.input > source.it        || exit ${ERROR_MISSING_WRFINPUT}
source source.it && rm source.it
source ${wrf4g_root}/wn/lib/bash/wrf_util.sh       || exit ${ERROR_MISSING_WRFUTIL}
#
#  export some variables
#
export WRF4G_CONF_FILE="${userdir}/wrf4g.conf"
export WRF4G_EXPERIMENT="${experiment_name}"

function get_ni_vars(){
  set | grep '^NI_' | sed -e 's/^NI_\(.*\)=.*$/\1/'
}

function get_nin_vars(){
  set | grep '^NIN_' | sed -e 's/^NIN_\(.*\)=.*$/\1/'
}

function get_nim_vars(){
  set | grep '^NIM_' | sed -e 's/^NIM_\(.*\)=.*$/\1/'
}

function is_dry_run(){
  test "${isdry}" = "yes"
}

function if_not_dry(){
  comando=$*
  if ! is_dry_run; then
    $comando
  fi
}

function rematch(){
  pattern=$1
  string=$2
  if test "${pattern:0:10}" = "from_file:"; then
    grep -q "$string" $(echo ${pattern} | sed -e 's/from_file://')
  else
    echo "$string" | grep -Eq "^${pattern}$"
  fi
}

function cycle_chunks(){
  local realization_name
  local eyy emm edd ehh 
  local cyy cmm cdd chh current_date
  local fyy fmm fdd fhh final_date
  realization_name=$1
  export WRF4G_REALIZATION="${realization_name}"
  realization_start_date=$2
  realization_end_date=$3
  if_not_dry echo "---> cycle_chunks: ${realization_name} ${realization_start_date} ${realization_end_date}"
  #
  if test -d realizations/${realization_name}; then
    if test ${owforce} = "yes"; then
      yon="y"
    else
      echo ""
      echo "    >>>   THE REALIZATION ALREADY EXISTS!!   <<<"
      echo ""
      echo -n "Are you sure you want to send this experiment again? [y/N] "
      read yon
    fi
    test "${yon}" = "y" || exit
    tag=$(stat -c %y realizations | cut -c-19 | tr -d ': -')
    mv realizations realizations.${tag}
  fi
  current_date=${realization_start_date}
  read cyy cmm cdd chh trash <<< $(echo ${current_date} | tr '_:T-' '    ')
  read eyy emm edd ehh trash <<< $(echo ${realization_end_date} | tr '_:T-' '    ')
  chunkno=1
  chunkjid=""
  while test ${cyy}${cmm}${cdd}${chh} -lt ${eyy}${emm}${edd}${ehh}
  do
    export WRF4G_CHUNK="$(printf '%04d' ${chunkno})"
    let hours=chh+chunk_size_h
    final_date=$(date +%Y-%m-%d_%H:%M:%S -u -d"${cyy}${cmm}${cdd} $hours hours")
    read fyy fmm fdd fhh trash <<< $(echo ${final_date} | tr '_:T-' '    ')
    if test ${fyy}${fmm}${fdd}${fhh} -gt ${eyy}${emm}${edd}${ehh}; then
      final_date=${realization_end_date}
      read fyy fmm fdd fhh trash <<< $(echo ${final_date} | tr '_:T-' '    ')
    fi
    if test ${is_restart} -eq 0 || 
      (test ${is_restart} -eq 1 && rematch "${rst_realization}" "${realization_name}" && rematch "${rst_chunk}" "${chunkno}" ; ) ; then
      if is_dry_run; then
        echo "  ${realization_name}  $(printf "%4d" ${chunkno})  ${current_date}  ${final_date}"
      else
        chunkdir="${userdir}/realizations/${realization_name}/$(printf '%04d' ${chunkno})"
        mkdir -p ${chunkdir}
        echo "  ---> chunk: ${chunkno} - ${current_date} -> ${final_date}"
        test ${chunkno} -eq 1 && restart_flag=".F." || restart_flag=".T."
        #
        #  Create the sandbox file
        #
        cat << EOF > ${chunkdir}/wrf.chunk
realization_name="${realization_name}"
chunk_name="$(printf "%04d" ${chunkno})"
chunk_start_date="${current_date}"
chunk_end_date="${final_date}"
chunk_is_restart="${restart_flag}"
EOF
        mkdir -p ${chunkdir}/WRFV3/run
        cp ${userdir}/namelist.input ${chunkdir}/WRFV3/run/namelist.input
        cp ${userdir}/wrf.input      ${chunkdir}/wrf.input
        cp ${userdir}/wrf4g.conf     ${chunkdir}/wrf4g.conf
        mkdir ${chunkdir}/bin
        cp ${wrf4g_root}/wn/bin/vcp  ${chunkdir}/bin/vcp
        cp ${wrf4g_root}/wn/WRF4G.sh ${chunkdir}/WRF4G.sh
        cp ${wrf4g_root}/ui/scripts/WRF4G_ini.sh ${chunkdir}/WRF4G_ini.sh
        if test -d "wrf4g_files"; then
          cd wrf4g_files/
            tar czf ${chunkdir}/wrf4g_files.tar.gz *
          cd ..
        fi
        cd ${chunkdir}
          #
          #   Submit the job
          #
          chunkjid=$(${wrf4g_root}/ui/scripts/wrf4g_submit.${JOB_TYPE} WRF4G_ini.sh ${chunkjid})
        cd ${userdir}
        echo "${chunkjid} ${chunkno} ${realization_name}" >> pids.${experiment_name}
      fi
    fi
    #
    #  Cycle dates and jobids
    #
    current_date=${final_date}
    read cyy cmm cdd chh trash <<< $(echo ${current_date} | tr '_:T-' '    ')
    let chunkno++
    test "${justone}" = "yes" && exit
    sleep ${waitsec}
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
  let hours=chh+simulation_length_h
  final_date=$(date +%Y-%m-%d_%H:%M:%S -u -d"${cyy}${cmm}${cdd} $hours hours")
  read fyy fmm fdd fhh trash <<< $(echo ${final_date} | tr '_:T-' '    ')
  while test ${cyy}${cmm}${cdd}${chh} -le ${eyy}${emm}${edd}${ehh}
  do
    let hours=chh+simulation_length_h
    final_date=$(date +%Y-%m-%d_%H:%M:%S -u -d"${cyy}${cmm}${cdd} $hours hours")
    read fyy fmm fdd fhh trash <<< $(echo ${final_date} | tr '_:T-' '    ')
    if test ${fyy}${fmm}${fdd}${fhh} -gt ${eyy}${emm}${edd}${ehh}; then
      break # new behavior (delete these line to go back)
      final_date=${end_date}
      read fyy fmm fdd fhh trash <<< $(echo ${final_date} | tr '_:T-' '    ')
    fi
    cycle_chunks ${realization_name}__${cyy}${cmm}${cdd}${chh}_${fyy}${fmm}${fdd}${fhh} ${current_date} ${final_date}
    #
    #  Cycle dates
    #
    let hoursnext=chh+simulation_interval_h
    current_date=$(date +%Y-%m-%d_%H:%M:%S -u -d"${cyy}${cmm}${cdd} $hoursnext hours")
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
      test -n "${simulation_length_h}" || exit
      test -n "${simulation_interval_h}" || exit
      cycle_hindcasts ${realization_name} ${start_date} ${end_date}
      ;;
    1)
      echo "---> Continuous run"
      cycle_chunks ${realization_name} ${start_date} ${end_date}
      ;;
  esac
}

rm -f pids.${experiment_name}
#
#  Initial override of namelist values
#
if ! is_dry_run; then 
  cp ${wrf4g_root}/wn/WRFV3/test/em_real/namelist.input ${userdir}/namelist.input.base
  fortnml -wof namelist.input.base -s max_dom ${max_dom}
  for var in $(get_ni_vars); do
    fnvar=${var/__/@}
    fortnml -wof namelist.input.base -s $fnvar -- $(eval echo \$NI_${var})
  done
  for var in $(get_nim_vars); do
    fnvar=${var/__/@}
    fortnml -wof namelist.input.base -s $fnvar -- $(eval "echo \$NIM_${var} | tr ',' ' '")
  done
  for var in $(get_nin_vars); do
    fnvar=${var/__/@}
    fortnml -wof namelist.input.base -m $fnvar -- $(eval echo \$NIN_${var})
  done
fi
#
#  Multiphysics support. Physical parameters overwritten!
#
if test "${is_multiphysics}" -ne "0"; then
  echo "---> Multi-physics run"
  for mpid in $(echo ${multiphysics_combinations} | tr '/' ' '); do
    if_not_dry cp namelist.input.base namelist.input
    iphys=1
    for var in $(echo ${multiphysics_variables} | tr ',' ' '); do
      nitems=$(tuple_item ${multiphysics_nitems} ${iphys})
      if_not_dry fortnml_setn namelist.input $var ${nitems} $(tuple_item ${mpid} ${iphys})
      let iphys++
    done
    cycle_time "${experiment_name}__${mpid//,/_}" ${start_date} ${end_date}
  done
else
  echo "---> Single physics run"
  if_not_dry cp namelist.input.base namelist.input
  cycle_time "${experiment_name}" ${start_date} ${end_date}
fi
