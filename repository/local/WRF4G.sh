#!/bin/bash
#
# In order to run this script in verbose mode run the 
# gwsubmit command with the verbose option. DO NOT ADD -x 
# to the first of this script


function verbose_print() { ((VERBOSE)) && echo $@; return 0; }

function date2int() { date=$[1]; echo $date | sed 's/[-_:]//g' }

function default_config() {
  #
  #  Some default values
  #
  save_wps=0
  clean_after_run=1
  timestep_dxfactor=6
  real_parallel=0
  wrf_parallel=1
  DEBUG="-v"
  VERBOSE=1
  default_preprocessor="default"
  ERROR_MISSING_RESOURCESWRF4G=21
  LAUNCHER_REAL=""
  LAUNCHER_WRF=""
}

function WRF4G_structure (){
  out=$(WRF4G.py Job set_status id=${WRF4G_JOB_ID} ${JOB_STATUS_DOWN_BIN})

  vcp ${DEBUG} ${WRF4G_APPS}/WRFbin-${WRF_VERSION}.tar.gz . 
  tar xzf WRFbin-${WRF_VERSION}.tar.gz || wrf4g_exit ${ERROR_MISSING_WRFbin}
  rm WRFbin-${WRF_VERSION}.tar.gz

  vcp ${DEBUG} ${WRF4G_BASEPATH}/${WRF4G_EXPERIMENT}/${WRF4G_REALIZATION}/namelist.input  WRFV3/run/namelist.input \
    || wrf4g_exit ${ERROR_MISSING_NAMELIST}

  #   If there are additional files, expand them
  vcp ${DEBUG} ${WRF4G_BASEPATH}/${WRF4G_EXPERIMENT}/wrf4g_files.tar.gz ${ROOTDIR} &>/dev/null
  if test -f ${ROOTDIR}/wrf4g_files.tar.gz; then
    tar xzf ${ROOTDIR}/wrf4g_files.tar.gz && rm ${ROOTDIR}/wrf4g_files.tar.gz
  fi
}

function WRF4G_prepare (){
  #
  #  Move all executables out of LOCALDIR
  #
  mv ${LOCALDIR}/WPS/ungrib/ungrib.exe   ${ROOTDIR}/bin/
  mv ${LOCALDIR}/WPS/metgrid/metgrid.exe ${ROOTDIR}/bin/
  mv ${LOCALDIR}/WRFV3/run/real.exe      ${ROOTDIR}/bin/
  mv ${LOCALDIR}/WRFV3/run/wrf.exe       ${ROOTDIR}/bin/
  umask 002
}

function prepare_runtime_environment(){
  # Does any component run in parallel?
  prepare_openmpi=0
  local_openmpi=0
  
  if (($PPN));then 
    MPI_LAUNCHER="mpirun -np $GW_NP -npernode $PPN $MPI_ENV"
  else
    MPI_LAUNCHER="mpirun -np $GW_NP $MPI_ENV"
  fi
  
  if [ $real_parallel -eq 1 ]; then
    prepare_openmpi=1
    LAUNCHER_REAL=${MPI_LAUNCHER}
  fi

  if [ $wrf_parallel -eq 1 ]; then
    prepare_openmpi=1
    LAUNCHER_WRF=${MPI_LAUNCHER}
  fi

  # If WRF4G is going to run in a shared folder do not copy the
  # binary and input files. Otherwise copy them.
  if [ $prepare_openmpi -eq 1 ]; then
    if test -n "${RUN_DIR}"; then    
      local_openmpi=1	   
    fi
  fi
  
  export OPAL_PREFIX=${ROOTDIR}/openmpi
  export PATH=$OPAL_PREFIX/bin:$PATH
  export LD_LIBRARY_PATH=$OPAL_PREFIX/lib:$LD_LIBRARY_PATH
  
}

function prepare_local_environment (){
  mv ${LOCALDIR}/openmpi  ${ROOTDIR}/
  cp -R ${LOCALDIR}/WRFV3/run ${ROOTDIR}/runshared
$MPI_LAUNCHER -pernode --wdir ${ROOTDIR} ${ROOTDIR}/bin/load_wrfbin.sh	
  prepare_openmpi=0
}

function transfer_output_log (){
  ls -l >& ${logdir}/ls.wrf
  test -f namelist.output         && cp namelist.output ${logdir}/
  test -f ../configure.wrf_wrf    && cp ../configure.wrf_wrf ${logdir}/
  test -f ../configure.wrf_real   && cp ../configure.wrf_real ${logdir}/
  test -f ../../WPS/configure.wps && cp ../../WPS/configure.wps ${logdir}/
  
  test -f namelist.input && cp namelist.input ${logdir}/
  
  verbose_print "**********************************************************************************"
  verbose_print "WRF4G was deployed in ... "
  verbose_print "    $ROOTDIR"
  verbose_print "and it ran in ..."
  verbose_print "    $LOCALDIR"
  verbose_print "**********************************************************************************"
  logfile="log_${WRF4G_NCHUNK}_${WRF4G_JOB_ID}.tar.gz"
  cd ${logdir}
  tar czf ${logfile} * && vcp ${logfile} ${WRF4G_BASEPATH}/${WRF4G_EXPERIMENT}/${WRF4G_REALIZATION}/log/
  cd -
}

function download_wps (){
  output=$(WRF4G.py Job set_status id=${WRF4G_JOB_ID} ${JOB_STATUS_DOWN_WPS})
  vcp ${DEBUG} ${WRF4G_DOMAINPATH}/${domain_name}/ . || wrf4g_exit ${ERROR_VCP_FAILED}
}

function run_modify_namelist (){ 
  fortnml_setn namelist.wps start_date       ${max_dom} "'${chunk_start_date}'"
  fortnml_setn namelist.wps end_date         ${max_dom} "'${chunk_end_date}'"
  fortnml_set  namelist.wps max_dom          ${max_dom}
  fortnml_set  namelist.wps prefix           "'${extdata_vtable}'"
  fortnml_set  namelist.wps interval_seconds ${extdata_interval}
}

function run_preprocessor_ungrib (){
  vts=''
  i=1
  for vt in ${extdata_vtable/,/ }; do
    fortnml --overwrite -f namelist.wps -s prefix@ungrib ${vt}
    test -f Vtable && rm Vtable 
    ln -s ungrib/Variable_Tables/Vtable.${vt} Vtable
    vpath=$(tuple_item ${extdata_path} ${i})
    vpreprocessor=$(tuple_item ${extdata_preprocessor} ${i})

    output=$(WRF4G.py Job set_status id=${WRF4G_JOB_ID} ${JOB_STATUS_DOWN_BOUND})
    verbose_print "* `date`: Running preprocessor.${vpreprocessor} ... "
    preprocessor.${vpreprocessor} ${chunk_start_date} ${chunk_end_date} ${vpath} ${vt} \
      || wrf4g_exit ${ERROR_PREPROCESSOR_FAILED}
    ./link_grib.sh grbData/*
    
    output=$(WRF4G.py Job set_status id=${WRF4G_JOB_ID} ${JOB_STATUS_UNGRIB})
    verbose_print "* `date`: Running ungrib ... "
    ungrib.exe >& ${logdir}/ungrib_${vt}_${iyy}${imm}${idd}${ihh}.out \
      || wrf4g_exit ${ERROR_UNGRIB_FAILED}
    cat ${logdir}/ungrib_${vt}_${iyy}${imm}${idd}${ihh}.out \
      |  grep -q -i 'Successful completion of ungrib' \
      || wrf4g_exit ${ERROR_UNGRIB_FAILED}
    rm -rf grbData
    rm GRIBFILE.*
    vts=${vts}\'${vt}\',
    let i++
  done
}

function run_metgrid (){
  output=$(WRF4G.py Job set_status id=${WRF4G_JOB_ID} ${JOB_STATUS_METGRID})
  fortnml --overwrite -f namelist.wps -s fg_name@metgrid "${vts}"
  fortnml_vardel namelist.wps opt_output_from_metgrid_path
  fortnml_vardel namelist.wps opt_output_from_geogrid_path
  fortnml_vardel namelist.wps opt_metgrid_tbl_path
  fortnml_vardel namelist.wps opt_geogrid_tbl_path
  fortnml_setn namelist.wps start_date ${max_dom} "'${chunk_start_date}'"
  fortnml_setn namelist.wps end_date   ${max_dom} "'${chunk_end_date}'"
  
  metgrid.exe >& ${logdir}/metgrid_${iyy}${imm}${idd}${ihh}.out \
    || wrf4g_exit ${ERROR_METGRID_FAILED}
  cat ${logdir}/metgrid_${iyy}${imm}${idd}${ihh}.out \
    |  grep -q -i 'Successful completion of metgrid' \
    || wrf4g_exit ${ERROR_METGRID_FAILED}
  
  # Clean after run
  if test "${clean_after_run}" -eq 1; then
    rm -f GRIBFILE.*
    rm -rf grbData
    for file in ${extdata_vtable/,/ }; do
      rm -rf ${file}\:*
    done
  fi
}

function run_real (){
  clean_real
  ln -s ../../WPS/met_em.d??.????-??-??_??:00:00.nc .
  fix_ptop
  namelist_wps2wrf ${chunk_restart_date} ${chunk_end_date} ${max_dom} ${chunk_rerun} ${timestep_dxfactor}
  
  # If real is run in parallel, prepare the environment.  	 
  if [ ${local_openmpi} -eq 1 -a ${real_parallel} -eq 1 ]; then
    prepare_local_environment
  fi
  output=$(WRF4G.py Job set_status id=${WRF4G_JOB_ID} ${JOB_STATUS_REAL})
  ${LAUNCHER_REAL} ${ROOTDIR}/bin/wrapper.exe real.exe \
    >& ${logdir}/real_${ryy}${rmm}${rdd}${rhh}.out \
    || wrf4g_exit ${ERROR_REAL_FAILED}

  if [ ${real_parallel} -ne 1 ]; then
    log_file_real=${logdir}/real_${ryy}${rmm}${rdd}${rhh}.out			
  else
    log_file_real=rsl.out.0000
  fi
  
  if test $(grep -c "SUCCESS COMPLETE REAL_EM" ${log_file_real}) -ne 1 ;then
    wrf4g_exit ${ERROR_REAL_FAILED}
  fi

  if test -e rsl.out.0000; then
    mkdir -p rsl_real
    mv rsl.* rsl_real/
    mv rsl_real ${logdir}/
  fi
  
  # Clean after run
  if test "${clean_after_run}" -eq 1; then
    rm -f met_em*
    rm -f ../../WPS/met_em*
  fi
}

function run_save_wps (){
  output=$(WRF4G.py Job set_status id=${WRF4G_JOB_ID} ${JOB_STATUS_UPLIAD_WPS})
  post_and_register --no-bg wps "${chunk_start_date}"
  output=$(WRF4G.py Chunk set_wps id=${WRF4G_CHUNK_ID} 1)
}

function run_icbprocesor (){
  output=$(WRF4G.py Job set_status id=${WRF4G_JOB_ID} ${JOB_STATUS_ICBCPROCESOR})
  icbcprocessor.${icbcprocessor} >&  ${logdir}/icbcproc_${ryy}${rmm}${rdd}${rhh}.out
}

function run_wrf (){
  output=$(WRF4G.py Job set_status id=${WRF4G_JOB_ID} ${JOB_STATUS_WRF})
  # If wrf is run in parallel and the environment is not prepared, prepare it.  	 
  if [ ${local_openmpi} -eq 1 -a ${wrf_parallel} -eq 1  ]; then
    prepare_local_environment
  fi
  
  fortnml -o -f namelist.input -s debug_level 0
  
  ${LAUNCHER_WRF} ${ROOTDIR}/bin/wrapper.exe wrf.exe >& ${logdir}/wrf_${ryy}${rmm}${rdd}${rhh}.out &
  # Wait enough time to allow 'wrf_wrapper.exe' create 'wrf.pid'
  # This time is also useful to copy the wpsout data
  sleep 30
  
  if [ ${wrf_parallel} -ne 1 ]; then
    log_file_wrf=${logdir}/wrf_${ryy}${rmm}${rdd}${rhh}.out
  else
    log_file_wrf=rsl.out.0000
  fi
   
  bash wrf4g_monitor $(cat wrf.pid) $log_file_wrf >& ${logdir}/monitor.log &
  echo $! > monitor.pid
  wait $(cat monitor.pid)
   
  if test $(grep -c "SUCCESS COMPLETE WRF" ${log_file_wrf}) -eq 1 ;then
    excode=0
  else
    excode=${ERROR_UNEXPECTED_WRF_TERMINATION}
  fi
  
  if test -e rsl.out.0000; then
    mkdir -p rsl_wrf
    mv rsl.* rsl_wrf/
    mv rsl_wrf ${logdir}/
  fi
  
  wrf4g_exit $excode
}

function wrf4g_exit(){
  excode=$1
  #
  #  This is the way out of this script. So close the timing info, move the
  #  logs to a safe place and leave
  #
 
  case $excode in
  0)
    status=${JOB_STATUS_FINISHED}
  ;;
  ${ERROR_UNGRIB_FAILED})
    ls -lR >& ${logdir}/ls.wps
    status=${JOB_STATUS_FAILED}
  ;;
  ${EXIT_CHUNK_ALREADY_FINISHED})
    exit 0
  ;;
  ${EXIT_CHUNK_SHOULD_NOT_RUN})
    exit ${EXIT_CHUNK_SHOULD_NOT_RUN}
  ;;
  ${EXIT_PREVIOUS_CHUNK_NOT_FINISHED_CORRECT})
    exit ${EXIT_PREVIOUS_CHUNK_NOT_FINISHED_CORRECT}
  ;;
  ${EXIT_RESTART_MISMATCH})
    exit ${EXIT_RESTART_MISMATCH}
  ;;
  *)
    status=${JOB_STATUS_FAILED}
  ;;
  esac
  
  output=$(WRF4G.py Job set_status   id=${WRF4G_JOB_ID} ${status})
  output=$(WRF4G.py Job set_exitcode id=${WRF4G_JOB_ID} ${excode})
  
  transfer_output_log
  
  # Clean the heavy stuff
  if test "${clean_after_run}" -eq 1; then
    cd ${ROOTDIR}
    test "${LOCALDIR}" != "${ROOTDIR}" && rm -rf ${LOCALDIR} 
  fi
  exit ${excode}
}

################# WRF4G initializer ######################

#
#  Change working directory to WRF4G_RUN_SHARED if needed 
#
default_config

#
#  Get variables.
#

source resources.wrf4g || exit ${ERROR_MISSING_RESOURCESWRF4G}

export WRF4G_EXPERIMENT=$1
export WRF4G_REALIZATION=$2
export WRF4G_REALIZATION_ID=$3
export WRF4G_NCHUNK=$4
export WRF4G_CHUNK_ID=$5
export chunk_start_date=$6
export chunk_end_date=$7

#
#   Expand the WRF4G scripts
#
ROOTDIR=$(pwd)
tar xzf WRF4G-${WRF4G_VERSION}.tar.gz && rm -f WRF4G-${WRF4G_VERSION}.tar.gz || exit ${ERROR_MISSING_WRF4GSRC}
chmod +x ${ROOTDIR}/bin/*

#
#   Load functions and set the PATH
#

export RESOURCES_WRF4G=${ROOTDIR}/resources.wrf4g
export DB4G_CONF=${ROOTDIR}/db4g.conf
export PATH=${ROOTDIR}/bin:$PATH
export LD_LIBRARY_PATH=${ROOTDIR}/lib:$LD_LIBRARY_PATH
export PYTHONPATH=${ROOTDIR}/lib/python:${ROOTDIR}/bin:$PYTHONPATH
export PATH=${ROOTDIR}/lib/bash:$PATH

source ${ROOTDIR}/lib/bash/wrf_util.sh
source ${ROOTDIR}/lib/bash/wrf4g_exit_codes.sh
source ${ROOTDIR}/lib/bash/wrf4g_job_status_code.sh

#
#   Try to download experiment from realization folder. If it doesn't exist download it from experiment.
#
vcp ${DEBUG} ${WRF4G_BASEPATH}/${WRF4G_EXPERIMENT}/${WRF4G_REALIZATION}/experiment.wrf4g . &>/dev/null
if [ $? -ne 0 ];then 
  vcp ${DEBUG} ${WRF4G_BASEPATH}/${WRF4G_EXPERIMENT}/experiment.wrf4g . || exit ${ERROR_MISSING_EXPERIMENTSWRF4G}
fi
sed --in-place 's/\ *=\ */=/' experiment.wrf4g 
source experiment.wrf4g

#
#   Update Job Status in DB
#
job_conf="gw_job=${GW_JOB_ID},id_chunk=${WRF4G_CHUNK_ID},resource=${GW_HOSTNAME},wn=$(hostname)"
WRF4G_JOB_ID=$(WRF4G.py --verbose Job load_wn_conf $job_conf $GW_RESTARTED) 
exitcode=$?
if test $exitcode -ne 0; then      
  if test $exitcode -eq ${EXIT_CANNOT_CONTACT_DB}; then
    exit ${EXIT_CANNOT_CONTACT_DB}
  elif test  $exitcode -eq ${EXIT_CHUNK_ALREADY_FINISHED}; then
    wrf4g_exit ${EXIT_CHUNK_ALREADY_FINISHED}
  elif test  $exitcode -eq ${EXIT_CHUNK_SHOULD_NOT_RUN}; then
    wrf4g_exit ${EXIT_CHUNK_SHOULD_NOT_RUN}
  elif test  $exitcode -eq ${EXIT_PREVIOUS_CHUNK_NOT_FINISHED_CORRECT}; then
    wrf4g_exit ${EXIT_PREVIOUS_CHUNK_NOT_FINISHED_CORRECT}
  else
    exit ${EXIT_WRF4G_NOT_WORKING}
  fi
fi

#
#   Should we unpack here or there is a local filesystem for us to run?
#
if test -n "${RUN_DIR}"; then
  LOCALDIR="${RUN_DIR}/wrf4g.$(date +%Y%m%d%H%M%S%N)"
  mkdir -p ${LOCALDIR} || wrf4g_exit ${ERROR_CANNOT_ACCESS_LOCALDIR}
  cd ${LOCALDIR}
else
  LOCALDIR=${ROOTDIR}
fi

export ROOTDIR
export LOCALDIR
echo ${ROOTDIR} > rootdir
echo ${PWD} > ${ROOTDIR}/localdir

#
# GridWay won't remove ROOTDIR directory if clean_after_run is 0
#
if test "${clean_after_run}" -eq 0; then
  touch ${ROOTDIR}/.lock
fi

#   Make log directory
logdir=${LOCALDIR}/log; export logdir
mkdir -p ${logdir}
#   Redirect output and error file descriptors
exec &>log/WRF4G.log

verbose_print "* `date`: Creating WRF4G structure ... "
WRF4G_structure
verbose_print "* `date`: Preparing WRF4G binaries ... "
WRF4G_prepare
verbose_print "* `date`: Creating parallel environment ... "
prepare_runtime_environment

#
#  Get the restart files if they are necessary.
#
verbose_print "* `date`: Checking restart information ... "
restart_date=$(WRF4G.py Realization get_restart id=${WRF4G_REALIZATION_ID}) || wrf4g_exit ${ERROR_ACCESS_DB}
if test ${restart_date} == "None"; then
  export chunk_restart_date=${chunk_start_date}
  export chunk_rerun=".F."
elif test $(date2int ${restart_date}) -ge $(date2int ${chunk_start_date}) -a $(date2int ${restart_date}) -le $(date2int ${chunk_end_date}); then
  export chunk_restart_date=${restart_date}
  export chunk_rerun=".T."  
  output=$(WRF4G.py Job set_status id=${WRF4G_JOB_ID} ${JOB_STATUS_DOWN_RESTART})
  download_file rst $(date_wrf2iso ${restart_date}) || wrf4g_exit ${ERROR_RST_DOWNLOAD_FAILED}
  mv wrfrst* WRFV3/run 
else
  wrf4g_exit ${EXIT_RESTART_MISMATCH}
fi

read iyy imm idd ihh trash <<< $(echo ${chunk_start_date}   | tr '_:T-' '    ')
read ryy rmm rdd rhh trash <<< $(echo ${chunk_restart_date} | tr '_:T-' '    ')
read fyy fmm fdd fhh trash <<< $(echo ${chunk_end_date}     | tr '_:T-' '    ')

#
#   Either WPS runs or the boundaries and initial conditions are available
#
wps_stored=$(WRF4G.py Chunk get_wps id=${WRF4G_CHUNK_ID}) \
  || wrf4g_exit ${ERROR_ACCESS_DB}

cd ${LOCALDIR}/WPS || wrf4g_exit ${ERROR_GETTING_WPS}
if test ${wps_stored} -eq "1"; then
  verbose_print "* `date`: The boundaries and initial conditions are available ... "
  vcp ${DEBUG} ${WRF4G_DOMAINPATH}/${domain_name}/namelist.wps . || wrf4g_exit ${ERROR_VCP_FAILED} 
  cd ${LOCALDIR}/WRFV3/run || wrf4g_exit ${ERROR_CANNOT_ACCESS_LOCALDIR}
  namelist_wps2wrf ${chunk_restart_date} ${chunk_end_date} ${max_dom} ${chunk_rerun} ${timestep_dxfactor}
  output=$(WRF4G.py Job set_status id=${WRF4G_JOB_ID} ${ERROR_MISSING_EXPERIMENTSWRF4G})
  download_file ${DEBUG} real $(date_wrf2iso ${chunk_start_date})
else
  verbose_print "* `date`: The boundaries and initial conditions are not available ... "
  clean_wps
  #
  #   Get geo_em files and namelist.wps
  #
  verbose_print "* `date`: Downloading geo_em files and namelist.wps ... "
  download_wps 
  
  #
  #   Modify the namelist
  #
  verbose_print "* `date`: Modifying namelist ... "
  run_modify_namelist
  
  #
  #  Preprocessor and Ungrib
  #
  verbose_print "* `date`: About to run preprocessor and Ungrib ... "
  run_preprocessor_ungrib
  
  #
  #  Metgrid
  #
  verbose_print "* `date`: Running Metgrid ... "
  run_metgrid  
  
  cd ${LOCALDIR}/WRFV3/run || wrf4g_exit ${ERROR_CANNOT_ACCESS_LOCALDIR}
  #
  #  REAL
  #
  verbose_print "* `date`: Running Real ... "
  run_real  
  
  #
  #  Upload the wpsout files (create the output structure if necessary):
  #
  #    wrfinput_d0?
  #    wrfbdy_d0?
  #    wrflowinp_d0?
  #    wrffdda_d0?
  #
  if test "${save_wps}" -eq 1; then
    verbose_print "* `date`: Saving wps ... "
    run_save_wps
  fi
fi

#
# Icbcprocessor
#
if test -n "${icbcprocessor}"; then
  verbose_print "* `date`: Running icbcprocessor.${icbcprocessor} ... "
  run_icbprocesor
fi

#
#  WRF
#    
verbose_print "* `date`: Running WRF ... "
run_wrf

