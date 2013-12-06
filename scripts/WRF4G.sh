#!/bin/bash

function date2int(){
  date=$[1]
  echo $date| sed 's/[-_:]//g'
}

function prepare_local_environment (){
  cp -R ${LOCALDIR}/WRFV3/run ${ROOTDIR}/runshared
  $MPI_LAUNCHER -pernode --wdir ${ROOTDIR} ${ROOTDIR}/WRFGEL/load_wrfbin.sh
}

function run_preprocessor_ungrib (){
  output=$(WRF4G.py Job set_status id=${WRF4G_JOB_ID} ${JOB_STATUS_UNGRIB})
  vts=''
  i=1
  for vt in ${extdata_vtable/,/ }; do
    fortnml --overwrite -f namelist.wps -s prefix@ungrib ${vt}
    test -f ${WRF4G_LOCALSCP}/WPS/Vtable && rm -rf ${WRF4G_LOCALSCP}/WPS/Vtable 
    ln -s ${WRF4G_LOCALSCP}/WPS/ungrib/Variable_Tables/Vtable.${vt} ${WRF4G_LOCALSCP}/WPS/Vtable
    vpath=$(tuple_item ${extdata_path} ${i})
    vpreprocessor=$(tuple_item ${extdata_preprocessor} ${i})

    echo "* `date`: Running preprocessor.${vpreprocessor} ... "
    
    preprocessor.${vpreprocessor} ${chunk_start_date} ${chunk_end_date} ${vpath} ${vt} || wrf4g_exit ${ERROR_PREPROCESSOR_FAILED}
    ./link_grib.sh ${WRF4G_LOCALSCP}/WPS/grbData/*
    
    echo "* `date`: Running ungrib ... "
    
    log_ungrib=${logdir}/ungrib_${vt}_${iyy}${imm}${idd}${ihh}.out
    ungrib.exe >& ${logdir_ungrib} || wrf4g_exit ${ERROR_UNGRIB_FAILED}
    if test $(grep -c "Successful completion of ungrib" ${log_ungrib}) -ne 1 ;then
      wrf4g_exit ${ERROR_UNGRIB_FAILED}
    fi
       
    rm -rf ${WRF4G_LOCALSCP}/WPS/grbData ${WRF4G_LOCALSCP}/WPS/GRIBFILE.*
    vts=${vts}\'${vt}\',
    let i++
  done
}

function run_metgrid (){
  WRF4G.py Job set_status id=${WRF4G_JOB_ID} ${JOB_STATUS_METGRID}
  
  fortnml        --overwrite -f namelist.wps -s fg_name@metgrid "${vts}"
  fortnml_vardel namelist.wps opt_output_from_metgrid_path
  fortnml_vardel namelist.wps opt_output_from_geogrid_path
  fortnml_vardel namelist.wps opt_metgrid_tbl_path
  fortnml_vardel namelist.wps opt_geogrid_tbl_path
  fortnml_setn   namelist.wps start_date ${max_dom} "'${chunk_start_date}'"
  fortnml_setn   namelist.wps end_date   ${max_dom} "'${chunk_end_date}'"
  
  log_metgrid=${logdir}/metgrid_${iyy}${imm}${idd}${ihh}.out
  metgrid.exe >& ${log_metgrid} || wrf4g_exit ${ERROR_METGRID_FAILED}
  if test $(grep -c "Successful completion of metgrid" ${log_metgrid}) -ne 1 ;then
    wrf4g_exit ${ERROR_METGRID_FAILED}
  fi
  
  # Clean after run
  if test "${clean_after_run}" -eq 1; then
    rm -rf ${WRF4G_LOCALSCP}/WPS/grbData ${WRF4G_LOCALSCP}/WPS/GRIBFILE.*
    for file in ${extdata_vtable/,/ }; do
      rm -rf ${file}\:*
    done
  fi
}

function run_real (){
  clean_real
  ln -s ${WRF4G_LOCALSCP}/WPS/met_em.d??.????-??-??_??:00:00.nc ${WRF4G_LOCALSCP}/WRFV3/run
  fix_ptop
  namelist_wps2wrf ${chunk_restart_date} ${chunk_end_date} ${max_dom} ${chunk_rerun} ${timestep_dxfactor}
  
  # If real is run in parallel, prepare the environment.  	 
  if [ ${real_parallel} -eq 1 ]; then
    prepare_local_environment
  fi
  
  WRF4G.py Job set_status id=${WRF4G_JOB_ID} ${JOB_STATUS_REAL}

  
  log_file_real=${logdir}/real_${ryy}${rmm}${rdd}${rhh}.out
  ${LAUNCHER_REAL} ${WRF4G_ROOTDIR}/bin/wrapper.exe real.exe >& ${log_file_real} || wrf4g_exit ${ERROR_REAL_FAILED}
  
  if [ ${real_parallel} -eq 1 ]; then
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
    rm -f ${WRF4G_LOCALSCP}/WRFV3/run/met_em*
    rm -f ${WRF4G_LOCALSCP}/WPS/met_em*
  fi
}

function run_wrf (){
  WRF4G.py Job set_status id=${WRF4G_JOB_ID} ${JOB_STATUS_WRF}
  # If wrf is run in parallel and the environment is not prepared, prepare it.  	 
  if [ ${wrf_parallel} -eq 1  ]; then
    prepare_local_environment
  fi
  
  fortnml -o -f namelist.input -s debug_level 0
  
  ${LAUNCHER_WRF} ${WRF4G_ROOTDIR}/bin/wrapper.exe wrf.exe >& ${logdir}/wrf_${ryy}${rmm}${rdd}${rhh}.out &
  # Wait enough time to allow 'wrf_wrapper.exe' create 'wrf.pid'
  # This time is also useful to copy the wpsout data
  sleep 30
   
  log_file_wrf=${logdir}/wrf_${ryy}${rmm}${rdd}${rhh}.out
  bash wrf4g_monitor $(cat wrf.pid) ${log_file_wrf} >& ${logdir}/monitor.log &
  echo $! > monitor.pid
  wait $(cat monitor.pid)
   
  if [ ${wrf_parallel} -eq 1 ]; then
    log_file_wrf=rsl.out.0000
  fi
 
  if test $(grep -c "SUCCESS COMPLETE WRF" ${log_file_wrf}) -eq 1 ;then
    excode=0
  else
    excode=${ERROR_UNEXPECTED_WRF_TERMINATION}
  fi
  
  if test -e ${WRF4G_LOCALSCP}/WRFV3/run/rsl.out.0000; then
    mkdir -p ${WRF4G_LOCALSCP}/WRFV3/run/rsl_wrf
    mv ${WRF4G_LOCALSCP}/WRFV3/run/rsl.* ${WRF4G_LOCALSCP}/WRFV3/run/rsl_wrf/
    mv ${WRF4G_LOCALSCP}/WRFV3/run/rsl_wrf ${logdir}/
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



##########################################################
##################### WRF4G init #########################
##########################################################

#  Tree directory

################################
# ├── bin                      #
# ├── lib                      #
# │   ├── bash                 #
# │   ├── python               #
# │   └── shared_libs          #
# ├── wrf4g_files              #
# ├── WRFGEL                   #
# ├── openmpi                  #
# │   ├── bin                  #
# │   └── lib                  #
# │       └── openmpi          #
# ├── WPS                      #
# │   ├── metgrid              #
# │   └── ungrib               #
# │       └── Variable_Tables  #
# └── WRFV3                    #
#     └── run                  #
################################


cd `dirname $0`
export WRF4G_ROOTDIR=`pwd`
export WRF4G_ROOTDIR=`pwd`


#
#  Default values
#

save_wps=0
clean_after_run=1
timestep_dxfactor=6
real_parallel=0
wrf_parallel=1
DEBUG="-v"
ERROR_MISSING_RESOURCESWRF4G=21


#
#  Get variables
#

export WRF4G_EXPERIMENT=$1
export WRF4G_REALIZATION=$2
export WRF4G_REALIZATION_ID=$3
export WRF4G_NCHUNK=$4
export WRF4G_CHUNK_ID=$5
export chunk_start_date=$6
export chunk_end_date=$7

sed --in-place 's/\ *=\ */=/' experiment.wrf4g
. experiment.wrf4g &>/dev/null
sed --in-place 's/\ *=\ */=/' resources.wrf4g
. resources.wrf4g &>/dev/null
sed --in-place 's/\ *=\ */=/' db4g.conf
. db4g.conf &>/dev/null

. ${WRF4G_ROOTDIR}/lib/bash/wrf_util.sh
. ${WRF4G_ROOTDIR}/lib/bash/wrf4g_codes.sh

#
#   Expand the WRF4G scripts
#

if test -f WRF4G-${WRF4G_VERSION}.tar.gz; then
  tar xzf WRF4G-${WRF4G_VERSION}.tar.gz 
  rm WRF4G-${WRF4G_VERSION}.tar.gz
  chmod +x ${WRF4G_ROOTDIR}/WRFGEL/*
else
  exit ${ERROR_MISSING_WRF4GSRC}
fi
#   If there are additional files, expand them
if test -f wrf4g_files.tar.gz; then
  echo "* `date`: There is a wrf4g_files.tar.gz package available ... "
  tar xzf wrf4g_files.tar.gz
  rm wrf4g_files.tar.gz
  chmod +x ${WRF4G_ROOTDIR}/wrf4g_files/*
fi

#
#   Set the PATH
#

export RESOURCES_WRF4G=${WRF4G_ROOTDIR}/resources.wrf4g
export DB4G_CONF=${WRF4G_ROOTDIR}/db4g.conf
export PATH=${WRF4G_ROOTDIR}/bin:${WRF4G_ROOTDIR}/WRFGEL:${WRF4G_ROOTDIR}/lib/bash:$PATH
export LD_LIBRARY_PATH=${WRF4G_ROOTDIR}/lib/shared_libs:$LD_LIBRARY_PATH
export PYTHONPATH=${WRF4G_ROOTDIR}/lib/python:$PYTHONPATH

#
#   Update Job Status in DB
#

job_conf="gw_job=${GW_JOB_ID},id_chunk=${WRF4G_CHUNK_ID},resource=${GW_HOSTNAME},wn=$(hostname)"
WRF4G_JOB_ID=$(WRF4G.py --verbose Job load_wn_conf $job_conf $GW_RESTARTED) 
exit_code=$?
if test $exit_code -ne 0; then
  wrf4g_exit ${exit_code} 
fi

#
#   Making remote directories for this job
#
echo "* `date`: Creating remote WRF4G structure ... "

out=$(WRF4G.py Realization prepare_remote_storage)
if test $out -ne 0; then
  wrf4g_exit ${ERROR_CANNOT_CREATE_REMOTE_DIR} 
fi

#
#   Should we unpack here or there is a local filesystem for us to run?
#

if test -n "${WRF4G_LOCALSCP}"; then
  export WRF4G_LOCALSCP="${WRF4G_LOCALSCP}/wrf4g.$(date +%Y%m%d%H%M%S%N)"
  mkdir -p ${WRF4G_LOCALSCP} || wrf4g_exit ${ERROR_CANNOT_ACCESS_WRF4G_LOCALSCP}
  cd ${WRF4G_LOCALSCP}
else
  export WRF4G_LOCALSCP=${WRF4G_ROOTDIR}
fi

#
# DRM4G won't remove WRF4G_ROOTDIR directory if clean_after_run is 0
#

if test "${clean_after_run}" -eq 0; then
  touch ${WRF4G_ROOTDIR}/.lock
fi

#
#   Make log directory
#

export logdir=${WRF4G_LOCALSCP}/log
mkdir -p ${logdir}
#   Redirect output and error file descriptors
exec &>log/WRF4G.log

echo "* `date`: Creating WRF4G structure ... "

WRF4G.py Job set_status id=${WRF4G_JOB_ID} ${JOB_STATUS_DOWN_BIN}
vcp ${DEBUG} ${WRF4G_APPS}/WRFbin-${WRF_VERSION}.tar.gz ${WRF4G_LOCALSCP}/
exitcode=$?
if test $exitcode -eq 0; then
  tar xzf WRFbin-${WRF_VERSION}.tar.gz && rm WRFbin-${WRF_VERSION}.tar.gz
  mv ${WRF4G_ROOTDIR}/namelist.input  ${WRF4G_LOCALSCP}/WRFV3/run/namelist.input
else
  wrf4g_exit ${ERROR_MISSING_WRFbin}
fi

echo "* `date`: Moving WRF binaries to \${WRF4G_ROOTDIR}/bin directory ... "

mv ${WRF4G_LOCALSCP}/WPS/ungrib/ungrib.exe   ${WRF4G_ROOTDIR}/bin/
mv ${WRF4G_LOCALSCP}/WPS/metgrid/metgrid.exe ${WRF4G_ROOTDIR}/bin/
mv ${WRF4G_LOCALSCP}/WRFV3/run/real.exe      ${WRF4G_ROOTDIR}/bin/
mv ${WRF4G_LOCALSCP}/WRFV3/run/wrf.exe       ${WRF4G_ROOTDIR}/bin/

# To give all access permissions to the group and allow other users 
# read and execute permission  
umask 002

echo "* `date`: Creating parallel environment ... "
# Does any component run in parallel?
LAUNCHER_REAL=""
LAUNCHER_WRF=""
MPI_LAUNCHER="mpirun -np $GW_NP $MPI_ENV"

if [ $real_parallel -eq 1 ]; then
  LAUNCHER_REAL=${MPI_LAUNCHER}
fi

if [ $wrf_parallel -eq 1 ]; then
  LAUNCHER_WRF=${MPI_LAUNCHER}
fi

# If WRF4G is going to run in a shared folder do not copy the
# binary and input files. Otherwise copy them.
if [ $prepare_openmpi -eq 1 ]; then
  if test "${WRF4G_LOCALSCP}" -eq "${WRF4G_ROOTDIR}"; then
    export OPAL_PREFIX=${WRF4G_LOCALSCP}/openmpi
  else
    mv ${WRF4G_LOCALSCP}/openmpi  ${WRF4G_ROOTDIR}/
    export OPAL_PREFIX=${WRF4G_ROOTDIR}/openmpi
  fi
  export PATH=$OPAL_PREFIX/bin:$PATH
  export LD_LIBRARY_PATH=$OPAL_PREFIX/lib:$LD_LIBRARY_PATH
fi

#
#  Get the restart files if they are necessary.
#
echo "* `date`: Checking restart information ... "

restart_date=$(WRF4G.py Realization get_restart id=${WRF4G_REALIZATION_ID}) || wrf4g_exit ${ERROR_ACCESS_DB}
if test ${restart_date} == "None"; then
  export chunk_restart_date=${chunk_start_date}
  export chunk_rerun=".F."
elif test $(date2int ${restart_date}) -ge $(date2int ${chunk_start_date}) -a $(date2int ${restart_date}) -le $(date2int ${chunk_end_date}); then
  export chunk_restart_date=${restart_date}
  export chunk_rerun=".T."  
  WRF4G.py Job set_status id=${WRF4G_JOB_ID} ${JOB_STATUS_DOWN_RESTART}
  cd ${WRF4G_LOCALSCP}/WRFV3/run
    download_file rst $(date_wrf2iso ${restart_date}) || wrf4g_exit ${ERROR_RST_DOWNLOAD_FAILED}
  cd -
else
  wrf4g_exit ${EXIT_RESTART_MISMATCH}
fi

#
#  Dividing dates 
#
read iyy imm idd ihh trash <<< $(echo ${chunk_start_date}   | tr '_:T-' '    ')
read ryy rmm rdd rhh trash <<< $(echo ${chunk_restart_date} | tr '_:T-' '    ')
read fyy fmm fdd fhh trash <<< $(echo ${chunk_end_date}     | tr '_:T-' '    ')

#
#   Either WPS runs or the boundaries and initial conditions are available
#
wps_stored=$(WRF4G.py Chunk get_wps id=${WRF4G_CHUNK_ID}) || wrf4g_exit ${ERROR_ACCESS_DB}

if test ${wps_stored} -eq "1"; then
  echo "* `date`: The boundaries and initial conditions are available ... "
  vcp ${DEBUG} ${WRF4G_DOMAINPATH}/${domain_name}/namelist.wps ${WRF4G_LOCALSCP}/WPS/ || wrf4g_exit ${ERROR_VCP_FAILED} 
  cd ${WRF4G_LOCALSCP}/WRFV3/run || wrf4g_exit ${ERROR_CANNOT_ACCESS_LOCALDIR}
    namelist_wps2wrf ${chunk_restart_date} ${chunk_end_date} ${max_dom} ${chunk_rerun} ${timestep_dxfactor}
    download_file ${DEBUG} real $(date_wrf2iso ${chunk_start_date})
    WRF4G.py Job set_status id=${WRF4G_JOB_ID} ${JOB_STATUS_DOWN_WPS}
else
  echo "* `date`: The boundaries and initial conditions are not available ... "
  cd ${WRF4G_LOCALSCP}/WPS || wrf4g_exit ${ERROR_GETTING_WPS}
    clean_wps

    #
    #   Get geo_em files and namelist.wps
    #
    echo "* `date`: Downloading geo_em files and namelist.wps ... "
    WRF4G.py Job set_status id=${WRF4G_JOB_ID} ${JOB_STATUS_DOWN_BOUND}
    vcp ${DEBUG} ${WRF4G_DOMAINPATH}/${domain_name}/ ${WRF4G_LOCALSCP}/WPS/ || wrf4g_exit ${ERROR_VCP_FAILED}
  
    #
    #   Modify the namelist
    #
    echo "* `date`: Modifying namelist ... "
    fortnml_setn namelist.wps start_date       ${max_dom} "'${chunk_start_date}'"
    fortnml_setn namelist.wps end_date         ${max_dom} "'${chunk_end_date}'"
    fortnml_set  namelist.wps max_dom          ${max_dom}
    fortnml_set  namelist.wps prefix           "'${extdata_vtable}'"
    fortnml_set  namelist.wps interval_seconds ${extdata_interval}

    #
    #  Preprocessor and Ungrib
    #
    echo "* `date`: About to run preprocessor and Ungrib ... "
    run_preprocessor_ungrib
  
    #
    #  Metgrid
    #
    echo "* `date`: Running Metgrid ... "
    run_metgrid  
  
  cd ${WRF4G_LOCALSCP}/WRFV3/run || wrf4g_exit ${ERROR_CANNOT_ACCESS_LOCALDIR}
    #
    #  REAL
    #
    echo "* `date`: Running Real ... "
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
      echo "* `date`: Saving wps ... "
      WRF4G.py Job set_status id=${WRF4G_JOB_ID} ${JOB_STATUS_UPLIAD_WPS}
      post_and_register --no-bg wps "${chunk_start_date}" 
      WRF4G.py Chunk set_wps id=${WRF4G_CHUNK_ID} 1
    fi
fi

#
# Icbcprocessor
#
if test -n "${icbcprocessor}"; then
  echo "* `date`: Running icbcprocessor.${icbcprocessor} ... "
  WRF4G.py Job set_status id=${WRF4G_JOB_ID} ${JOB_STATUS_ICBCPROCESOR}
  icbcprocessor.${icbcprocessor} >&  ${logdir}/icbcproc_${ryy}${rmm}${rdd}${rhh}.out || wrf4g_exit ${ERROR_ICBCPROCESSOR_FAILED}
fi

#
#  WRF
#
echo "* `date`: Running WRF ... "
run_wrf
