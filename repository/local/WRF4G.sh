#!/bin/bash
#
# In order to run this script in verbose mode run the 
# gwsubmit command with the verbose option. DO NOT ADD -x 
# to the first of this script

function verbose_print() { ((VERBOSE)) && echo $@; return 0; }

function date2int() { date=$[1]; echo $date | sed 's/[-_:]//g' }

function prepare_runtime_environment(){
  if (($PPN));then 
    MPI_LAUNCHER="mpirun -np $GW_NP -npernode $PPN $MPI_ENV"
  else
    MPI_LAUNCHER="mpirun -np $GW_NP $MPI_ENV"
  fi
  
  [ ${real_parallel} == 1 ] && LAUNCHER_REAL=${MPI_LAUNCHER} || LAUNCHER_REAL=""
  [ ${wrf_parallel} == 1 ]  && LAUNCHER_WRF=${MPI_LAUNCHER}  || LAUNCHER_WRF=""
  
  # If WRF4G is going to run in a shared folder do not copy the
  # binary and input files. Otherwise copy them.
  [ "${LOCALDIR}" != "${ROOTDIR}" ] && mv ${LOCALDIR}/openmpi  ${ROOTDIR}
  export OPAL_PREFIX=${LOCALDIR}/openmpi
  export PATH=$OPAL_PREFIX/bin:$PATH
  export LD_LIBRARY_PATH=$OPAL_PREFIX/lib:$LD_LIBRARY_PATH
}

function prepare_local_environment (){
  cp -R ${LOCALDIR}/WRFV3/run ${ROOTDIR}/runshared
  $MPI_LAUNCHER -pernode --wdir ${ROOTDIR} ${ROOTDIR}/bin/load_wrfbin.sh	
}

function transfer_output_log (){
  ls -l >& ${logdir}/ls.wrf
  test -f namelist.output         && cp namelist.output ${logdir}/
  test -f ../configure.wrf_wrf    && cp ../configure.wrf_wrf ${logdir}/
  test -f ../configure.wrf_real   && cp ../configure.wrf_real ${logdir}/
  test -f ../../WPS/configure.wps && cp ../../WPS/configure.wps ${logdir}/
  
  test -f namelist.input && cp namelist.input ${logdir}/
  
  echo 
  echo "*********************************
  echo "WRF4G was deployed in ... "
  echo "    $ROOTDIR"
  echo "and it ran in ..."
  echo "    $LOCALDIR"
  echo "*********************************
  echo
  
  logfile="log_${WRF4G_NCHUNK}_${WRF4G_JOB_ID}.tar.gz"
  cd ${logdir}
  tar czf ${logfile} * && vcp ${logfile} ${WRF4G_BASEPATH}/${WRF4G_EXPERIMENT}/${WRF4G_REALIZATION}/log/
  cd -
}

function download_wps (){
  output=$(WRF4G.py Job set_status id=${WRF4G_JOB_ID} ${JOB_STATUS_DOWN_WPS})
  vcp ${DEBUG} ${WRF4G_DOMAINPATH}/${domain_name}/ . 
  [ $? != 0 ] && wrf4g_exit ${ERROR_VCP_FAILED}
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
    preprocessor.${vpreprocessor} ${chunk_start_date} ${chunk_end_date} ${vpath} ${vt} 
    [ $? != 0 ] && wrf4g_exit ${ERROR_PREPROCESSOR_FAILED}
    ./link_grib.sh grbData/*
    
    output=$(WRF4G.py Job set_status id=${WRF4G_JOB_ID} ${JOB_STATUS_UNGRIB})
    verbose_print "* `date`: Running ungrib ... "
    log_ungrib="${logdir}/ungrib_${vt}_${iyy}${imm}${idd}${ihh}.out"
    ungrib.exe >& ${log_ungrib} 
    [ $? != 0 ] && wrf4g_exit ${ERROR_UNGRIB_FAILED}
    cat ${log_ungrib} | grep -q -i 'Successful completion of ungrib' 
    [ $? != 0 ] && wrf4g_exit ${ERROR_UNGRIB_FAILED}
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
  
  log_metgrid="${logdir}/metgrid_${iyy}${imm}${idd}${ihh}.out"
  metgrid.exe >& ${log_metgrid} 
  [ $? != 0 ] && wrf4g_exit ${ERROR_METGRID_FAILED}
  cat ${log_metgrid} | grep -q -i 'Successful completion of metgrid' 
  [ $? != 0 ] && wrf4g_exit ${ERROR_METGRID_FAILED}
  
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
  mv ../../WPS/met_em.d??.????-??-??_??:00:00.nc .
  fix_ptop
  namelist_wps2wrf ${chunk_restart_date} ${chunk_end_date} ${max_dom} ${chunk_rerun} ${timestep_dxfactor}
  
  # If real is run in parallel, prepare the environment.  	 
  [ ${real_parallel} == 1 -a ${LOCALDIR} != ${ROOTDIR} ] && prepare_local_environment
  
  output=$(WRF4G.py Job set_status id=${WRF4G_JOB_ID} ${JOB_STATUS_REAL})
  
  ${LAUNCHER_REAL} ${ROOTDIR}/bin/wrapper.exe real.exe >& ${logdir}/real_${ryy}${rmm}${rdd}${rhh}.out 
  [ $? != 0 ] && wrf4g_exit ${ERROR_REAL_FAILED}

  if [ ${real_parallel} -ne 1 ]; then
    log_file_real=${logdir}/real_${ryy}${rmm}${rdd}${rhh}.out			
  else
    log_file_real=rsl.out.0000
  fi
  
  if test $(grep -c "SUCCESS COMPLETE REAL_EM" ${log_file_real}) -ne 1 ;then
    wrf4g_exit ${ERROR_REAL_FAILED}
  fi

  if test -e rsl.out.0000; then
    mkdir -p  ${logdir}/rsl_real
    mv rsl.* ${logdir}/rsl_real
  fi
  
  # Clean after run
  if test "${clean_after_run}" -eq 1; then
    rm -f met_em*
    rm -f ../../WPS/met_em*
  fi
}

function run_wrf (){
  output=$(WRF4G.py Job set_status id=${WRF4G_JOB_ID} ${JOB_STATUS_WRF})
  # If wrf is run in parallel and the environment is not prepared, prepare it.  	 
  [ ${wrf_parallel} == 1 -a ${LOCALDIR} != ${ROOTDIR} ] && prepare_local_environment
  fortnml -o -f namelist.input -s debug_level 0
  wrf_file=${logdir}/wrf_${ryy}${rmm}${rdd}${rhh}.out
  ${LAUNCHER_WRF} ${ROOTDIR}/bin/wrapper.exe wrf.exe >& ${logdir}/${wrf_file} &
  # Wait enough time to allow 'wrf_wrapper.exe' create 'wrf.pid'
  # This time is also useful to copy the wpsout data
  sleep 30
  
  if [ ${wrf_parallel} -ne 1 ]; then
    log_wrf_file=${logdir}/${wrf_file}
  else
    log_wrf_file=rsl.out.0000
  fi
   
  bash wrf4g_monitor $(cat wrf.pid) ${log_wrf_file} >& ${logdir}/monitor.log &
  echo $! > monitor.pid
  wait $(cat monitor.pid)
   
  if test $(grep -c "SUCCESS COMPLETE WRF" ${log_wrf_file}) -eq 1 ;then
    excode=0
  else
    excode=${ERROR_UNEXPECTED_WRF_TERMINATION}
  fi
  
  if test -e rsl.out.0000; then
    mkdir -p ${logdir}/rsl_wrf
    mv rsl.* ${logdir}/rsl_wrf
  fi
  
  wrf4g_exit $excode
}

function wrf4g_exit(){
  excode=$1
  #
  #  This is the way out of this script. So close the timing info, move the
  #  logs to a safe place and leave
  #
  0)
    status=${JOB_STATUS_FINISHED}
  ;;
   ${ERROR_UNGRIB_FAILED})
    ls -lR >& ${logdir}/ls.wps
    status=${JOB_STATUS_FAILED}
  ;;
  ${EXIT_CANNOT_CONTACT_DB})
    exit ${EXIT_CANNOT_CONTACT_DB}
  ;;
  ${EXIT_CHUNK_ALREADY_FINISHED})
    exit ${EXIT_CHUNK_ALREADY_FINISHED}
  ;;
  ${EXIT_CHUNK_SHOULD_NOT_RUN})
    exit ${EXIT_CHUNK_SHOULD_NOT_RUN}
  ;;
  ${EXIT_PREVIOUS_CHUNK_NOT_FINISHED_CORRECT})
    exit ${EXIT_PREVIOUS_CHUNK_NOT_FINISHED_CORRECT}
  ;;
  ${EXIT_WRF4G_NOT_WORKING})
    exit ${EXIT_WRF4G_NOT_WORKING}
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
    [ ${LOCALDIR} != ${ROOTDIR} ] && rm -rf ${LOCALDIR} 
  fi
  exit ${excode}
}

################# WRF4G initializer ######################
#
#  Tree directory
#
################################
# ├── bin                      #
# ├── lib                      #
# │   ├── bash                 #
# │   └── python               #
# │                            #
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
export ROOTDIR=`pwd`

#
#  Default values
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

#
#  Get variables.
#

export WRF4G_EXPERIMENT=$1
export WRF4G_REALIZATION=$2
export WRF4G_REALIZATION_ID=$3
export WRF4G_NCHUNK=$4
export WRF4G_CHUNK_ID=$5
export chunk_start_date=$6
export chunk_end_date=$7

source ${ROOTDIR}/lib/bash/wrf_util.sh
source ${ROOTDIR}/lib/bash/wrf4g_exit_codes.sh
source ${ROOTDIR}/lib/bash/wrf4g_job_status_code.sh

sed --in-place 's/\ *=\ */=/' experiment.wrf4g
source experiment.wrf4g
sed --in-place 's/\ *=\ */=/' db4g.conf
source db4g.conf 

python wrf4g_expvar -f resources.wrf4g

#
#   Expand the WRF4G scripts
#

tar xzf WRF4G-${WRF4G_VERSION}.tar.gz && rm -f WRF4G-${WRF4G_VERSION}.tar.gz || exit ${ERROR_MISSING_WRF4GSRC}

#   If there are additional files, expand them
if test -f wrf4g_files.tar.gz; then
  verbose_print "* `date`: There is a wrf4g_files.tar.gz package available ... "
  tar xzf wrf4g_files.tar.gz
  mv ${ROOTDIR}/wrf4g_files/* ${ROOTDIR}/bin
  rm -rf wrf4g_files* 
fi
chmod +x ${ROOTDIR}/bin/*

#
#   Load functions and set the PATH
#
export DB4G_CONF=${ROOTDIR}/db4g.conf
export PATH=${ROOTDIR}/bin:${ROOTDIR}/lib/bash:$PATH
export LD_LIBRARY_PATH=${ROOTDIR}/lib:$LD_LIBRARY_PATH
export PYTHONPATH=${ROOTDIR}/lib/python:${ROOTDIR}/bin:$PYTHONPATH

#
#   Update Job Status in DB
#

job_conf="gw_job=${GW_JOB_ID},id_chunk=${WRF4G_CHUNK_ID},resource=${GW_HOSTNAME},wn=$(hostname)"
WRF4G_JOB_ID=$(WRF4G.py --verbose Job load_wn_conf $job_conf $GW_RESTARTED) 
exit_code=$?
[ $exit_code != 0 ] && wrf4g_exit ${exit_code} 

WRF4G_RM_REALIZATION="${WRF4G_BASEPATH}/${WRF4G_EXPERIMENT}/${WRF4G_REALIZATION}"

#   Making remote directories for this job
verbose_print "* `date`: Creating remote output structure ... "

out=$(WRF4G.py --verbose  Realization prepare_remote_storage ${WRF4G_RM_REALIZATION})
[ $out != 0 ] && wrf4g_exit ${ERROR_CANNOT_CREATE_REMOTE_DIR} 

#   Coping Making remote directories for this job
echo "* `date`: Coping configuration files to remote out structure ... "

out=$(WRF4G.py --verbose Realization copy_configuration_files ${WRF4G_RM_REALIZATION})
[ $out != 0 ] && wrf4g_exit ${ERROR_CANNOT_COPY_CONFIGURATION_FILES}

#
#   Should we unpack here or there is a local filesystem for us to run?
#
if test -n "${WRF4G_LOCALSCP}"; then
  export LOCALDIR="${WRF4G_LOCALSCP}/wrf4g.$(date +%Y%m%d%H%M%S%N)"
  mkdir -p ${LOCALDIR} 
  [ $? != 0 ] && wrf4g_exit ${ERROR_CANNOT_ACCESS_LOCALDIR}
  cd ${LOCALDIR}
else
  export LOCALDIR=${ROOTDIR}
fi

#
# GridWay won't remove ROOTDIR directory if clean_after_run is 0
#
[ ${clean_after_run} == 0 ] && touch ${ROOTDIR}/.lock

#   Make log directory
export logdir=${LOCALDIR}/log
mkdir -p ${logdir}
#   Redirect output and error file descriptors
exec &>log/WRF4G.log

#
#  Create remote tree directory 
#

verbose_print "* `date`: Creating remote structure ... "
out=$(WRF4G.py Job set_status id=${WRF4G_JOB_ID} ${JOB_STATUS_DOWN_BIN})

vcp ${DEBUG} ${WRF4G_APPS}/WRF/WRFbin-${WRF_VERSION}.tar.gz . 
[ $? != 0 ] && wrf4g_exit ${ERROR_MISSING_WRFbin}
tar xzf WRFbin-${WRF_VERSION}.tar.gz && rm WRFbin-${WRF_VERSION}.tar.gz

vcp ${DEBUG} ${WRF4G_APPS}/netcdf/netcdf-${NETCDF_VERSION}.tar.gz . 
[ $? != 0 ] && wrf4g_exit ${ERROR_MISSING_NETCDF}
tar xzf netcdf-${NETCDF_VERSION}.tar.gz && rm netcdf-${NETCDF_VERSION}.tar.gz

vcp ${DEBUG} ${WRF4G_APPS}/nco/nco-${NCO_VERSION}.tar.gz . 
[ $? != 0 ] && wrf4g_exit ${ERROR_MISSING_NCO}
tar xzf nco-${NCO_VERSION}.tar.gz && rm nco-${NCO_VERSION}.tar.gz

cp ${ROOTDIR}/namelist.input ${LOCALDIR}/WRFV3/run/namelist.input 
[ $? != 0 ] && wrf4g_exit ${ERROR_MISSING_NAMELIST}

verbose_print "* `date`: Preparing WRF4G binaries ... "
#  Move all executables out of LOCALDIR
mv ${LOCALDIR}/WPS/ungrib/ungrib.exe   ${ROOTDIR}/bin/
mv ${LOCALDIR}/WPS/metgrid/metgrid.exe ${ROOTDIR}/bin/
mv ${LOCALDIR}/WRFV3/run/real.exe      ${ROOTDIR}/bin/
mv ${LOCALDIR}/WRFV3/run/wrf.exe       ${ROOTDIR}/bin/

# To give all access permissions to the group and allow other users 
umask 002

verbose_print "* `date`: Creating parallel environment ... "
prepare_runtime_environment

#
#  Get the restart files if they are necessary.
#
verbose_print "* `date`: Checking restart information ... "
restart_date=$(WRF4G.py Realization get_restart id=${WRF4G_REALIZATION_ID}) 
[ $? != 0 ] && wrf4g_exit ${ERROR_ACCESS_DB}
if test ${restart_date} == "None"; then
  export chunk_restart_date=${chunk_start_date}
  export chunk_rerun=".F."
elif test $(date2int ${restart_date}) -ge $(date2int ${chunk_start_date}) -a $(date2int ${restart_date}) -le $(date2int ${chunk_end_date}); then
  export chunk_restart_date=${restart_date}
  export chunk_rerun=".T."  
  output=$(WRF4G.py Job set_status id=${WRF4G_JOB_ID} ${JOB_STATUS_DOWN_RESTART})
  download_file rst $(date_wrf2iso ${restart_date}) 
  [ $? != 0 ] && wrf4g_exit ${ERROR_RST_DOWNLOAD_FAILED}
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
[ $? != 0 ] && wrf4g_exit ${ERROR_ACCESS_DB}

cd ${LOCALDIR}/WPS || wrf4g_exit ${ERROR_GETTING_WPS}
if test ${wps_stored} -eq "1"; then
  verbose_print "* `date`: The boundaries and initial conditions are available ... "
  vcp ${DEBUG} ${WRF4G_DOMAINPATH}/${domain_name}/namelist.wps . 
  [ $? != 0 ] && wrf4g_exit ${ERROR_VCP_FAILED} 
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
  
  cd ${LOCALDIR}/WRFV3/run 
  [ $? != 0 ] && wrf4g_exit ${ERROR_CANNOT_ACCESS_LOCALDIR}
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
  verbose_print "* `date`: Running icbcprocessor.${icbcprocessor} ... "
  WRF4G.py Job set_status id=${WRF4G_JOB_ID} ${JOB_STATUS_ICBCPROCESOR}
  icbcprocessor.${icbcprocessor} >&  ${logdir}/icbcproc_${ryy}${rmm}${rdd}${rhh}.out
  [ $? != 0 ] && wrf4g_exit ${ERROR_ICBCPROCESSOR_FAILED}
fi


#
#  WRF
#    
verbose_print "* `date`: Running WRF ... "
run_wrf

