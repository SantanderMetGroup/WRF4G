#! /bin/bash -x

# WRF4G_ini.sh
#

function load_default_config (){
 #
 #  Some default values
 #
 save_wps=1
 clean_after_run=1
 timestep_dxfactor=6
 real_parallel=0
 wrf_parallel=1
 VCPDEBUG="-v"
 global_preprocessor="default"
}

function date2int(){
  date=$[1]
  echo $date| sed 's/[-_:]//g'
}

function WRF4G_prepare (){
  #
  #  Move all executables out of LOCALDIRvcp
  #
  mv ${LOCALDIR}/WPS/ungrib/ungrib.exe   ${ROOTDIR}/bin/
  mv ${LOCALDIR}/WPS/metgrid/metgrid.exe ${ROOTDIR}/bin/
  mv ${LOCALDIR}/WPS/preprocessor.*      ${ROOTDIR}/bin/
  mv ${LOCALDIR}/WRFV3/run/real.exe      ${ROOTDIR}/bin/
  mv ${LOCALDIR}/WRFV3/run/wrf.exe       ${ROOTDIR}/bin/
  mv ${LOCALDIR}/WRFV3/run/icbcprocessor.*  ${ROOTDIR}/bin/
  mv ${LOCALDIR}/WRFV3/run/postprocessor.*  ${ROOTDIR}/bin/
  umask 002

}

function prepare_runtime_environment(){
    # Does any component run in parallel?
    prepare_openmpi=0
    local_openmpi=0
    LAUNCHER_REAL="";LAUNCHER_WRF=""
    MPI_LAUNCHER="mpirun -np $GW_NP $MPI_ENV"

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
       if test -n "${WRF4G_RUN_LOCAL}"; then    
          local_openmpi=1	   
       fi
    fi
    export OPAL_PREFIX=${ROOTDIR}/openmpi
    export PATH=$OPAL_PREFIX/bin:$PATH
    export LD_LIBRARY_PATH=$OPAL_PREFIX/lib:$LD_LIBRARY_PATH
}

function prepare_local_environment (){
  #cp ${LOCALDIR}/WRF4Gbin-${WRF_VERSION}.tar.gz ${ROOTDIR}
  mv ${LOCALDIR}/openmpi  ${ROOTDIR}/
  $MPI_LAUNCHER -pernode --wdir ${ROOTDIR} ${ROOTDIR}/WRFGEL/load_wrfbin.sh	
  #rm ${ROOTDIR}/WRF4Gbin-${WRF_VERSION}.tar.gz
  #rm {LOCALDIR}/WRF4Gbin-${WRF_VERSION}.tar.gz
  prepare_openmpi=0
  #mpiexec -mca btl self,sm,tcp --wdir /localtmp --preload-files-dest-dir /localtmp --preload-files /localtmp/WRF4Gbin-3.1.1_r484INTEL_OMPI.tar.gz --preload-binary /localtmp/wrf.sh /localtmp/wrf.sh
}


function wrf4g_exit(){
  excode=$1
  #
  #  This is the way out of this script. So close the timing info, move the
  #  logs to a safe place and leave
  #

  case $excode in
    0)
       if test $(grep -c "SUCCESS COMPLETE WRF" rsl.out.0000) -eq 1 ;then
          WRF4G.py Job set_status id=${WRF4G_JOB_ID} 40
       else
	  WRF4G.py Job set_status id=${WRF4G_JOB_ID} 41
	  extcode=${ERROR_UNEXPECTED_WRF_TERMINATION}
       fi
       ;;
    ${ERROR_UNGRIB_FAILED})
       ls -lR >& ${logdir}/ls.wps
       WRF4G.py Job set_status id=${WRF4G_JOB_ID} 41
       ;;
    *)
       WRF4G.py Job set_status id=${WRF4G_JOB_ID} 41
       ;;
  esac
  WRF4G.py Job set_exitcode id=${WRF4G_JOB_ID} ${excode}

  ls -l >& ${logdir}/ls.wrf
  test -f namelist.output && cp namelist.output ${logdir}/
  if test -e rsl.out.0000; then
    mkdir -p rsl_wrf
    mv rsl.* rsl_wrf/
    mv rsl_wrf ${logdir}/
  fi
  test -f namelist.input && cp namelist.input ${logdir}/
  logfile="log_${WRF4G_NCHUNK}_${WRF4G_JOB_ID}.tar.gz"
  cd ${logdir}
  echo ROOTDIR: $ROOTDIR
  echo LOCALDIR: $LOCALDIR
  tar czf ${logfile} * && vcp ${logfile} ${WRF4G_BASEPATH}/${experiment_name}/${WRF4G_REALIZATION}/log/
  cd -
  # Clean the heavy stuff
  if test "${clean_after_run}" -eq 1; then
    cd ${ROOTDIR}
    test "${LOCALDIR}" != "${ROOTDIR}" && rm -rf ${LOCALDIR} 
  fi
  exit ${excode}
}


# WRF4G initializer
#
#
#  Change working directory to WRF4G_RUN_SHARED if needed 
#
tar xzf sandbox.tar.gz resources.wrf4g experiment.wrf4g

load_default_config

#
#  Load experiment.wrf4g
#
source resources.wrf4g                        || exit 2
sed -e 's/\ *=\ */=/' experiment.wrf4g > source.it  || exit 2
source source.it && rm source.it
rm resources.wrf4g experiment.wrf4g

export WRF4G_EXPERIMENT="${experiment_name}"
export WRF4G_REALIZATION=$1
export WRF4G_ID_REALIZATION=$2
export WRF4G_NCHUNK=$3
export WRF4G_ID_CHUNK=$4
export chunk_start_date=$5
export chunk_end_date=$6

#
#  Change ROOTDIR if necesary
#
ROOTDIR=$(pwd)
if test -n "${WRF4G_RUN_SHARED}"; then
  mkdir -p ${WRF4G_RUN_SHARED}/${WRF4G_EXPERIMENT}/${WRF4G_REALIZATION}/${WRF4G_NCHUNK}
  cd ${WRF4G_RUN_SHARED}/${WRF4G_EXPERIMENT}/${WRF4G_REALIZATION}/${WRF4G_NCHUNK}
  mv ${ROOTDIR}/sandbox.tar.gz .
fi 
ROOTDIR=$(pwd)
#
#  Expand the sandbox files
#
tar xzf sandbox.tar.gz 
rm sandbox.tar.gz
#
#   Expand the WRF4G scripts
#
export RESOURCES_WRF4G="${ROOTDIR}/resources.wrf4g"
export DB4G_CONF="${ROOTDIR}/db4g.conf"
export PATH="${ROOTDIR}/bin:$PATH"
export LD_LIBRARY_PATH=${ROOTDIR}/lib/shared_libs:$LD_LIBRARY_PATH
export PYTHONPATH=${ROOTDIR}/lib/python:${ROOTDIR}/lib/shared_libs:$PYTHONPATH
chmod +x ${ROOTDIR}/bin/* 
vcp ${WRF4G_APPS}/WRF4G-${WRF4G_VERSION}.tar.gz . || exit 40 # We do not have yet the wrf4g_exit_codes ${ERROR_MISSING_WRF4GSRC}
tar xzf WRF4G-${WRF4G_VERSION}.tar.gz && rm -f WRF4G-${WRF4G_VERSION}.tar.gz  || exit 40
chmod +x ${ROOTDIR}/bin/* 


#
#  Load functions and set the PATH
#
source ${ROOTDIR}/lib/bash/wrf_util.sh
source ${ROOTDIR}/lib/bash/wrf4g_exit_codes.sh
export PATH="${ROOTDIR}/WRFGEL:${ROOTDIR}/lib/bash:$PATH"
chmod +x ${ROOTDIR}/WRFGEL/*

# Update Job Status in DB
job_conf="gw_job=${GW_JOB_ID},id_chunk=${WRF4G_ID_CHUNK},resource=${GW_HOSTNAME},wn=$(hostname)"
WRF4G_JOB_ID=$(WRF4G.py Job load_wn_conf  $job_conf $GW_RESTARTED) #|| wrf4g_exit ${ERROR_LOW_GW_RESTARTED}
if test $? -ne 0; then
    exit 92
fi

#
#   Should we unpack here or there is a local filesystem for us to run?
#
if test -z "${WRF4G_RUN_LOCAL}"; then
    if test -n "${GW_LOCALDIR}";then
        ${WRF4G_RUN_LOCAL}=${GW_LOCALDIR}
    fi
fi

if test -n "${WRF4G_RUN_LOCAL}"; then
  if test "${WRF4G_RUN_LOCAL:0:4}" = "var:" ; then
    eval "WRF4G_RUN_LOCAL=\$$(echo ${WRF4G_RUN_LOCAL} | sed -e 's/var://')"
  fi
  LOCALDIR="${WRF4G_RUN_LOCAL}/wrf4g.$(date +%Y%m%d%H%M%S%N)"
  mkdir ${LOCALDIR} || wrf4g_exit ${ERROR_CANNOT_ACCESS_LOCALDIR}
  cd ${LOCALDIR}
else
  LOCALDIR=${ROOTDIR}
fi
export ROOTDIR
export LOCALDIR=${PWD}
echo ${ROOTDIR} > rootdir
echo ${PWD} > ${ROOTDIR}/localdir


#
#  Create WRF4G framework structure
#
logdir=${LOCALDIR}/log
mkdir -p ${logdir}

# Redirect output and error file descriptors
exec &>log/WRF4G.log

WRF4G.py Job set_status id=${WRF4G_JOB_ID} 11
#WRF4G.py Chunk should_I_run id=${WRF4G_ID_CHUNK} || wrf4g_exit ${ERROR_PREVIOUS_CHUNK_NOT_FINISHED}
  
  
vcp ${WRF4G_APPS}/WRF4Gbin-${WRF_VERSION}.tar.gz .
tar xzf WRF4Gbin-${WRF_VERSION}.tar.gz || wrf4g_exit ${ERROR_MISSING_WRF4GBIN}
rm WRF4Gbin-${WRF_VERSION}.tar.gz

vcp ${WRF4G_BASEPATH}/${experiment_name}/${WRF4G_REALIZATION}/namelist.input  WRFV3/run/namelist.input || wrf4g_exit ${ERROR_MISSING_NAMELIST}

#
#  If there are additional files, expand'em
#
vcp ${WRF4G_BASEPATH}/${experiment_name}/wrf4g_files.tar.gz ${ROOTDIR} &>/dev/null
if test -f ${ROOTDIR}/wrf4g_files.tar.gz; then
  tar xzvf ${ROOTDIR}/wrf4g_files.tar.gz && rm ${ROOTDIR}/wrf4g_files.tar.gz
fi

WRF4G_prepare

prepare_runtime_environment

#
#  Get the restart files if necesary.
#
restart_date=$(WRF4G.py -v Realization get_restart id=${WRF4G_ID_REALIZATION}) || wrf4g_exit ${ERROR_ACCESS_DB}
if  test ${restart_date} == "None"; then
   export chunk_restart_date=${chunk_start_date}
   export chunk_rerun=".F."
elif test $(date2int ${restart_date}) -ge $(date2int ${chunk_start_date}) -a $(date2int ${restart_date}) -le $(date2int ${chunk_end_date}); then
   export chunk_restart_date=${restart_date}
   export chunk_rerun=".T."  
   WRF4G.py Job set_status id=${WRF4G_JOB_ID} 12
   download_file rst $(date_wrf2iso ${restart_date}) || wrf4g_exit ${ERROR_RST_DOWNLOAD_FAILED}
   mv wrfrst* WRFV3/run 
else
   wrf4g_exit ${EXIT_CHUNK_SHOULD_NOT_RUN}
fi

read iyy imm idd ihh trash <<< $(echo ${chunk_start_date} | tr '_:T-' '    ')
read ryy rmm rdd rhh trash <<< $(echo ${chunk_restart_date} | tr '_:T-' '    ')
read fyy fmm fdd fhh trash <<< $(echo ${chunk_end_date}   | tr '_:T-' '    ')

#
#   Must WPS run or are the boundaries available?
#
wps_stored=$(WRF4G.py -v Chunk get_wps id=${WRF4G_ID_CHUNK}) || wrf4g_exit ${ERROR_ACCESS_DB}

if test ${wps_stored} -eq "1"; then
  cd ${LOCALDIR}/WPS || wrf4g_exit ${ERROR_GETTING_WPS}
    vcp ${VCPDEBUG} ${WRF4G_DOMAINPATH}/${domain_name}/namelist.wps . || wrf4g_exit ${ERROR_VCP_FAILED} namelist.wps
  cd ${LOCALDIR}/WRFV3/run || wrf4g_exit ${ERROR_GETTING_WPS}
    namelist_wps2wrf ${chunk_restart_date} ${chunk_end_date} ${max_dom} ${chunk_rerun} ${timestep_dxfactor}
    WRF4G.py Job set_status id=${WRF4G_JOB_ID} 20
    download_file wps $(date_wrf2iso ${chunk_start_date})
  cd ${LOCALDIR}
else
  cd ${LOCALDIR}/WPS || wrf4g_exit ${ERROR_GETTING_WPS}
    clean_wps
    #
    #   Get geo_em files and namelist.wps
    #
    vcp ${VCPDEBUG} ${WRF4G_DOMAINPATH}/${domain_name}/'*' . || wrf4g_exit ${ERROR_VCP_FAILED} ${domain_name}

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
    WRF4G.py Job set_status id=${WRF4G_JOB_ID} 21

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
      ./link_grib.sh grbData/*.grb
    WRF4G.py Job set_status id=${WRF4G_JOB_ID} 22
      ln -sf ungrib/Variable_Tables/Vtable.${global_name} Vtable
      ${ROOTDIR}/bin/ungrib.exe \
        >& ${logdir}/ungrib_${global_name}_${iyy}${imm}${idd}${ihh}.out \
        || wrf4g_exit ${ERROR_UNGRIB_FAILED}
      cat ${logdir}/ungrib_${global_name}_${iyy}${imm}${idd}${ihh}.out \
        | grep -q -i 'Successful completion of ungrib' || wrf4g_exit ${ERROR_UNGRIB_FAILED}


    
   #
   # Check for other preprocessors and apply them.
   #
   for preprocessor_other in ${preprocessor_others//,/ }
   do
     fortnml --overwrite -f namelist.wps -s prefix@ungrib ${preprocessor_other}
     #No global path. Path to the data must be included inside the postprocessor.
     preprocessor.${preprocessor_other} ${chunk_start_date} ${chunk_end_date} ${global_name}
     rm -f GRIBFILE.*
    ./link_grib.sh grbOtherData/*.grb
    ${ROOTDIR}/bin/ungrib.exe >& ${logdir}/ungrib_${preprocessor_other}_${iyy}${imm}${idd}${ihh}.out || wrf4g_exit ${ERROR_UNGRIB_FAILED}
    cat ${logdir}/ungrib_${preprocessor_other}_${iyy}${imm}${idd}${ihh}.out \
    | grep -q -i 'Successful completion of ungrib' \
    || wrf4g_exit ${ERROR_UNGRIB_FAILED}
   done
   # List of prefixes for metgrid. If same field is found in two or more input sources,
   # the last encountered will take priority.
   fortnml --overwrite -f namelist.wps -s fg_name@metgrid "'${global_name}','${preprocessor_others}'" 


    #
    #   Run metgrid
    #
    WRF4G.py Job set_status id=${WRF4G_JOB_ID} 23
      fortnml_vardel namelist.wps opt_output_from_metgrid_path
      fortnml_vardel namelist.wps opt_output_from_geogrid_path
      fortnml_vardel namelist.wps opt_metgrid_tbl_path
      fortnml_vardel namelist.wps opt_geogrid_tbl_path
      fortnml_setn namelist.wps start_date ${max_dom} "'${chunk_start_date}'"
      fortnml_setn namelist.wps end_date   ${max_dom} "'${chunk_end_date}'"
      fortnml -o -f namelist.wps -s fg_name ${global_name}
      ${ROOTDIR}/bin/metgrid.exe \
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
  cd ${LOCALDIR}/WRFV3/run 
    #------------------------------------------------------------------
    #                              REAL
    #------------------------------------------------------------------
   WRF4G.py Job set_status id=${WRF4G_JOB_ID} 24
      clean_real
      ln -s ../../WPS/met_em.d??.????-??-??_??:00:00.nc .
      fix_ptop
      namelist_wps2wrf ${chunk_restart_date} ${chunk_end_date} ${max_dom} ${chunk_rerun} ${timestep_dxfactor}
      
      # If real is run in parallel, prepare the environment.  	 
      if [ ${local_openmpi} -eq 1 -a ${real_parallel} -eq 1 ]; then
          prepare_local_environment
      fi
      ${LAUNCHER_WRF} ${ROOTDIR}/bin/wrapper.exe real.exe >& ${logdir}/real_${ryy}${rmm}${rdd}${rhh}.out || wrf4g_exit ${ERROR_REAL_FAILED}
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

    #
    #  Upload the wpsout files (create the output structure if necessary):
    #
    #    wrfinput_d0?
    #    wrfbdy_d0?
    #    wrflowinp_d0?
    #    wrffdda_d0?
    #

    if test "${save_wps}" -eq 1; then
      WRF4G.py Job set_status id=${WRF4G_JOB_ID} 25
        post_and_register --no-bg wps "${chunk_start_date}"
        WRF4G.py Chunk set_wps id=${WRF4G_ID_CHUNK} 1
    fi
  cd ${LOCALDIR} || wrf4g_exit $ERROR_CANNOT_ACCESS_LOCALDIR
fi
    
    


#------------------------------------------------------------------
#                              WRF
#------------------------------------------------------------------
cd ${LOCALDIR}/WRFV3/run || wrf4g_exit ERROR_CANNOT_ACCESS_LOCALDIR
  if test -n "${icbcprocessor}"; then
    WRF4G.py Job set_status id=${WRF4G_JOB_ID} 26
      icbcprocessor.${icbcprocessor} >&  ${logdir}/icbcproc_${ryy}${rmm}${rdd}${rhh}.out
  fi
    
    # If wrf is run in parallel and the environment is not prepared, prepare it.  	 
    if [ ${local_openmpi} -eq 1 -a ${wrf_parallel} -eq 1  ]; then
        prepare_local_environment
    fi
	
    fortnml -o -f namelist.input -s debug_level 0
    WRF4G.py Job set_status id=${WRF4G_JOB_ID} 29
    ${LAUNCHER_WRF} ${ROOTDIR}/bin/wrapper.exe wrf.exe >& ${logdir}/wrf_${ryy}${rmm}${rdd}${rhh}.out &
    # Wait enough time to allow 'wrf_wrapper.exe' create 'wrf.pid'
    # This time is also useful to copy the wpsout data
    sleep 30
    ps -ef | grep wrf.exe
    bash -x wrf4g_monitor $(cat wrf.pid) >& ${logdir}/monitor.log &
    echo $! > monitor.pid   
    wait $(cat monitor.pid)

  wrf4g_exit 0


