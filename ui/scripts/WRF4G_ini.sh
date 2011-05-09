#! /bin/bash

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
}

function date2int(){
  date=$[1]
  echo $date| sed 's/[-_:]//g'
}

function w4gini_exit(){
  excode=$1
  case ${excode} in
    ${ERROR_GETDATERST_FAILED})
      echo "Problems getting the restart date... (not even -1)"
      ;;
    ${EXIT_CHUNK_ALREADY_FINISHED})
      echo "This chunk already run! Ciao..." >> WRF4G_ini.err
      ;;
    ${EXIT_RESTART_MISMATCH})
      echo "Something went wrong! (the restart file is not available and the chunk is a restart...)" >> WRF4G_ini.err
      ;;
    ${EXIT_CHUNK_SHOULD_NOT_RUN})
      echo "The date of the simulation did not reach this chunk yet! Ciao..." >> WRF4G_ini.err
      ;;
    ${ERROR_MISSING_WRF4GBIN})
      echo "Could not find the WRF binary file: WRF4Gbin-${WRF_VERSION}.tar.gz " >> WRF4G_ini.err
      ;;
    ${ERROR_LOW_GW_RESTARTED})
      echo "This Job should not run this chunk " >> WRF4G_ini.err
      ;;
    ${ERROR_MISSING_NAMELIST})
      echo "Could not find the namelist" >> WRF4G_ini.err
      ;;
    ${ERROR_ACCESS_DB})
      echo "Could not access DB" >> WRF4G_ini.err
      ;;
    ${ERROR_CANNOT_ACCESS_LOCALDIR})
    echo "Cannot access $LOCALDIR" >> WRF4G_ini.err
      ;;
    ${ERROR_VCP_FAILED})
        echo "Error Copying file $2" >> WRF4G_ini.err
      ;;
  esac
  exit ${excode}
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
    MPI_LAUNCHER="mpirun $MPI_ENV"

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


function timelog_clean(){
  rm -f ${logdir}/time.logERROR_RETRIEVE_RESTART_DATE
}

function timelog_end(){
  echo "$(date +%Y%m%d%H%M%S)" > ${timelog_item}.end && vcp ${timelog_item}.end ${WRF4G_BASEPATH}/experiments/${experiment_name}/${WRF4G_REALIZATION}/
  date +%Y%m%d%H%M%S >> ${logdir}/time.log
}

function timelog_init(){
  timelog_item=${1// /_}
  echo -e "$(date +%Y%m%d%H%M%S)\n$(hostname --fqdn):$(pwd)" > ${timelog_item}.init && vcp ${timelog_item}.init ${WRF4G_BASEPATH}/experiments/${experiment_name}/${WRF4G_REALIZATION}/
  echo -n "$(printf "%20s" "$timelog_item") $(date +%Y%m%d%H%M%S) " >> ${logdir}/time.log
}

function wrf4g_exit(){
  excode=$1
  #
  #  This is the way out of this script. So close the timing info, move the
  #  logs to a safe place and leave
  #

  echo "exit $excode" >> ${logdir}/time.log
  ls -l >& ${logdir}/ls.wrf
  if test -e rsl.out.0000; then
    mkdir -p rsl_wrf
    mv rsl.* rsl_wrf/
    mv rsl_wrf ${logdir}/
  fi
  case $excode in
    0)
       timelog_end
       WRF4G.py Job set_status id=${WRF4G_JOB_ID} 40
       ;;
    ${ERROR_UNGRIB_FAILED})
       ls -lR >& ${logdir}/ls.wps
       WRF4G.py Job set_status id=${WRF4G_JOB_ID} 41
       ;;
    *)
       WRF4G.py Job set_status id=${WRF4G_JOB_ID} 41
       ;;
  esac

  test -f namelist.input && cp namelist.input ${logdir}/
  tar czf log.tar.gz.${WRF4G_JOB_ID} ${logdir} && vcp log.tar.gz ${WRF4G_BASEPATH}/experiments/${experiment_name}/${WRF4G_REALIZATION}/
  test "${LOCALDIR}" != "${ROOTDIR}" && mv ${logdir} ${ROOTDIR}/
  exit ${excode}
}


# WRF4G initializer
#
#
#  Change working directory to WRF4G_RUN_SHARED if needed 
#
tar xzf sandbox.tar.gz wrf4g.conf wrf.input

load_default_config

#
#  Load wrf.input and wrf.chunk
#
source wrf4g.conf                            || exit ${ERROR_MISSING_WRF4GCNF}
sed -e 's/\ *=\ */=/' wrf.input > source.it  || exit ${ERROR_MISSING_WRFINPUT}
source source.it && rm source.it
rm wrf4g.conf wrf.input

export WRF4G_EXPERIMENT="${experiment_name}"
export WRF4G_REALIZATION=$1
export WRF4G_ID_REALIZATION=$2
export WRF4G_CHUNK=$3
export WRF4G_CHUNK_ID=$4
export chunk_start_date=$5
export chunk_end_date=$6


#
#  Change ROOTDIR if necesary
#
ROOTDIR=$(pwd)
if test -n "${WRF4G_RUN_SHARED}"; then
  mkdir -p ${WRF4G_RUN_SHARED}/${WRF4G_EXPERIMENT}/${WRF4G_REALIZATION}/${WRF4G_CHUNK}
  cd ${WRF4G_RUN_SHARED}/${WRF4G_EXPERIMENT}/${WRF4G_REALIZATION}/${WRF4G_CHUNK}
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
export WRF4G_CONF_FILE="${ROOTDIR}/wrf4g.conf"
export PATH="${ROOTDIR}/bin:$PATH"
export LD_LIBRARY_PATH=${ROOTDIR}/lib/shared_libs:$LD_LIBRARY_PATH
export PYTHONPATH=${ROOTDIR}/lib/python:${ROOTDIR}/lib/shared_libs:$PYTHONPATH
chmod +x ${ROOTDIR}/bin/*
vcp ${WRF4G_APPS}/WRF4G-${WRF4G_VERSION}.tar.gz . || exit 40 # We do not have yet the wrf4g_exit_codes ${ERROR_MISSING_WRF4GSRC}
tar xzf WRF4G-${WRF4G_VERSION}.tar.gz && rm -f WRF4G-${WRF4G_VERSION}.tar.gz  || exit 40


#
#  Load functions and set the PATH
#
source ${ROOTDIR}/lib/bash/wrf_util.sh
source ${ROOTDIR}/lib/bash/wrf4g_exit_codes.sh
export PATH="${ROOTDIR}/WRFGEL:${ROOTDIR}/lib/bash:$PATH"
chmod +x ${ROOTDIR}/WRFGEL/*

# Update Job Status in DB
job_conf="gw_job=${GW_JOB_ID},id_chunk=${WRF4G_CHUNK},resource=${GW_HOSTNAME},wn=$(hostname)"
WRF4G_JOB_ID=$(WRF4G.py Job load_wn_conf  $job_conf $GW_RESTARTED) || w4gini_exit ${ERROR_LOW_GW_RESTARTED}

#
#   Should we unpack here or there is a local filesystem for us to run?
#
if test -n "${WRF4G_RUN_LOCAL}"; then
  if test "${WRF4G_RUN_LOCAL:0:4}" = "var:" ; then
    eval "WRF4G_RUN_LOCAL=\$$(echo ${WRF4G_RUN_LOCAL} | sed -e 's/var://')"
  fi
  LOCALDIR="${WRF4G_RUN_LOCAL}/wrf4g.$(date +%Y%m%d%H%M%S%N)"
  mkdir ${LOCALDIR} || w4gini_exit ${ERROR_CANNOT_ACCESS_LOCALDIR}
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
mkdir ${logdir}

# Redirect output and error file descriptors
exec &>log/WRF4G.log

WRF4G.py Job set_status id=${WRF4G_JOB_ID} 11
vcp ${WRF4G_APPS}/WRF4Gbin-${WRF_VERSION}.tar.gz .
tar xzf WRF4Gbin-${WRF_VERSION}.tar.gz || w4gini_exit ${ERROR_MISSING_WRF4GBIN}
rm WRF4Gbin-${WRF_VERSION}.tar.gz

vcp  ${WRF4G_BASEPATH}/experiments/${experiment_name}/${WRF4G_REALIZATION}/namelist.input  WRFV3/run/namelist.input || w4gini_exit ${ERROR_MISSING_NAMELIST}

#
#  If there are additional files, expand'em
#
if test -f ${ROOTDIR}/wrf4g_files.tar.gz; then
  tar xzvf ${ROOTDIR}/wrf4g_files.tar.gz && rm ${ROOTDIR}/wrf4g_files.tar.gz
fi

WRF4G_prepare

prepare_runtime_environmentEXIT_CHUNK_SHOULD_NOT_RUN

timelog_clean

#
#  Get the restart files if necesary.
#
restart_date=$(WRF4G.py -v Realization get_restart name=${WRF4G_REALIZATION}) || w4gini_exit ${ERROR_ACCESS_DB}
if  test ${restart_date} == "None"; then
   export chunk_restart_date=${chunk_start_date}
   export chunk_is_restart=".F."
elif test date2int ${restart_date} -ge date2int ${chunk_start_date} -a date2int ${restart_date} -le date2int ${chunk_end_date}; then
   export chunk_restart_date=${chunk_restart_date}
   export chunk_is_restart=".T."  
   WRF4G.py Job set_status id=${WRF4G_JOB_ID} 12
   download_file rst ${restart_date} || exit ${ERROR_RST_DOWNLOAD_FAILED}
   mv ${ROOTDIR}/wrfrst* WRFV3/run >& /dev/null 
else
   w4gini_exit ${EXIT_CHUNK_SHOULD_NOT_RUN}
fi


#
#   Must WPS run or are the boundaries available?
#
wps_stored=$(WRF4G.py -v Chunk get_wps id=${WRF4G_CHUNK_ID}) || w4gini_exit ${ERROR_ACCESS_DB}

if test ${wps_stored} -eq "1"; then
  cd ${LOCALDIR}/WPS || exit
    vcp ${VCPDEBUG} ${WRF4G_DOMAINPATH}/${domain_name}/namelist.wps . || w4gini_exit ${ERROR_VCP_FAILED} namelist.wps
  cd ${LOCALDIR}/WRFV3/run || exit
    namelist_wps2wrf ${chunk_restart_date} ${chunk_end_date} ${max_dom} ${chunk_is_restart} ${timestep_dxfactor}
    WRF4G.py Job set_status id=${WRF4G_JOB_ID} 20
    timelog_init "wps get"
      download_file wps $(date_wrf2iso ${chunk_start_date})
    timelog_end
  cd ${LOCALDIR}
else
  cd ${LOCALDIR}/WPS || exit
    clean_wps
    #
    #   Get geo_em files and namelist.wps
    #
    vcp ${VCPDEBUG} ${WRF4G_DOMAINPATH}/${domain_name}/'*' . || w4gini_exit ${ERROR_VCP_FAILED} ${domain_name}

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
      ./link_grib.csh grbData/*.grb
    timelog_end
    WRF4G.py Job set_status id=${WRF4G_JOB_ID} 22
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
   # Check for other preprocessors and apply them.
   #
   for preprocessor_other in ${preprocessor_others//,/ }
   do
     fortnml --overwrite -f namelist.wps -s prefix@ungrib ${preprocessor_other}
     #No global path. Path to the data must be included inside the postprocessor.
     preprocessor.${preprocessor_other} ${chunk_start_date} ${chunk_end_date} ${global_name}
     rm -f GRIBFILE.*
    ./link_grib.csh grbOtherData/*.grb
    timelog_init "ungrib_others"
    ${ROOTDIR}/bin/ungrib.exe >&
    ${logdir}/ungrib_${preprocessor_other}_${iyy}${imm}${idd}${ihh}.out || wrf4g_exit ${ERROR_UNGRIB_FAILED}
    cat ${logdir}/ungrib_${preprocessor_other}_${iyy}${imm}${idd}${ihh}.out \
    | grep -q -i 'Successful completion of ungrib' \
    || wrf4g_exit ${ERROR_UNGRIB_FAILED}
    timelog_end
   done
   # List of prefixes for metgrid. If same field is found in two or more input sources,
   # the last encountered will take priority.
   fortnml --overwrite -f namelist.wps -s fg_name@metgrid "'${global_name}','${preprocessor_others}'" 

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
    WRF4G.py Job set_status id=${WRF4G_JOB_ID} 23
    timelog_init "metgrid"
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
    timelog_end
  cd ${LOCALDIR}/WRFV3/run || exit
    #------------------------------------------------------------------
    #                              REAL
    #------------------------------------------------------------------
   WRF4G.py Job set_status id=${WRF4G_JOB_ID} 24
   timelog_init "real"
      clean_real
      ln -s ../../WPS/met_em.d??.????-??-??_??:00:00.nc .
      fix_ptop
      namelist_wps2wrf ${chunk_restart_date} ${chunk_end_date} ${max_dom} ${chunk_is_restart} ${timestep_dxfactor}
      
      # If real is run in parallel, prepare the environment.  	 
      if [ ${local_openmpi} -eq 1 -a ${real_parallel} -eq 1 ]; then
          prepare_local_environment
      fi
	 
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
      WRF4G.py Job set_status id=${WRF4G_JOB_ID} 25
      timelog_init "wps put"
        post_and_register --no-bg wps "${chunk_start_date}"
        WRF4G.py Chunk set_wps id=${WRF4G_CHUNK_ID} 1
      timelog_end
    fi
  cd ${LOCALDIR} || exit
fi
    
    


#------------------------------------------------------------------
#                              WRF
#------------------------------------------------------------------
cd ${LOCALDIR}/WRFV3/run || exit
  if test -n "${icbcprocessor}"; then
    WRF4G.py Job set_status id=${WRF4G_JOB_ID} 26
    timelog_init "icbcprocessor"
      icbcprocessor.${icbcprocessor} >&  ${logdir}/icbcproc_${ryy}${rmm}${rdd}${rhh}.out
    timelog_end
  fi
  timelog_init "wrf"
    
    # If wrf is run in parallel and the environment is not prepared, prepare it.  	 
    if [ ${local_openmpi} -eq 1 -a ${wrf_parallel} -eq 1  ]; then
        prepare_local_environment
    fi
	
    fortnml -o -f namelist.input -s debug_level 0
    WRF4G.py Job set_status id=${WRF4G_JOB_ID} 29
    ${LAUNCHER_WRF} ${ROOTDIR}/bin/wrf_wrapper.exe >& ${logdir}/wrf_${ryy}${rmm}${rdd}${rhh}.out &
    # Wait enough time to allow 'wrf_wrapper.exe' create 'wrf.pid'
    # This time is also useful to copy the wpsout data
    sleep 30
    ps -ef | grep wrf.exe
    wrf4g_monitor $(cat wrf.pid) >& ${logdir}/monitor.log &
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


