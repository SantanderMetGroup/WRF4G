#! /bin/bash
#
# WRF4G.sh
#
# 

set -v
VCPDEBUG="-v"

function load_default_config (){
 #
 #  Some default values
 #
 save_wps=1
 clean_after_run=1
 timestep_dxfactor=6
 metgrid_parallel=0
 ungrib_parallel=0
 real_parallel=0
 wrf_parallel=1
}


function WRF4G_prepare (){
  #
  #  Move all executables out of LOCALDIR
  #
  mv ${LOCALDIR}/WPS/ungrib/ungrib.exe   ${ROOTDIR}/bin/
  mv ${LOCALDIR}/WPS/metgrid/metgrid.exe ${ROOTDIR}/bin/
  mv ${LOCALDIR}/WPS/preprocessor.*      ${ROOTDIR}/bin/
  mv ${LOCALDIR}/WRFV3/run/real.exe      ${ROOTDIR}/bin/
  mv ${LOCALDIR}/WRFV3/run/wrf.exe       ${ROOTDIR}/bin/
  mv ${LOCALDIR}/WRFV3/run/icbcprocessor.*  ${ROOTDIR}/bin/
  mv ${LOCALDIR}/WRFV3/run/postprocessor.*  ${ROOTDIR}/bin/
  umask 002

  test -n "${logdir}" || logdir=${LOCALDIR}/log;
  mkdir -p ${logdir}
}

function prepare_runtime_environment(){
    # Does any component run in parallel?
    prepare_openmpi=0
    local_openmpi=0
    LAUNCHER_METGRID="";LAUNCHER_UNGRIB="";LAUNCHER_REAL="";LAUNCHER_WRF=""
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
          mv ${LOCALDIR}/openmpi  ${ROOTDIR}/
       fi
    fi
    export OPAL_PREFIX=${ROOTDIR}/openmpi
    export PATH=$OPAL_PREFIX/bin:$PATH
    export LD_LIBRARY_PATH=$OPAL_PREFIX/lib:$LD_LIBRARY_PATH
}

function prepare_local_environment (){
  cp ${LOCALDIR}/WRF4Gbin-${WRF_VERSION}.tar.gz ${ROOTDIR}
  $MPI_LAUNCHER -pernode --wdir ${ROOTDIR} ${ROOTDIR}/WRFGEL/load_wrfbin.sh	
  rm ${ROOTDIR}/WRF4Gbin-${WRF_VERSION}.tar.gz
  rm {LOCALDIR}/WRF4Gbin-${WRF_VERSION}.tar.gz
  prepare_openmpi=0
  #mpiexec -mca btl self,sm,tcp --wdir /localtmp --preload-files-dest-dir /localtmp --preload-files /localtmp/WRF4Gbin-3.1.1_r484INTEL_OMPI.tar.gz --preload-binary /localtmp/wrf.sh /localtmp/wrf.sh
}


function timelog_clean(){
  rm -f ${logdir}/time.log
}

function timelog_end(){
  echo "$(date +%Y%m%d%H%M%S)" > ${timelog_item}.end && vcp ${timelog_item}.end ${WRF4G_BASEPATH}/experiments/${experiment_name}/${realization_name}/
  date +%Y%m%d%H%M%S >> ${logdir}/time.log
}

function timelog_init(){
  timelog_item=${1// /_}
  create_output_structure
  echo -e "$(date +%Y%m%d%H%M%S)\n$(hostname --fqdn):$(pwd)" > ${timelog_item}.init && vcp ${timelog_item}.init ${WRF4G_BASEPATH}/experiments/${experiment_name}/${realization_name}/
  echo -n "$(printf "%20s" "$timelog_item") $(date +%Y%m%d%H%M%S) " >> ${logdir}/time.log
}

function wrf4g_exit(){
  excode=$1
  #
  #  This is the way out of this script. So close the timing info, move the
  #  logs to a safe place and leave
  #
  test "${excode}" -eq 0 || timelog_end
  echo "exit $excode" >> ${logdir}/time.log
  ls -l >& ${logdir}/ls.wrf
  if test -e rsl.out.0000; then
    mkdir -p rsl_wrf
    mv rsl.* rsl_wrf/
    mv rsl_wrf ${logdir}/
  fi
  case $excode in
    ${ERROR_UNGRIB_FAILED})
      ls -lR >& ${logdir}/ls.wps
      ;;
  esac
  create_output_structure
  test -f namelist.input && cp namelist.input ${logdir}/
  tar czf log.tar.gz ${logdir} && vcp log.tar.gz ${WRF4G_BASEPATH}/experiments/${experiment_name}/${realization_name}/
  test "${LOCALDIR}" != "${ROOTDIR}" && mv ${logdir} ${ROOTDIR}/
  exit ${excode}
}

WRF4G_prepare

load_default_config

prepare_runtime_environment

timelog_clean
#
#  Get the (re)start (i**) and end (f**) dates for this chunk
#
read iyy imm idd ihh trash <<< $(echo ${chunk_start_date} | tr '_:T-' '    ')
read ryy rmm rdd rhh trash <<< $(echo ${chunk_restart_date} | tr '_:T-' '    ')
read fyy fmm fdd fhh trash <<< $(echo ${chunk_end_date}   | tr '_:T-' '    ')
#
#   Must WPS run or are the boundaries available?
#
set -v
if test "$(exist_wps $(date_wrf2iso ${chunk_start_date}))" -eq "1"; then
  wps_ran=0
  cd ${LOCALDIR}/WPS || exit
    vcp ${VCPDEBUG} ${WRF4G_DOMAINPATH}/${domain_name}/namelist.wps . || exit ${ERROR_VCP_FAILED}
  cd ${LOCALDIR}
  cd ${LOCALDIR}/WRFV3/run || exit
    namelist_wps2wrf ${chunk_restart_date} ${chunk_end_date} ${max_dom} ${chunk_is_restart} ${timestep_dxfactor}
    timelog_init "wps get"
      download_file wps $(date_wrf2iso ${chunk_start_date})
    timelog_end
  cd ${LOCALDIR}
else
  wps_ran=1
  cd ${LOCALDIR}/WPS || exit
    clean_wps
    #
    #   Get geo_em files and namelist.wps
    #
    vcp ${VCPDEBUG} ${WRF4G_DOMAINPATH}/${domain_name}/'*' . || exit ${ERROR_VCP_FAILED}
    #
    #   Modify the namelist
    #
    set +v
    fortnml_setn namelist.wps start_date ${max_dom} "'${chunk_start_date}'"
    fortnml_setn namelist.wps end_date   ${max_dom} "'${chunk_end_date}'"
    fortnml_set  namelist.wps interval_seconds      ${global_interval}
    fortnml_set  namelist.wps max_dom               ${max_dom}
    fortnml_set  namelist.wps prefix                "'${global_name}'"
    set -v
    #
    #   Preprocessor
    #
    timelog_init "get boundaries"
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
    #   Check for other input namelists and apply them
    #
    for vtname in ${global_vtable_other}
    do
      fortnml --overwrite -f namelist.wps -s prefix@ungrib          ${vtname}
      fortnml --overwrite -f namelist.wps -s end_date               ${chunk_start_date}  # single time step!!
      fortnml --overwrite -f namelist.wps -a constants_name@metgrid ${vtname}:${iyy}-${imm}-${idd}_${ihh}
      ln -sf ungrib/Variable_Tables/Vtable.${vtname} Vtable
      rm -rf grbData/*.grb
      if ! which cdo; then
        thisdir=$(pwd)
        cd `cat ../rootdir`
          if test -e /software/ScientificLinux/4.6/etc/bashrc; then
            cp /oceano/gmeteo/WORK/MDM.UC/Apps/cdo.tar.gz .
          else
            vcp gsiftp://ce01.macc.unican.es:2812/oceano/gmeteo/WORK/MDM.UC/Apps/cdo.tar.gz .
          fi
          tar xzf cdo.tar.gz && rm cdo.tar.gz
        cd ${thisdir}
      fi
      cdo setdate,${iyy}-${imm}-${idd} FAKESOIL.grb grbData/FAKESOIL.grb
      ./link_grib.csh grbData/*.grb
      ${ROOTDIR}/bin/ungrib.exe >& ${logdir}/ungrib_${vtname}_${iyy}${imm}${idd}${ihh}.out || wrf4g_exit ${ERROR_UNGRIB_FAILED}
      cat ${logdir}/ungrib_${vtname}_${iyy}${imm}${idd}${ihh}.out \
        | grep -q -i 'Successful completion of ungrib' \
        || wrf4g_exit ${ERROR_UNGRIB_FAILED}
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
      set +v
      fortnml_vardel namelist.wps opt_output_from_metgrid_path
      fortnml_vardel namelist.wps opt_output_from_geogrid_path
      fortnml_vardel namelist.wps opt_metgrid_tbl_path
      fortnml_vardel namelist.wps opt_geogrid_tbl_path
      fortnml_setn namelist.wps start_date ${max_dom} "'${chunk_start_date}'"
      fortnml_setn namelist.wps end_date   ${max_dom} "'${chunk_end_date}'"
      fortnml -o -f namelist.wps -s fg_name ${global_name}
      set -v
      ${LAUNCHER_METGRID} ${ROOTDIR}/bin/metgrid.exe \
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
    timelog_init "real"
      clean_real
      ln -s ../../WPS/met_em.d??.????-??-??_??:00:00.nc .
      ls -l ########################################################## borrar
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
      timelog_init "wps put"
        create_output_structure
        ${ROOTDIR}/WRFGEL/post_and_register --no-bg wps "${chunk_start_date}"
      timelog_end
    fi
  cd ${LOCALDIR} || exit
fi

#------------------------------------------------------------------
#                              WRF
#------------------------------------------------------------------
cd ${LOCALDIR}/WRFV3/run || exit
  if test -n "${icbcprocessor}"; then
    timelog_init "icbcprocessor"
      icbcprocessor.${icbcprocessor} >&  ${logdir}/icbcproc_${ryy}${rmm}${rdd}${rhh}.out
    timelog_end
  fi
  timelog_init "wrf"
    
    # If wrf is run in parallel and the environment is not prepared, prepare it.  	 
    if [ ${local_openmpi} -eq 1 -a ${wrf_parallel} -eq 1  ]; then
        prepare_local_environment
    fi
	
    echo "${LAUNCHER_WRF} ${ROOTDIR}/bin/wrf_wrapper.exe >& ${logdir}/wrf_${ryy}${rmm}${rdd}${rhh}.out"
    fortnml -o -f namelist.input -s debug_level 0
    ${LAUNCHER_WRF} ${ROOTDIR}/bin/wrf_wrapper.exe >& ${logdir}/wrf_${ryy}${rmm}${rdd}${rhh}.out &
    # Wait enough time to allow 'wrf_wrapper.exe' create 'wrf.pid'
    # This time is also useful to  to copy the wpsout data
    sleep 10 
    ps -ef | grep wrf.exe
  #  ${ROOTDIR}/WRFGEL/wrf4g_monitor $(cat wrf.pid) >& ${logdir}/monitor.log &
    sleep 400
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
