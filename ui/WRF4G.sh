#! /bin/bash
#
# WRF4G.sh
#
WRF_VERSION="3.1"
WRF4G_VERSION="0.0.0"
ROOTDIR=$(pwd)
#
#  Load wrf.input and wrf.chunk
#
sed -e 's/\ *=\ */=/' wrf.input > source.it        || exit ${ERROR_MISSING_WRFINPUT}
source source.it && rm source.it
sed -e 's/\ *=\ */=/' wrf.chunk > source.it        || exit ${ERROR_MISSING_WRFCHUNK}
source source.it && rm source.it
#
#  Create WRF4G framework structure
#
#vcp ${base_path}/Apps/WRF4G-${WRF4G_VERSION}.tar.gz . # !!! NO fona. No hay vcp!
cp ${base_path}/Apps/WRF4G-${WRF4G_VERSION}.tar.gz . # !!! NO fona. No hay vcp!
tar xzf WRF4G-${WRF4G_VERSION}.tar.gz
#
#  Load functions
#
source ${ROOTDIR}/usr/lib/bash/wrf_util.sh
#
#  Get the standard WRF binaries
#
vcp ${base_path}/Apps/WRFbin-${WRF_VERSION}.tar.gz
tar xzf WRFbin-${WRF_VERSION}.tar.gz
#
# Running WRF
#

function update_ptop(){
  echo "Adding 'Zglobal' and 'Ptop' variables..."
  ##Num vertical levels '@Zglobal@'
  zglobal=$($ncdump -h /gpfs/ifca.es/meteo/SCRATCH/scaling/cores8/simulations/met_em.d01.2001-01-01_06:00:00.nc | grep 'num_metgrid_levels' | grep = | awk '{print $3}')
  ##Ptop '@Ptop@'
  ptop=$($ncdump -v PRES /gpfs/ifca.es/meteo/SCRATCH/scaling/cores8/simulations/met_em.d01.2001-01-01_06:00:00.nc | tail -2 | head -1 | tr -d ',;' | awk '{printf "%d", $1}')
  ${foresthome}/bats/change_in_file.bash /gpfs/ifca.es/meteo/SCRATCH/scaling/cores8/simulations/2001010106_2001010300/namelist.input '@Zglobal@' $zglobal
  ${foresthome}/bats/change_in_file.bash /gpfs/ifca.es/meteo/SCRATCH/scaling/cores8/simulations/2001010106_2001010300/namelist.input '@Ptop@' $ptop
}

function get_physics_tables(){
  cp /gpfs/ifca.es/meteo/DATA/WRF/WRF_bin/3.0.1.1/CE01/mpich_1.2.7_pgi_gcc/WRFV3/run/*.TBL ./
  cp /gpfs/ifca.es/meteo/DATA/WRF/WRF_bin/3.0.1.1/CE01/mpich_1.2.7_pgi_gcc/WRFV3/run/*_DATA* ./
  cp /gpfs/ifca.es/meteo/DATA/WRF/WRF_bin/3.0.1.1/CE01/mpich_1.2.7_pgi_gcc/WRFV3/run/*formatted ./
}

function timelog_end(){
  date +%Y%m%d%H%M%S >> ${ROOTDIR}/time.log
}

function timelog_init(){
  item=$1
  echo -n "$item $(date +%Y%m%d%H%M%S) " >> ${ROOTDIR}/time.log
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

read iyy imm idd ihh <<< $(echo ${chunk_start_date} | tr -d '_:T-')
read fyy fmm fdd fhh <<< $(echo ${chunk_end_date}   | tr -d '_:T-')
if test "${fyy}${fmm}${fdd}${fhh}" -gt "${eyy}${emm}${edd}${ehh}"; then
  fyy=${eyy}; fmm=${emm}; fdd=${edd}; fhh=${ehh}
fi

cd WPS || exit
  #
  #   Must WPS run or are the boundaries available?
  #
  ####### TODO
  #
  #   Get geo_em files and namelist.wps
  #
  vcp -r ${domain_path}/${domain_name}/'*' . 
  #
  #   Modify the namelist
  #
  fortnml_setn namelist.wps start_date ${max_dom} "'${chunk_start_date}'"
  fortnml_setn namelist.wps end_date   ${max_dom} "'${chunk_end_date}'"
  fortnml_set  namelist.wps interval_seconds      ${global_interval}
  fortnml_set  namelist.wps max_dom               ${max_dom}
  fortnml_set  namelist.wps prefix                ${global_name}
  #
  #   Preprocessor
  #
  timelog_init "get boundaries"
    echo "Linking global data from: ${global_path}"
    mkdir -p grbData
    for yearmon in $(get_yearmons $iyy $imm $fyy $fmm) 
    do
      vcp ${global_path}/${year}/*${yearmon}*.grb ln:///grbData/ 
    done
    ./link_grib.csh grbData/*.grb
  timelog_end
  timelog_init "ungrib"
    ln -sf Vtable.${global_name} Vtable
    ./ungrib/ungrib.exe >& ${logdir}/ungrib_${global_name}_${syy}${smm}${sdd}${shh}.out || exit ${ERROR_UNGRIB_FAILED}
  timelog_end
  #
  #   Check for other input namelists and apply them
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
    ./ungrib/ungrib.exe >& ${logdir}/ungrib_${global_name}FIX_${syy}${smm}${sdd}${shh}.out || exit 203
    mv ${global_name}FIX* ${global_name}FIX || exit 71
  fi
  #
  #   Run metgrid
  #
  timelog_init "metgrid"
    fortnml_setn namelist.wps start_date ${max_dom} "'${chunk_start_date}'"
    fortnml_setn namelist.wps end_date   ${max_dom} "'${chunk_end_date}'"
    ./metgrid/metgrid.exe >& ${logdir}/metgrid_${syy}${smm}${sdd}${shh}.out || exit ${ERROR_METGRID_FAILED}
  timelog_end
cd ${ROOTDIR}/WRFV3/run || exit
  #------------------------------------------------------------------
  #                              WRF
  #------------------------------------------------------------------
  timelog_init "real"
    fix_ptop
    cp ../../namelist.wps .
    fortnml_varcopy namelist.wps   namelist.input parent_grid_ratio
    fortnml_varcopy namelist.wps   namelist.input parent_grid_ratio parent_time_step_ratio
    fortnml_varcopy namelist.wps   namelist.input i_parent_start
    fortnml_varcopy namelist.wps   namelist.input j_parent_start
    fortnml_varcopy namelist.wps   namelist.input e_we
    fortnml_varcopy namelist.wps   namelist.input e_sn
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

    $launcher ./real.exe || exit ${ERROR_REAL_FAILED}
    if [ "$is_restart" = ".true." ]; then
      fortnml_set namelist.input restart .true.
    fi
  timelog_end
  timelog_init "wrf"
    $launcher ./wrf.exe >& ${logdir}/wrf_${syy}${smm}${sdd}${shh}.out || exit ${ERROR_WRF_FAILED}
  timelog_end
cd ${ROOTDIR}







  #
  # preprocess
  #
  #
  # ungrib.exe
  #
  timelog_init ungrib
    /gpfs/ifca.es/meteo/DATA/WRF/WRF_bin/3.0.1/CE01/WPS/ungrib/src/ungrib.exe
  timelog_end
  rm -rf /gpfs/ifca.es/meteo/SCRATCH/scaling/cores8/simulations/2001010106_2001010300/AN  /gpfs/ifca.es/meteo/SCRATCH/scaling/cores8/simulations/2001010106_2001010300/GRIBFILE.*
  #
  # metgrid.exe
  #
  timelog_init metgrid
    ./metgrid.exe
    cp met_em.* /gpfs/ifca.es/meteo/SCRATCH/scaling/cores8/simulations
  timelog_end
  cd /gpfs/ifca.es/meteo/SCRATCH/scaling/cores8/simulations/2001010106_2001010300
  rm GRIBFILE.* met_em* *:* >& /dev/null
  rm *.exe 
  rm Vtable.ECMWF* 
  rm link_grib.csh
fi

if test $bit3 -eq 1
then
  if test -e /gpfs/ifca.es/meteo/SCRATCH/scaling/cores8/simulations/2001010106_2001010300/launcher_phases${WPS_WRFlabel}.inf
  then
    echo "real.exe        @ireal@  @ereal@ " >> /gpfs/ifca.es/meteo/SCRATCH/scaling/cores8/simulations/2001010106_2001010300/launcher_phases${WPS_WRFlabel}.inf
  else
    echo "real.exe        @ireal@  @ereal@ " > /gpfs/ifca.es/meteo/SCRATCH/scaling/cores8/simulations/2001010106_2001010300/launcher_phases${WPS_WRFlabel}.inf
  fi
  #
  # Running WRF modules
  #
  cd /gpfs/ifca.es/meteo/SCRATCH/scaling/cores8/simulations/2001010106_2001010300
  update_ptop
  restart=`(cat /gpfs/ifca.es/meteo/SCRATCH/scaling/cores8/simulations/2001010106_2001010300/wrf.input | grep RESTART= | awk '{print $2}')`
  if test $restart = '.T.'; then
    ln -sf /gpfs/ifca.es/meteo/SCRATCH/scaling/cores8/simulations/wrfrst* ./
  fi
  get_physics_tables 
  for yearsmonths in 200101; do
    year=`(expr substr $yearsmonths 1 4)`
    month=`(expr substr $yearsmonths 5 2)`
    ls -1 /gpfs/ifca.es/meteo/SCRATCH/scaling/cores8/simulations/met_em*.${year}-${month}-* | while read file;do
      ln -sf $file ./
    done
  done
  
  cp /gpfs/ifca.es/meteo/DATA/WRF/WRF_bin/3.0.1.1/CE01/mpich_1.2.7_pgi_gcc/WRFV3/main/real.exe ./
  #
  # real.exe
  #
  initdate=`(date +%Y%m%d%H%M%S)`
  ${foresthome}/bats/change_in_file.bash /gpfs/ifca.es/meteo/SCRATCH/scaling/cores8/simulations/2001010106_2001010300/launcher_phases${WPS_WRFlabel}.inf '@ireal@' $initdate
  mpiexec -comm mpich-ib -npernode 8 ./real.exe
  rm real.exe
  rm met_em.*
  enddate=`(date +%Y%m%d%H%M%S)`
  ${foresthome}/bats/change_in_file.bash /gpfs/ifca.es/meteo/SCRATCH/scaling/cores8/simulations/2001010106_2001010300/launcher_phases${WPS_WRFlabel}.inf '@ereal@' $enddate
  #
  # Making copies of initial boundary conditions
  # 
  cp wrfinput* /gpfs/ifca.es/meteo/SCRATCH/scaling/cores8/simulations
  cp wrfbdy* /gpfs/ifca.es/meteo/SCRATCH/scaling/cores8/simulations
  if test -e tslist; then
    cp wrflowinp* /gpfs/ifca.es/meteo/SCRATCH/scaling/cores8/simulations
  fi
fi

if test $bit4 -eq 1
then
  #
  # Running WRF modules
  #
  cd /gpfs/ifca.es/meteo/SCRATCH/scaling/cores8/simulations/2001010106_2001010300
  if test "${is_restart}" -eq 1
  then
    ln -sf /gpfs/ifca.es/meteo/SCRATCH/scaling/cores8/simulations/wrfrst* ./
  fi
  clean_rsl
  get_physics_tables
  for yearsmonths in 200101 
  do
    year=`expr substr $yearsmonths 1 4`
    month=`expr substr $yearsmonths 5 2`
    ls -1 /gpfs/ifca.es/meteo/SCRATCH/scaling/cores8/simulations/met_em*.${year}-${month}-* | while read file;do
      ln -sf $file ./
    done
  done
  cp /gpfs/ifca.es/meteo/DATA/WRF/WRF_bin/3.0.1.1/CE01/mpich_1.2.7_pgi_gcc/WRFV3/main/wrf.exe ./
  cp /gpfs/ifca.es/meteo/SCRATCH/scaling/cores8/simulations/wrfbdy* ./
  cp /gpfs/ifca.es/meteo/SCRATCH/scaling/cores8/simulations/wrfinput* ./
  if test -e tslist
  then
    cp /gpfs/ifca.es/meteo/SCRATCH/scaling/cores8/simulations/wrflowinp* ./
  fi
  if test $bit3 -eq 0; then
    update_ptop
  fi
  #
  # wrf.exe
  #
  LAUNCHER=$(eval echo ${LAUNCHER_TPL})
  timelog_init wrf
    $LAUNCHER ./wrf.exe
    restartinterv=0
    if test $restartinterv -ne 0
    then
      cp wrfrst* /gpfs/ifca.es/meteo/SCRATCH/scaling/cores8/simulations
    fi
    rm wrf.exe
    rm *.TBL 
    rm *_DATA*
    rm met_em*
    rm wrfbdy* 
    rm wrfinput* 
    if test -e tslist; then
      rm wrflowinp* 
    fi
  timelog_end
  #
  # OUTputting
  #
  cd $runhome
  #rm wrfrst* >& /dev/null
  chmod -w $runhome/wrfout_d*
  chmod -w $runhome/namelist.output
  if test ! $outputhome = $runhome
  then
    ( # Asyncronous copy...
      ${foresthome}/bats/vcp ${run_path}/wrfout'*'       ${output_path}
      ${foresthome}/bats/vcp ${run_path}/namelist.output ${output_path}
      if test -e tslist; then
        ${foresthome}/bats/vcp ${run_path}/'*'.TS        ${output_path}
      fi
      echo "Removing WRF outputs..."
      rm -f wrfout_d*
    ) &
  fi
  #
  # Writting correct ending date in links file
  #
  ls -1 /gpfs/ifca.es/meteo/SCRATCH/scaling/cores8/simulations/2001010106_2001010300/wrfout_d01* > /gpfs/ifca.es/meteo/SCRATCH/scaling/cores8/simulations/2001010106_2001010300/outWRF_d01.inf
  numouts=`(wc -l /gpfs/ifca.es/meteo/SCRATCH/scaling/cores8/simulations/2001010106_2001010300/outWRF_d01.inf | awk '{print $1}')`
  if test $numouts -ne 0; then
    lastoutfile=`(tail -n 1 /gpfs/ifca.es/meteo/SCRATCH/scaling/cores8/simulations/2001010106_2001010300/outWRF_d01.inf)`
    $ncdump -v Times -l 10000 $lastoutfile > /gpfs/ifca.es/meteo/SCRATCH/scaling/cores8/simulations/2001010106_2001010300/Times.inf
    simdate=`(tail -n 2  /gpfs/ifca.es/meteo/SCRATCH/scaling/cores8/simulations/2001010106_2001010300/Times.inf | head -n 1 | awk '{print substr($1,2,19)}')`
    iyyyyfsim=`(expr substr $datefSIM 1 4)`
    immfsim=`(expr substr $datefSIM 5 2)`
    iddfsim=`(expr substr $datefSIM 7 2)`
    ihhfsim=`(expr substr $datefSIM 9 2)`
    imifsim=`(expr substr $datefSIM 11 2)`
    if test $simdate = ${iyyyyfsim}-${immfsim}-${iddfsim}_${ihhfsim}:${imifsim}:00; then
      ${foresthome}/bats/change_in_file.bash /gpfs/ifca.es/meteo/SCRATCH/scaling/cores8/lwrf_iteration/Iteration_state.inf '@endWRF.exe@' $datefSIMshort
    fi
    rm /gpfs/ifca.es/meteo/SCRATCH/scaling/cores8/simulations/2001010106_2001010300/outWRF_d01.inf
  fi
  rm /gpfs/ifca.es/meteo/SCRATCH/scaling/cores8/simulations/met_em*
fi
