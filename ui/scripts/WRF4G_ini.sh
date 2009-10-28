#! /bin/bash
#
# WRF4G_ini.sh
#
# WRF4G initializer
#
ROOTDIR=$(pwd)
#
#   Expand the sandbox files
#
tar xzf sandbox.tar.gz
#
#  Load wrf.input and wrf.chunk
#
source wrf4g.conf                            || exit ${ERROR_MISSING_WRF4GCNF}
sed -e 's/\ *=\ */=/' wrf.input > source.it  || exit ${ERROR_MISSING_WRFINPUT}
source source.it && rm source.it
sed -e 's/\ *=\ */=/' wrf.chunk > source.it  || exit ${ERROR_MISSING_WRFCHUNK}
source source.it && rm source.it
#
#   Expand the WRF4G scripts
#
export PATH="${ROOTDIR}/bin:$PATH"
chmod +x ${ROOTDIR}/bin/*
vcp ${WRF4G_APPS}/WRF4G-${WRF4G_VERSION}.tar.gz . || exit ${ERROR_MISSING_WRF4GSRC}
tar xzf WRF4G-${WRF4G_VERSION}.tar.gz && rm -f WRF4G-${WRF4G_VERSION}.tar.gz
#
#  Load functions and set the PATH
#
source ${ROOTDIR}/lib/bash/wrf_util.sh
source ${ROOTDIR}/lib/bash/wrf4g_exit_codes.sh
export PATH="${ROOTDIR}/WRFGEL:$PATH"
chmod +x ${ROOTDIR}/WRFGEL/*
#
#   Should we unpack here or there is a local filesystem for us to run?
#
if test -n "${WRF4G_RUN_LOCAL}"; then
  LOCALDIR="${WRF4G_RUN_LOCAL}/wrf4g.${RANDOM}"
  mkdir ${LOCALDIR} || exit ${ERROR_CANNOT_ACCESS_LOCALDIR}
fi
#
#  Should this chunk REALLY run?
#
set -v
export WRF4G_CONF_FILE="${ROOTDIR}/wrf4g.conf"
export WRF4G_EXPERIMENT="${experiment_name}"
export WRF4G_REALIZATION="${realization_name}"
if test ${is_restart} -eq 1; then
  echo "This is a forced-restart run"
  # This will make the trick...
  restart_date=$(date_wrf2iso ${chunk_start_date})
else
  restart_date=$(get_date_restart -v || exit ${ERROR_GETDATERST_FAILED})
  echo "Last restart date for this realization is: ${restart_date}"
fi
if test "${restart_date}" = "-1"; then
  if test "${chunk_is_restart}" = ".T."; then
    echo "Something went wrong! (the restart file is not available and the chunk is a restart...)"
    test -n "${LOCALDIR}" && rmdir ${LOCALDIR}
    exit ${EXIT_RESTART_MISMATCH}
  fi
  echo "chunk_restart_date=\"${chunk_start_date}\"" >> wrf.chunk
  test -n "${LOCALDIR}" && cp wrf.chunk ${LOCALDIR}/
elif test "$(date2int ${restart_date})" -ge "$(date2int ${chunk_end_date})"; then
  echo "This chunk already run! Ciao..."
  test -n "${LOCALDIR}" && rmdir ${LOCALDIR}
  exit ${EXIT_CHUNK_ALREADY_FINISHED}
elif test "$(date2int ${restart_date})" -lt "$(date2int ${chunk_start_date})"; then
  echo "The date of the simulation did not reach this chunk yet! Ciao..."
  test -n "${LOCALDIR}" && rmdir ${LOCALDIR}
  exit ${EXIT_CHUNK_SHOULD_NOT_RUN}
else
  #
  #  Get the restart files, set the restart flag to true and set the new start date
  #
  test -n "${LOCALDIR}" && cd ${LOCALDIR}
    download_file rst ${restart_date} || exit ${ERROR_RST_DOWNLOAD_FAILED}
  test -n "${LOCALDIR}" && cd ${ROOTDIR}
  if test "$(date2int ${restart_date})" -ne "$(date2int ${chunk_start_date})"; then
    echo 'chunk_is_restart=".T."' >> wrf.chunk
  fi
  echo "chunk_restart_date=\"$(date_iso2wrf ${restart_date})\"" >> wrf.chunk
  test -n "${LOCALDIR}" && cp wrf.chunk ${LOCALDIR}/
fi
set +v
#
#  Create WRF4G framework structure
#
test -n "${LOCALDIR}" && cd ${LOCALDIR}
mkdir -p log
vcp ${WRF4G_APPS}/WRF4Gbin-${WRF_VERSION}.tar.gz .
tar xzf WRF4Gbin-${WRF_VERSION}.tar.gz && rm -f WRF4Gbin-${WRF_VERSION}.tar.gz || exit ${ERROR_MISSING_WRF4GBIN}
tar xzf ${ROOTDIR}/sandbox.tar.gz WRFV3/run/namelist.input # La namelist buena esta aqui!
mv wrfrst* WRFV3/run >& /dev/null || :
rm -f ${ROOTDIR}/sandbox.tar.gz 
test -n "${LOCALDIR}" && cp ${ROOTDIR}/wrf4g.conf ${LOCALDIR}/
#
#  If there are additional files, expand'em
#
if test -f ${ROOTDIR}/wrf4g_files.tar.gz; then
  tar xzvf ${ROOTDIR}/wrf4g_files.tar.gz && rm ${ROOTDIR}/wrf4g_files.tar.gz
fi
#
#   Now run the WRF4G...
#
source ${ROOTDIR}/WRF4G.sh >& log/WRF4G.log
