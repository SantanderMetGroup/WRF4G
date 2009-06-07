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
vcp ${WRF4G_APPS}/WRF4G-${WRF4G_VERSION}.tar.gz .
tar xzf WRF4G-${WRF4G_VERSION}.tar.gz && rm -f WRF4G-${WRF4G_VERSION}.tar.gz
#
#  Load functions and set the PATH
#
source ${ROOTDIR}/lib/bash/wrf_util.sh
source ${ROOTDIR}/lib/bash/wrf4g_exit_codes.sh
export PATH="${ROOTDIR}/WRFGEL:$PATH"
chmod +x ${ROOTDIR}/bin/*
chmod +x ${ROOTDIR}/WRFGEL/*
#
#  Should this chunk REALLY run?
#
set -v
export WRF4G_CONF_FILE="${ROOTDIR}/wrf4g.conf"
export WRF4G_EXPERIMENT="${experiment_name}"
export WRF4G_REALIZATION="${realization_name}"
restart_date=$(get_date_restart)
echo "Last restart date for this realization is: ${restart_date}"
if test "${restart_date}" = "-1"; then
  if test "${chunk_is_restart}" = ".T."; then
    echo "Something went wrong! (the restart file is not available and the chunk is a restart...)"
    exit ${EXIT_RESTART_MISMATCH}
  fi
  echo "chunk_restart_date=\"${chunk_start_date}\"" >> wrf.chunk
elif test "$(date2int ${restart_date})" -ge "$(date2int ${chunk_end_date})"; then
  echo "This chunk already run! Ciao..."
  exit ${EXIT_CHUNK_ALREADY_FINISHED}
elif test "$(date2int ${restart_date})" -lt "$(date2int ${chunk_start_date})"; then
  echo "The date of the simulation did not reach this chunk yet! Ciao..."
  exit ${EXIT_CHUNK_SHOULD_NOT_RUN}
else
  #
  #  Get the restart files, set the restart flag to true and set the new start date
  #
  download_file rst ${restart_date} || exit ${ERROR_RST_DOWNLOAD_FAILED}
  echo 'chunk_is_restart=".T."' >> wrf.chunk
  echo "chunk_restart_date=\"$(date_iso2wrf ${restart_date})\"" >> wrf.chunk
fi
set +v
#
#  Create WRF4G framework structure
#
mkdir -p log
vcp ${WRF4G_APPS}/WRF4Gbin-${WRF_VERSION}.tar.gz .
tar xzf WRF4Gbin-${WRF_VERSION}.tar.gz && rm -f WRF4Gbin-${WRF_VERSION}.tar.gz
tar xzf sandbox.tar.gz WRFV3/run/namelist.input # La namelist buena esta aqui!
mv wrfrst* WRFV3/run >& /dev/null || :
#
#   Now run the WRF4G...
#
source WRF4G.sh >& log/WRF4G.log
