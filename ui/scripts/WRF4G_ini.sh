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
export PATH="`pwd`/bin:${PATH}"
#
#  Load wrf.input and wrf.chunk
#
source wrf4g.conf                                  || exit ${ERROR_MISSING_WRF4GCNF}
sed -e 's/\ *=\ */=/' wrf.input > source.it        || exit ${ERROR_MISSING_WRFINPUT}
source source.it && rm source.it
sed -e 's/\ *=\ */=/' wrf.chunk > source.it        || exit ${ERROR_MISSING_WRFCHUNK}
source source.it && rm source.it
#
#  Create WRF4G framework structure
#
vcp ${WRF4G_APPS}/WRF4G-${WRF4G_VERSION}.tar.gz .
tar xzf WRF4G-${WRF4G_VERSION}.tar.gz && rm -f WRF4G-${WRF4G_VERSION}.tar.gz
vcp ${WRF4G_APPS}/WRF4Gbin-${WRF_VERSION}.tar.gz .
tar xzf WRF4Gbin-${WRF_VERSION}.tar.gz && rm -f WRF4Gbin-${WRF_VERSION}.tar.gz
#
#   Now run the WRF4G...
#
tar xzf sandbox.tar.gz # La namelist buena esta aqui!
sleep 5000
source WRF4G.sh
