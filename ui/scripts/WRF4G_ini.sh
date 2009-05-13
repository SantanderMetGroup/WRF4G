#! /bin/bash
#
# WRF4G_ini.sh
#
# WRF4G initializer
#
WRF_VERSION="3.1"
WRF4G_VERSION="0.0.0"
ROOTDIR=$(pwd)
#
#   Expand the sandbox files
#
tar xzf sandbox.tar.gz
export PATH="`pwd`/bin:${PATH}"
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
vcp ${base_path}/Apps/WRF4G-${WRF4G_VERSION}.tar.gz .
tar xzf WRF4G-${WRF4G_VERSION}.tar.gz && rm -f WRF4G-${WRF4G_VERSION}.tar.gz
vcp ${base_path}/Apps/WRFbin-${WRF_VERSION}.tar.gz .
tar xzf WRFbin-${WRF_VERSION}.tar.gz && rm -f WRFbin-${WRF_VERSION}.tar.gz
#
#   Now run the WRF4G...
#
source WRF4G.sh
