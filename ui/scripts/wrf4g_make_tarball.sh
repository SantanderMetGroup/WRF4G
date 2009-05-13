
version="0.0.0"

thisdir=$(pwd)
basedir=$(dirname $(dirname $(dirname $0)))

tardir="tarball${RANDOM}"

mkdir ${tardir}
cd ${tardir}
  ln -s ${basedir}/wn/* .
  tar czhv --exclude=".svn" \
    --exclude="WPS" --exclude="WRFV3" --exclude="bin" \
    -f ${thisdir}/WRF4G-${version}.tar.gz *
cd ..
rm -rf ${tardir}
