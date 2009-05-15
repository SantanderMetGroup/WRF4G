#!/bin/bash
version="3.1"
thisdir=$(pwd)

if test ${#} -gt 1; then
  basedir=$1
else
  basedir="$(dirname $(dirname $(dirname $0)))/wn"
fi

echo "      <<<< BASEDIR: $basedir"

revision=`svn info ${basedir}/WRFV3 | grep 'Last Changed Rev:' | awk -F: '{print $2}' | tr -d ' '`

tardir="tarball${RANDOM}"

mkdir ${tardir}
cd ${tardir}
  mkdir -p WPS/metgrid
  mkdir -p WPS/ungrib
  mkdir -p WRFV3/run

  ln -s ${basedir}/bin .  

  ln -s ${basedir}/WPS/metgrid/metgrid.exe WPS/metgrid/metgrid.exe
  ln -s ${basedir}/WPS/metgrid/METGRID.TBL.ARW WPS/metgrid/METGRID.TBL
  ln -s ${basedir}/WPS/ungrib/ungrib.exe WPS/ungrib/ungrib.exe
  ln -s ${basedir}/WPS/ungrib/Variable_Tables_WRF4G WPS/ungrib/
  ln -s ${basedir}/WPS/link_grib.csh WPS/
  
  ln -s ${basedir}/WRFV3/configure.wrf WRFV3
  ln -s ${basedir}/WRFV3/run/*.TBL WRFV3/run
  ln -s ${basedir}/WRFV3/run/*_DATA* WRFV3/run
  ln -s ${basedir}/WRFV3/run/*formatted WRFV3/run/
  ln -s ${basedir}/WRFV3/run/tr* WRFV3/run
  ln -s ${basedir}/WRFV3/run/real.exe WRFV3/run
  ln -s ${basedir}/WRFV3/run/wrf.exe WRFV3/run 
  

  tar czhv --exclude=".svn" \
  -f ${thisdir}/WRF4Gbin-${version}_r${revision}.tar.gz *
cd ..
rm -rf ${tardir}
