#!/bin/bash
version="3.1"
revision=`svn info | grep Revision: | awk '{print $2}'`
ll1=`expr length $1`
ll1=`expr $ll1 + 0`
if test $ll1 -ne 0
then
basedir=$1
else
basedir=.
fi

echo "basedir: "$basedir
#exit

thisdir=$(pwd)

tardir="tarball${RANDOM}"

mkdir ${tardir}
cd ${tardir}
  mkdir -p WPS/metgrid
  mkdir -p WPS/ungrib
  mkdir -p WRFV3/run
  
  ln -s ${basedir}/WPS/metgrid/metgrid.exe WPS/metgrid/metgrid.exe
  ln -s ${basedir}/WPS/metgrid/METGRID.TBL.ARW WPS/metgrid/METGRID.TBL
  ln -s ${basedir}/WPS/ungrib/ungrib.exe WPS/ungrib/ungrib.exe
  ln -s ${basedir}/WPS/ungrib/Variable_Tables_WRF4G WPS/
  ln -s ${basedir}/WPS/link_grib.csh WPS/
  
  ln -s ${basedir}/WRFV3/run/*.TBL WRFV3/run
  ln -s ${basedir}/WRFV3/run/*_DATA* WRFV3/run
  ln -s ${basedir}/WRFV3/run/*formatted WRFV3/run/
  ln -s ${basedir}/WRFV3/run/tr* WRFV3/run
  ln -s ${basedir}/WRFV3/main/real.exe WRFV3/run
  ln -s ${basedir}/WRFV3/main/wrf.exe WRFV3/run 
  

  tar czhv --exclude=".svn" \
  -f ${thisdir}/WRF4Gbin-${version}_r${revision}.tar.gz *
cd ..
rm -rf ${tardir}
