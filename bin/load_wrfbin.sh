#!/bin/bash
ROOTDIR=$PWD
#echo $ROOTDIR
LOCALDIR=$(cat localdir)
mkdir -p ${LOCALDIR}/WRFV3/run
cd  ${LOCALDIR}/WRFV3/run
for file in `ls ${ROOTDIR}/runshared/ | grep -v met_em`
do 
  ln -s ${ROOTDIR}/runshared/$file .
done
cd -
#cd $LOCALDIR
#tar -xzf ${ROOTDIR}/WRF4Gbin-*.tar.gz
#rm ${ROOTDIR}/WRF4Gbin-*.tar.gz
#cd ${ROOTDIR}
