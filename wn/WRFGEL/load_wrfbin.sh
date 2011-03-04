#!/bin/bash
ROOTDIR=$PWD
echo $ROOTDIR
LOCALDIR=$(cat localdir)
mkdir -p ${LOCALDIR}
cd $LOCALDIR
tar -xzf ${ROOTDIR}/WRF4Gbin-*.tar.gz
rm ${ROOTDIR}/WRF4Gbin-*.tar.gz
cd ${ROOTDIR}
