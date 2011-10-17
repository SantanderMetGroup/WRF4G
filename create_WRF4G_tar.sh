#!/bin/bash
FOLDER="/tmp"
echo "Creating WRF4G.tar.gz in ${FOLDER}"
cd ui/opt/drm4g_gridway-5.7
svn info>svn_info.txt
cd -
cp -RpL ui/ ${FOLDER}/
svn info>${FOLDER}/ui/svn_info.txt
cd ${FOLDER}/
mv ui WRF4G
cd WRF4G
find ./ -name ".svn" | xargs rm -Rf
cd ..
tar -czvf WRF4G.tar.gz WRF4G
rm -rf WRF4G

