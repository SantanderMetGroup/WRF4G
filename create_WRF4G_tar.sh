#!/bin/bash
export LANG=C
FOLDER="/tmp"
echo "Creating WRF4G.tar.gz in ${FOLDER}"
cp -RpL ui/ ${FOLDER}/
svn info>${FOLDER}/ui/svn_info.txt
svn_tag=$(svn info | grep '^Revision:' | sed -e 's/^Revision: //')
cd ${FOLDER}/
mv ui WRF4G
cd WRF4G
find ./ -name ".svn" | xargs rm -Rf
cd ..
tar -czvf WRF4G_${svn_tag}.tar.gz WRF4G
ln -s WRF4G_${svn_tag}.tar.gz WRF4G.tar.gz
rm -rf WRF4G

