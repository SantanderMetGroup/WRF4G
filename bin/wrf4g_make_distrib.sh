#!/bin/bash
export LANG=C
if [ -z "$1" ]; then
  FOLDER="/tmp"
else
  FOLDER=$1
fi
echo "Creating WRF4G.tar.gz in ${FOLDER}"
mkdir -p ${FOLDER}/WRF4G
cp -R $PWD/../* ${FOLDER}/WRF4G
svn info>${FOLDER}/WRF4G/svn_info.txt
svn_tag=$(svn info | grep '^Revision:' | sed -e 's/^Revision: //')
cd ${FOLDER}/WRF4G
find ./ -name ".svn" | xargs rm -Rf
cd ..
tar -czvf WRF4G_${svn_tag}.tar.gz WRF4G
ln -s WRF4G_${svn_tag}.tar.gz WRF4G.tar.gz
rm -rf WRF4G

