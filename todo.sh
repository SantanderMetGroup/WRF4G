#!/bin/bash
wrf4g_framework stop
rm -rf /tmp/WRF4G*
bash create_WRF4G_tar.sh
cd /tmp
tar -xzvf WRF4G.tar.gz
export WRF4G_LOCATION=/tmp/WRF4G
export PATH=$WRF4G_LOCATION/bin:$PATH
wrf4g_framework start
cd WRF4G/experiments/single_test
wrf4g_prepare
wrf4g_submit
