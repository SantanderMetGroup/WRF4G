#!/bin/bash
wrf4g_framework stop
rm -rf /tmp/WRF4G*
bash create_WRF4G_tar.sh
cd /tmp
tar -xzvf WRF4G.tar.gz
wrf4g_framework start
cd WRF4G/experiments/single_test
sleep 10
wrf4g_prepare
wrf4g_submit
