#! /bin/bash
#
#  Wrapper to collect locally the std and err output and send the
#  launch_operativo.sh to the background.
#
sdate=$1

bash -x /oceano/gmeteo/WORK/chus/experimentos/operativo/launch_operativo.sh $sdate >& launch_operativo_${sdate.log} &
