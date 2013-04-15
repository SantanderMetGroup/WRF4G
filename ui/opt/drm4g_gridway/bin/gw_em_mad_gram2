#!/bin/bash

if [ -z "${GW_LOCATION}" ]; then
    export GW_LOCATION=`dirname $0`
fi

. $GW_LOCATION/bin/gw_mad_common.sh

setup_globus
cd_var
mad_debug
check_proxy

exec nice -n $PRIORITY $GW_LOCATION/bin/gw_em_mad_gram2.bin $*
