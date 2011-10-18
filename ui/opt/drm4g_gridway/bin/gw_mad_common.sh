# --------------------------------------------------------------------------
# Copyright 2002-2011, GridWay Project Leads (GridWay.org)          
#
# Licensed under the Apache License, Version 2.0 (the "License"); you may
# not use this file except in compliance with the License. You may obtain
# a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
# --------------------------------------------------------------------------

function check_globus {
    if [ -z "${GLOBUS_LOCATION}" ]; then
        echo "Please, set GLOBUS_LOCATION variable."
        exit -1
    fi
}

function setup_globus {
    check_globus
    if [ -f $GLOBUS_LOCATION/etc/globus-user-env.sh ]; then
        . $GLOBUS_LOCATION/etc/globus-user-env.sh
    fi
    if [ -f $GLOBUS_LOCATION/etc/globus-devel-env.sh ]; then
        . $GLOBUS_LOCATION/etc/globus-devel-env.sh
    fi
}

function cd_var {
    GW_VAR_DIR=var
    if [ -d "${GW_LOCATION}/var/gridway" ]; then
        GW_VAR_DIR=var/gridway
    fi
    cd $GW_LOCATION/$GW_VAR_DIR
}

function mad_debug {
    if [ -n "${MADDEBUG}" ]; then
        ulimit -c 15000
    fi
}

function export_rc_vars {
    if [ -f $1 ] ; then
        GW_VARS=`cat $1 | egrep -e '^[a-zA-Z\-\_0-9]*=' | sed 's/=.*$//'`

        . $1

        for v in $GW_VARS; do
            export $v
        done
    fi
}

# Check and set X509_USER_PROXY
function check_proxy {
    grid-proxy-info -exists 2> /dev/null

    if [ "x$?" = "x0" ]; then
        export X509_USER_PROXY=`grid-proxy-info -path`
    else
        echo "Please, set your proxy certificate."
        exit -1
    fi
}

export CLASSPATH=$CLASSPATH:$GW_LOCATION/bin

# Set global environment

export_rc_vars $GW_LOCATION/etc/gwrc

export_rc_vars $GW_LOCATION/etc/gridway/gwrc

# Set per user environment

export_rc_vars ~/.gwrc

# Sanitize PRIORITY variable
if [ -z "$PRIORITY" ]; then
    export PRIORITY=19
fi
