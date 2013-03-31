#!/bin/bash
# ------------------------------------------------------------------------------
# Copyright 2002-2012, GridWay Project Leads (GridWay.org)          
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
# ------------------------------------------------------------------------------

# Help
while getopts hu:gi option
    do
        case $option in
        h)   echo -e "USAGE\n gw_tm_mad_dummy [-h] [-u|-g|-i]" \
                     "\n\nSYNOPSIS"\
                     "\n  Transfer driver to interface with GridFTP and GASS servers. It is not intended to be used from CLI."\
                     "\n\nOPTIONS"\
                     "\n  -h    print this help"\
                     "\n  -u    URL of the GridFTP server"\
                     "\n  -g    use a user GASS server to transfer files"\
                     "\n  -i    use a user GASS insecure server to transfer files";
             exit 0;;
        [?]) echo -e "usage: gw_tm_mad_dummy [-h] [-u|-g|-i]";
             exit 1;;
        esac
    done

# Common initialization
if [ -z "${GW_LOCATION}" ]; then
    echo "Please, set GW_LOCATION variable."
    exit -1
fi

. $GW_LOCATION/bin/gw_mad_common.sh

setup_globus
cd_var
mad_debug
check_proxy

exec nice -n $PRIORITY $GW_LOCATION/bin/gw_tm_mad_dummy.bin $*
