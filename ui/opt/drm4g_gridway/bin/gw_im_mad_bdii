#!/bin/bash

# --------------------------------------------------------------------------
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
# --------------------------------------------------------------------------
LOCKDIR=".gw_im_mad_bdii.lock"

lock_stdout ()
{
    if ! mkdir $LOCKDIR; then
        printf "Warning: Lock failed, waiting\n" 1>&2
        i=0
        until mkdir $LOCKDIR; do
            sleep 0.01
            ((i++))
	    if [ $i -eq 1000 ]; then
                echo "`date` [Error]: Lock $1 waited over 10 seconds, exiting...\n" >> $LOCKDIR.info
		touch $LOCKDIR.info
                exit 1
	    fi
        done
	if ! [ -e $LOCKDIR.info ]; then
	    touch $LOCKDIR.info
	fi
	printf "`date` [Warning]: Lock $1 released after $i centiseconds\n" >> $LOCKDIR.info
    fi
}

unlock_stdout ()
{
    rmdir $LOCKDIR
}

dynamic_discover ()
{
	TMPFILE=".search.$$.$RANDOM"
	$GW_LOCATION/libexec/perl/gw_im_mad_bdii.pl DISCOVER $SERVER $QUEUEFILTER $TMPFILE
	lock_stdout "$$.$RANDOM"
	cat $TMPFILE
	rm $TMPFILE
        unlock_stdout

}

dynamic_monitor ()
{
	TMPFILE=".search.$$.$1.$RANDOM"
	$GW_LOCATION/libexec/perl/gw_im_mad_bdii.pl MONITOR $SERVER $1 $2 $QUEUEFILTER $TMPFILE
        lock_stdout "$$.$1.$RANDOM"
        cat $TMPFILE
        rm $TMPFILE
        unlock_stdout
}

# Common initialization
if [ -z "${GW_LOCATION}" ]; then
    echo "Please, set GW_LOCATION variable."
    exit -1
fi

. $GW_LOCATION/bin/gw_mad_common.sh

setup_globus

# Common initialization
if [ -z "${GW_LOCATION}" ]; then
    echo "Please, set GW_LOCATION variable."
    exit -1
fi

. $GW_LOCATION/bin/gw_mad_common.sh

setup_globus

. $GW_LOCATION/bin/gw_im_mad_common.sh
