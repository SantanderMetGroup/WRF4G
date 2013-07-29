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

SCRIPT_NAME=`basename $0`

discover (){
    unset INFO

    # Static discovery
    if [ -n "$HOSTLIST" ]
    then
        if [ -f "$HOSTLIST" ]
        then
            while read -a HOSTPAIR
            do
                HOSTNAME=${HOSTPAIR[0]}
        
                INFO="$INFO $HOSTNAME"
            done < $HOSTLIST
        else
            echo "DISCOVER - FAILURE Can't access $HOSTLIST file (cwd is `pwd`)"
            return
        fi
    fi
    
    dynamic_discover $1 $2
}

get_static_info (){
	HOSTNAME=`echo $HOSTPAIR | awk '{print $1}'`
	HOSTFILE=`echo $HOSTPAIR | awk '{print $2}'`

	if [ -n "$HOSTFILE" ]
	then
	    if [ ! -f "$HOSTFILE" ]
	    then
	        HOSTFILE="$GW_LOCATION/$HOSTFILE"
	    fi

	    if [ -f "$HOSTFILE" ]
	    then
	        # Static monitoring
	        unset INFO

	        while read ATTR
	        do
	            INFO="$INFO $ATTR"
	        done < $HOSTFILE
    
	        #echo "MONITOR $1 SUCCESS $INFO"
	        echo -n "$INFO"
	    else
	        #echo "MONITOR $1 FAILURE Can't access $HOSTFILE file (cwd is `pwd`)"
			return
	    fi
	    return
	fi
}

monitor (){
    HOSTPAIR=`grep "^$2 " $HOSTLIST`
	HOSTPAIR_RET=$?
    
    if [ "x$SCRIPT_NAME" = "xgw_im_mad_static" ]
    then
		INFO=`get_static_info`
		if [ -n "$INFO" ]; then
			echo "MONITOR $1 SUCCESS $INFO"
		else
			echo "MONITOR $1 FAILURE Can't access $HOSTFILE file (cwd is `pwd`)"
		fi
    else
		if [ $HOSTPAIR_RET -eq 0 ]
		then
			STATIC_INFO=`get_static_info`
			dynamic_monitor $1 $2 "$STATIC_INFO"
		else
        	dynamic_monitor $1 $2
		fi
    fi
}

while getopts hs:b:f:q:l:p: option $@ 
do
    case $option in
    s)  SERVER="$OPTARG";;
    b)  BASE="$OPTARG";;
    f)  HOSTFILTER="$OPTARG";;
    q)  QUEUEFILTER="$OPTARG";;
    l)  HOSTLIST="$OPTARG"
        if [ ! -f "$HOSTLIST" ]
        then
            HOSTLIST="$GW_LOCATION/$HOSTLIST"
        fi
        ;;
    p) PORTNUMBER="$OPTARG";;
    h|*)  printf "Usage: %s: [-s SERVER] [-b BASE] [-f HOSTFILTER] [-q QUEUEFILTER] [-l HOSTLIST]\n" $0
        exit 2;;
    esac
done

while read COMMAND HID HOST ARGS
do
    case $COMMAND in
        "INIT" | "init")
            echo "INIT - SUCCESS -"
            ;;
        "DISCOVER" | "discover")
            discover &
            ;;
        "MONITOR" | "monitor")
            monitor $HID "$HOST" &
            ;;
        "FINALIZE" | "finalize")
            echo "FINALIZE - SUCCESS -"
            exit 0
            ;;
        *)
            echo "$COMMAND - FAILURE Unknown command"
            ;;
    esac

    unset COMMAND
    unset HID
    unset HOST
    unset ARGS

done
