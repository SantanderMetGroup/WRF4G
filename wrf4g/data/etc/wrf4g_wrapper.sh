#!/bin/bash -xv

# -------------------------------------------------------------------------- 
# Copyright 2002-2010, GridWay Project Leads (GridWay.org)                   
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

#-------------------------------------------------------------------------------
#   Wrapping script for the GW submission tool
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#          Check environment and remote job directory on remote machine
#-------------------------------------------------------------------------------

setup(){

    if [ -z "${GW_JOB_ID}" \
            -o -z "${GW_USER}" \
            -o -z "${GW_HOSTNAME}" ]; then
         echo "failed."
         exit 1
    fi
    
    cd `dirname $0`

    printf "`date`: Checking job environment... "
    
    source ./job.env
	
    if [ -z "${GW_RESTARTED}" -o -z "${GW_EXECUTABLE}" ]; then
        echo "failed."
        exit 1
    fi

    echo "done."
}


execution(){

    #
    #   Execution of actual job in background
    #
    
    case ${GW_EXECUTABLE} in
    
        gsiftp://*)
            GW_EXECUTABLE=`basename ${GW_EXECUTABLE}`
            ;;
            
        file:/*)
            GW_EXECUTABLE=`basename ${GW_EXECUTABLE}`        
            ;;
                
        *)
            ;;
    esac
			
    printf "`date`: Uncompressing file WRF4G package ... "

    tar xzf WRF4G.tar.gz  
 
    if [ $? -eq 0 ]; then
        
        echo "done."
    else
      
        echo "failed."
        exit 1
    fi
   
    ulimit -s unlimited
 
    printf "`date`: Setting the LD_LIBRARY_PATH and PATH ... "

    export PATH=.:./bin:$PATH

    export PYTHONPATH=./lib/python/:./lib/python/site-packages:$PYTHONPATH

    export LD_LIBRARY_PATH=./lib:./lib64:$LD_LIBRARY_PATH

    echo "done."
 
    chmod +x ./bin/*

    export DB4G_CONF=$PWD/db.conf

    printf "`date`: Executing actual job \"$GW_EXECUTABLE $GW_ARGUMENTS\"... "

    if [ -f stdin.execution ]; then
       STDIN_FILE=stdin.execution
    else
       STDIN_FILE=/dev/null
    fi
      
    ${GW_EXECUTABLE} ${GW_ARGUMENTS} $@ < ${STDIN_FILE} >> stdout.execution 2>> stderr.execution &
    
    JOB_PID=$!

    echo "done."
    echo "`date`: PID of actual job is $JOB_PID."

    #
    #   Execution of monitor in background, if defined
    #

    if [ -f .monitor ]
    then
        printf "`date`: Executing monitor \".monitor $JOB_PID\"... "

        chmod +x .monitor
        
        .monitor $JOB_PID >> stdout.monitor 2>> stderr.monitor &
        
        MONITOR_PID=$!

        if [ $? -eq 0 ]
        then   
            echo "done."
        else
            echo "failed."
        fi

        echo "`date`: PID of monitor is $MONITOR_PID."
    fi

    #
    # 	Wait for actual job termination
    #

    printf "`date`: Waiting for actual job to finish... "

    wait $JOB_PID

    EXIT_STATUS=$?

    echo "done."

    kill %1

    #
    #   Check performance degradations or self-migrations
    #

    printf "`date`: Checking performance degradations or self-migrations... "

    if [ -f gw_rank ]
    then
        EXIT_STATUS="S"
    elif [ -f gw_reqs ]
    then
        EXIT_STATUS="S"
    elif [ -f gw_perf ]
    then
        EXIT_STATUS="P"
    fi
    
    echo "done." 

    #
    #   Kill monitor
    #
    if [ -f .monitor \
            -a "${EXIT_STATUS}" != "P" ]
    then
        printf "`date`: Killing monitor... "

        kill %2
        
        if [ $? -eq 0 ]
        then   
            echo "done."
        else
            echo "failed."
        fi
    fi

    echo "EXIT_STATUS=$EXIT_STATUS"

    if [ -f gw_rank ]
    then
        echo "NEW_RANK=`cat gw_rank`"
    fi

    if [ -f gw_reqs ]
    then
        echo "NEW_REQS=`cat gw_reqs`"
    fi

    if [ -f gw_perf ]
    then
        echo "PERF_DEGR=`cat gw_perf`"
    fi
}    


transfer_input_files(){

    if [ -z "$GW_STDIN_FILE" ]; 
    then
        STG_FILES="$GW_EXECUTABLE,$GW_INPUT_FILES"
    else
        STG_FILES="$GW_EXECUTABLE,$GW_STDIN_FILE stdin.execution,$GW_INPUT_FILES"
    fi
    
    if [ -n "$GW_MONITOR" ];
    then
        STG_FILES_SAVED=$STG_FILES
        STG_FILES="$STG_FILES_SAVED,file://$GW_MONITOR .monitor"    
    fi 
    
    SAVED_IFS=$IFS
    IFS=","

    for FILES in $STG_FILES; do
        if [ -z "$FILES" -o "$FILES" = " " ]; then
          continue
        fi

        SRC_FILE=`echo $FILES | awk '{print $1}'`
        DST_FILE=`echo $FILES | awk '{print $2}'`

        case ${SRC_FILE} in
        
        gsiftp://*)
            SERVER=`echo $SRC_FILE | awk -F/ '{print $3}'`
            FILE_NAME=`echo $SRC_FILE | awk -F/ '{print $NF}'`
            FILE_PATH=`echo $SRC_FILE | awk -F/ \
                '{ORS=""; for (i=4;i<=NF;i++) print "/" $i; print "\n"}'`
            
            if [ -z "$DST_FILE" ]; then
                DST_FILE=$FILE_NAME
            fi
	
            SRC_URL="${SRC_FILE}"

            DST_URL="file:${RMT_JOB_HOME}/${DST_FILE}"

            printf "`date`: Staging-in remote file \"${SRC_FILE}\" as \"${DST_FILE}\"... "
             
            ${GLOBUS_CP} ${SRC_URL} ${DST_URL}

            if [ $? -ne 0 ]; then
                echo "failed."
            else
                echo "done."
            fi
            ;;
        
        *)
            ;;
        esac
	
    done
    
    IFS=$SAVED_IFS
}

transfer_output_files(){

    # TODO: An output file destination could be a GridFTP URL!!!

    STG_FILES="$GW_OUTPUT_FILES,stdout.execution $GW_STDOUT,stderr.execution $GW_STDERR"

    if [ -f .monitor ]
    then
        STG_FILES="$STG_FILES,stdout.monitor,stderr.monitor"
    fi

    SAVED_IFS=$IFS
    IFS=","

    for FILES in ${STG_FILES}; do
    	
        if [ -z "$FILES" -o "$FILES" = " " ]; then
          continue
        fi

        SRC_FILE=`echo $FILES | awk '{print $1}'`
        DST_FILE=`echo $FILES | awk '{print $2}'`

        if [ ! -f ${RMT_JOB_HOME}/${SRC_FILE} ]; then
            printf "`date`: Skipping staging-out of non-existent output file \"${SRC_FILE}\"... "
            echo "done."
            continue
        fi

        if [ -z "$DST_FILE" ]; then
            DST_FILE=$SRC_FILE
        fi

        case ${DST_FILE} in
        
        gsiftp://*)
            if [ -z "$SRC_FILE" ]; then
                SRC_FILE=$DST_FILE
            fi

            SRC_URL="file:${RMT_JOB_HOME}/${SRC_FILE}"
            DST_URL="${DST_FILE}"
            printf "`date`: Staging-out file \"${SRC_FILE}\" as \"${DST_FILE}\"... "

            ${GLOBUS_CP} ${SRC_URL} ${DST_URL}

            if [ $? -ne 0 ]; then
                echo "failed."
            else
                echo "done."
            fi
            ;;

        *)
            ;;
        esac

    done

    IFS=$SAVED_IFS
}

#-------------------------------------------------------------------------------
#                       WRAPPER SCRIPT
#-------------------------------------------------------------------------------

SECONDS=0

JOB_ENV_URL=$1

setup

transfer_input_files

execution

transfer_output_files

echo "REAL_TIME=$SECONDS"
