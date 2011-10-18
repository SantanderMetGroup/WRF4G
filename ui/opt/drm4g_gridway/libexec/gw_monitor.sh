#!/bin/bash

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

#-------------------------------------------------------------------------------
#   Monitor script for the GW submission tool
#-------------------------------------------------------------------------------

# Kill a process tree
kill_process_tree ()
{
    PIDS=$1

    while [ -n "$PIDS" ]
    do
        unset NEW_CHILDS

        for PID in $PIDS
        do
            if [ -z "$NEW_CHILDS" ]
            then
                NEW_CHILDS="`ps ho pid --ppid $PID`"
            else
                NEW_CHILDS="$NEW_CHILDS `ps ho pid --ppid $PID`"
            fi

            #printf "Killing process `ps ho pid,cmd --pid $PID`... "

            kill $PID

            #echo "done."
        done

        PIDS=$NEW_CHILDS
    done
}

# Calculate the process tree load
calculate_process_tree_load ()
{
    PIDS=$1

    LOAD=0

    while [ -n "$PIDS" ]
    do
        unset NEW_CHILDS

        for PID in $PIDS
        do
            if [ -z "$NEW_CHILDS" ]
            then
                NEW_CHILDS="`ps ho pid --ppid $PID`"
            else
                NEW_CHILDS="$NEW_CHILDS `ps ho pid --ppid $PID`"
            fi

            LOAD_PID=`ps ho pcpu -p $PID | awk -F. '{print $1}'`
            LOAD=`echo $LOAD $LOAD_PID | awk '{printf "%f\n", $1 + $2}'`
        done

        PIDS=$NEW_CHILDS
    done

    LOAD=`echo $LOAD | awk '{printf "%.0f\n", $1}'`
}

JOB_PID=$1

echo "GW_CPULOAD_THRESHOLD=$GW_CPULOAD_THRESHOLD"

printf "JOB_LOAD \t CPU_IDLE\n"

while [ 1 ]
do
    CPU_IDLE=`vmstat 1 2 | tail -1 | awk '{print $15}'`
    #JOB_LOAD=`ps ho pcpu -p $JOB_PID --ppid $JOB_PID | awk 'BEGIN {sum=0} {sum+=$1} END {printf "%.0f\n", sum}'`
    calculate_process_tree_load $JOB_PID
    JOB_LOAD=$LOAD

    printf "$JOB_LOAD \t $CPU_IDLE\n"

    if [ $JOB_LOAD -lt $GW_CPULOAD_THRESHOLD -a $CPU_IDLE -lt $GW_CPULOAD_THRESHOLD ]
    then
        # Check if the job is still running
        ps -p $JOB_PID > /dev/null 2> /dev/null
        
        if [ $? -eq 0 ]
        then
    
            echo JOB_LOAD=$JOB_LOAD, CPU_IDLE=$CPU_IDLE > gw_perf

            # Warning! Check the existence and timestamps of restart files
            # ($GW_RESTART_FILES) before killing

            printf "Performance degradation detected! Killing job $JOB_PID... "

            kill_process_tree $JOB_PID

            echo "done."
        
            exit 0
        fi
    fi

done
