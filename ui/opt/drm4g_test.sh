#!/bin/bash 

if [ -z "${GW_LOCATION}" ]; then
    echo "Please, set GW_LOCATION variable."
    exit -1
fi

if [ ! -f $GW_LOCATION/etc/hosts_drm4g_copy.list ]; then
    mv $GW_LOCATION/etc/hosts_drm4g.list $GW_LOCATION/etc/hosts_drm4g_copy.list
fi

printf "Creating a new hosts_drm4g.list... "
echo "mycomputer  local://localhost?LRMS_TYPE=fork;NODECOUNT=1" > $GW_LOCATION/etc/hosts_drm4g.list
echo "PBS_cluster ssh://user@host?LRMS_TYPE=pbs;QUEUE_NAME=grid;NODECOUNT=1" >> $GW_LOCATION/etc/hosts_drm4g.list
echo "done"

rm -f $GW_LOCATION/var/gwd.port
rm -f $GW_LOCATION/var/.lock

printf "Starting up GWD.. "
gwd -c
sleep 2 
gwd_port=$(cat $GW_LOCATION/var/gwd.port  | awk '{print $2}')
if [ -z $gwd_port ]; then
    echo "Starting up failed!"
    exit -1
else
    echo "done"
fi
gwd_pid=$(netstat -atnp 2>/dev/null | grep gwd | grep $gwd_port | awk '{print $7}'| sed s#/gwd##)

sleep 15
gwhost

printf "Submitting 3 jobs... "
stdout=`gwsubmit -v -t $GW_LOCATION/examples/uname/uname.jt -n 3`
if [ $? -ne 0 ]; then
    echo "Submission failed!"
    exit -1
else 
    echo "done"
fi

sleep 5
gwps
sleep 10
gwps
sleep 15
gwps

printf "Waiting for the array to finish... "
gwwait -A `echo $stdout | head -1 | awk '{print $3}'`
echo "done"

mv $GW_LOCATION/etc/hosts_drm4g_copy.list $GW_LOCATION/etc/hosts_drm4g.list

echo "Check the result"
cat $GW_LOCATION/examples/uname/std*
rm $GW_LOCATION/examples/uname/std*
kill -9 $gwd_pid
rm -rf $GW_LOCATION/var/*
rm $GW_LOCATION/var/.[dls]*

