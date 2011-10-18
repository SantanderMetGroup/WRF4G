#!/bin/sh

/usr/bin/env > env.txt

if [ -f outfile ]; then
    I=`tail -n 1 outfile | cut -f1 -d' '`
else
    I=0
fi

while [ $I -lt $1 ]; do
    sleep $2
    DATE=`date`
    HOST=`uname -n`
    echo "$I $DATE $HOST"
    echo "$I $DATE $HOST" >> outfile
    I=`expr $I + 1`
done 
