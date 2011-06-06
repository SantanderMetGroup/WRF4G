#! /bin/bash

if test -z $WRF4G_LOCATION; then
   echo '$WRF4G_LOCATION variable not established. Please export WRF4G_LOCATION and try again'
   exit 1
fi

userdir=`pwd`

#
#  Prepare environment
#
export PATH=${WRF4G_LOCATION}/bin:${PATH}
export PYTHONPATH=${PYTHONPATH}:${WRF4G_LOCATION}/lib/python

#
#  Load configuration files
#
grep "DB_.*=" ${WRF4G_LOCATION}/etc/framework4g.conf | sed -e 's/\ *=\ */=/' > db4g.conf
export DB4G_CONF=`pwd`/db4g.conf ; source $DB4G_CONF; rm db4g.conf

WRF4G.py Experiment ps $1

