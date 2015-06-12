#!/bin/bash

#__version__  = '2.0.0'
#__author__   = 'Carlos Blanco'
#__revision__ = "$Id$"

BASE_URL="https://meteo.unican.es/work/WRF4G"
WRF4G_DEPLOYMENT_DIR=$PWD
WRF4G_HARDWARE=$(uname -m)
FILE_VERSIONS="wrf4g_${WRF4G_HARDWARE}_versions"

have_command () {
    type "$1" >/dev/null 2>/dev/null
}

require_command () {
    have_command "$1" 
    rc=$?
    if [ $rc -ne 0 ]
    then
        echo "Could not find required command '$1' in system PATH."
        exit 1
    fi
}

require_python () {
    require_command "python"
    # Support 2.5 >= python < 3.0 
    python_version=$(python <<EOF
import sys
print(sys.version_info[0]==2 and sys.version_info[1] >= 5 )
EOF
)

    if [ "$python_version" != "True" ] 
    then
        echo "Wrong version of python is installed" 
        echo "WRF4G requires Python version 2.5+"
        echo "It does not support your version of"
        echo "python: $(python -V 2>&1|sed 's/python//gi')"
        exit 1
    fi
}

download_wrf4g() {
    wget -N -nv --no-check-certificate $BASE_URL/$WRF4G_BUNDLE
    rc=$?
    if [ $rc -ne 0 ]
    then
        echo "ERROR: Unable to download bunble $WRF4G_BUNDLE from $BASE_URL ..."
        exit 1
    fi
}

download_wrf4g_versions() {
    wget -N -nv --no-check-certificate $BASE_URL/$FILE_VERSIONS
    rc=$?
    if [ $rc -ne 0 ]
    then
        echo "ERROR: Unable to download $FILE_VERSIONS from $BASE_URL ..."
        exit 1
    fi
}



unpack_wrf4g() {
    tar xzf $WRF4G_BUNDLE --overwrite -C $WRF4G_DEPLOYMENT_DIR
    rc=$?
    if [ $rc -ne 0 ]
    then
        echo "ERROR: Unable to unpack the bunble $WRF4G_BUNDLE in $WRF4G_DEPLOYMENT_DIR"
        exit 1
    fi
}


usage () {
    cat <<EOF
This program installs WRF4G.

usage:
$0 [OPTIONS]

Options:

      -d, --dir DIRECTORY    Install WRF4G into a directory.
                             (Default: $WRF4G_DEPLOYMENT_DIR)

      -V, --version          Version to install.
                             (Default: $WRF4G_VERSION)

      -h, --help             Print this help text.

EOF
}

######### 
while test -n "$1"
do
    case "$1" in
        -d|--dir)
            shift
            WRF4G_DEPLOYMENT_DIR=$1
            ;;
        -h|--help)
            usage
            exit 0
            ;;
        -V|--version)
            WRF4G_VERSION=$1       
            ;;
        *)
            echo "Unknown option: $1"
            usage
            exit 1
            ;;
    esac
    shift
done

cat <<EOF
==========================
WRF4G installation script
==========================
EOF

# Check wget and python  
require_command wget

require_python

if [ -n $WRF4G_VERSION  ]
then
    echo ""
    echo "--> Downloading $FILE_VERSIONS from $BASE_URL ..."
    echo ""
    download_wrf4g_versions
    WRF4G_VERSION=$(sort $FILE_VERSIONS | tail -1)
    rm -rf $FILE_VERSIONS
fi
echo ""
echo "This script will install WRF4G version: $WRF4G_VERSION"

WRF4G_BUNDLE="wrf4g-${WRF4G_VERSION}-${WRF4G_HARDWARE}.tar.gz"
echo ""
echo "--> Downloading $WRF4G_BUNDLE from $BASE_URL ..."
echo ""

if [ -f $WRF4G_BUNDLE ]
then
    echo "WARNING: $WRF4G_BUNDLE already exists"
    read -p "Are you sure you want to download it? [y/N] " response
    case $response in y|Y|yes|Yes) download_wrf4g;; *);; esac
else
    download_wrf4g
fi

echo ""
echo "--> Unpacking $WRF4G_BUNDLE in directory $WRF4G_DEPLOYMENT_DIR ..."
echo ""

if [ -d "$WRF4G_DEPLOYMENT_DIR/wrf4g" ]
then
    echo "WARNING: $WRF4G_DEPLOYMENT_DIR/wrf4g directory already exists"
    read -p "Are you sure you want to install it there? [y/N] " response
    case $response in y|Y|yes|Yes) $WRF4G_DEPLOYMENT_DIR/wrf4g/bin/wrf4g stop; unpack_wrf4g;; *);; esac
else
    unpack_wrf4g
fi

rm -rf $WRF4G_BUNDLE

cat <<EOF
====================================
Installation of WRF4G $WRF4G_VERSION is done!
====================================

In order to work with WRF4G you have to enable its 
environment with the command:

    . $WRF4G_DEPLOYMENT_DIR/wrf4g/bin/wrf4g_init.sh

You need to run the above command on every new shell you 
open before using WRF4G, but just once per session.

EOF

