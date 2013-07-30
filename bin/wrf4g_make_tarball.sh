#! /bin/bash

version="1.0"

scriptdir=$( (cd `dirname $0` && echo $PWD) )
basedir=$(dirname ${scriptdir})

while test -n "$1"; do
  case $1 in
    "-tag")
      tag=$2
      shift
      ;;
    "-destdir")
      destdir=$2
      shift
      ;;
    *)
      echo "Usage: $(basename $0) [-tag tag] [-destdir /destination/for/file.tar.gz]" 
      exit
      ;;
  esac
  shift
done

if test -z "${destdir}"; then
  destdir="${basedir}/var"
fi

cd ${basedir}
 tar czv -h --exclude=".svn" -f ${destdir}/WRF4G-${version}${tag}.tar.gz bin lib
