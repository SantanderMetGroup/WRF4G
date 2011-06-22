#!/bin/bash 
if test $1 = '-h'
then
  echo "************************"
  echo "*** Shell to run any ***"
  echo "***  postprocessor   ***"
  echo "***   after WRF4G    ***"
  echo "************************"
  echo "wrf4g_run_postprocessor.sh 'WRF4Ghome' 'postprocessor'(only name) 'DIR'(folder with wrfouts -- absolute path)"
else
  source $1/wn/lib/bash/wrf_util.sh
  export PATH=$3/bin:${PATH}
  export LD_LIBRARY_PATH=$3/shared_libs:${LD_LIBRARY_PATH}

  cd $3
  wrfiles=`ls -1 wrfout*`
  for wrfile in ${wrfiles} 
  do
    cat << EOF > ../../rootdir
.
EOF
    if test ! -f $1/wn/bin/postprocessor.$2
    then
      echo "ERROR -- error -- ERROR -- error :'"$1"/wn/bin/postprocessor."$2"' does not exist!"
    else
      $1/wn/bin/postprocessor.$2 ${wrfile}
    fi
#    exit
### End of wrfiles
  done
  exit
  dirapps=`cat ../../rootdir`
  rm -rf ${dirapps}/bin/*
  rm -rf ${dirapps}/lib/*
  rmdir ${dirapps}/bin
  rmdir ${dirapps}/lib
  rm ../../rootdir  

fi
