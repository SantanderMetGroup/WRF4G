#! /bin/bash
#PBS oceano

source /software/ScientificLinux/4.6/etc/bashrc

fullexp=SeaWind_N3028RF06
postdir="/vols/tetis/meteo4g/SEAWIND/SeaWind_N3028RF06__1948_2009"
postrf0618="/vols/tetis/escena/SEAWIND/SeaWind_N3028RF0618__1948_2009"
vars="psl,uasmax,vasmax,uasmean,vasmean"

tag=$(date +'%Y%m%d%H%M%S')
mkdir -p $postrf0618
cd $postrf0618

yearmons=$( \
  find -L ${postdir} -name "${fullexp}__*.nc" \
    | sed -e 's/^.*__\([12][0-9][0-9][0-9][0123][0-9]\).*$/\1/' \
    | sort | uniq \
)

lastf=""
for ym in $yearmons; do
  outfile="SeaWind_N3028RF0618_${ym}.nc"
  echo "Processing $outfile ..."
  if test -f "$outfile"; then
    echo "refusing to overwrite $outfile"
    do_this_month="no"
  else
    do_this_month="yes"
  fi
  for file in $lastf ${postdir}/????/${fullexp}__${ym}*.nc
  do
    if test ${do_this_month} = "yes"; then
      trash=$(basename $file)
      read expname rest <<< ${trash//__/ }
      read datei trash <<< ${rest//_/ }
      date1=$(date --utc '+%Y-%m-%dT%H:%M:%S' -d "${datei:0:8} 18 hours")
      date2=$(date --utc '+%Y-%m-%dT%H:%M:%S' -d "${datei:0:8} 41 hours")
      outname=${tag}__$(echo ${date1:0:13} | tr -d 'T-').nc
      echo Writing $outname
      cdo seldate,${date1},${date2} -selvar,${vars} $file ${outname} 
    fi
  done
  if test ${do_this_month} = "yes"; then
    rm -f s2.nc
    cdo cat ${tag}__*.nc s2.nc
    cdo selmon,${ym:4:2} s2.nc ${outfile}
    rm ${tag}__*.nc
    rm -f s2.nc
  fi
  lastf=$file
done
