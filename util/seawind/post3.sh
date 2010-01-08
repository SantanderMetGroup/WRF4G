#! /bin/bash
#PBS -q oceano

source /software/ScientificLinux/4.6/etc/bashrc

expname=Pswh2009

fullexp=SeaWind_I1540RF06
#postdir="/oceano/gmeteo/SCRATCH/ASNA/SEAWIND/SeaWind_I1540RF06__1989_2009"
postdir="/vols/tetis/escena/SEAWIND/SeaWind_I1540RF06__1989_2009"
sizethres=333981360

cd $postdir

for fx in ${expname}__*xtrm.nc
do
  fp=${fx/xtrm/plev}
  datei=$(echo $fx | sed -e 's/^.*\([12][0-9][0-9][0-9][01][0-9][0123][0-9]06\).*$/\1/')
  if test "${datei}" = "${fp}"; then
    echo Cannot get the date from $fp
    continue
  fi
  outname="${datei:0:4}/${fullexp}__${datei}.nc"
  if test -f "${outname}"; then
    echo "Cowardly refusing to overwrite $outname"
  else
    echo "Writing $outname"
    if test -f ${fp}; then
      cdo merge ${fp} ${fx} ${outname}
    else
      echo "Missing $fp, skipping..."
    fi
  fi
  if test $(stat -c %s ${outname}) -ge ${sizethres}; then
    rm -f ${fx} ${fp}
  else
    echo "Something went wrong! keeping the original files"
  fi
done
