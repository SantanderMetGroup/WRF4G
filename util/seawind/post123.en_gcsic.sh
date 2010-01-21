#! /bin/bash

source /gpfs/csic_projects/meteo/software/ScientificLinux/4.6/etc/bashrc

exp="Pswh1999"
fullexp=SeaWind_I1540RF06
sizethres=333981360

postdir="/gpfs/csic_projects/meteo/METEO4G/SEAWIND/SeaWind_I1540RF06__1989_2009"
datadir="/gpfs/csic_projects/meteo/WORK/WRF/experiments/${exp}"

mkdir -p $postdir || exit
cd $postdir

weajdir="/home/fital/WRF4G/util/postprocess"
  weajpy="${weajdir}/wrfnc_extract_and_join.py"
  weajtbl="${weajdir}/wrfnc_extract_and_join.table"
  weajatt="${weajdir}/wrfnc_extract_and_join.gattr_ISEAWIND"
geofile="/gpfs/csic_projects/meteo/WORK/WRF/domains/Europe_15k/geo_em.d01.nc"

function pintnml(){
  idir=$1
  ifile=$2
  cat << EOF > namelist.pinterp
&io
  path_to_input            = '${idir}/',
  input_name               = '${ifile}',
  path_to_output           = '${idir}/',
  fields                   = 'Q2,T2,GHT,TT,RH,MSLP,PSFC,U10,V10,RAINTOT,Times',
  process                  = 'list',
  debug                    = .TRUE.,
  grid_filt                = 3,
  ntimes_filt              = 10,
  path_to_geofile          = '/gpfs/csic_projects/meteo/WORK/WRF/domains/Europe_15k',
  geofile                  = 'geo_em.d01.nc'
/

&interp_in
  interp_levels            = 1000.0, 850.0, 500.0, 200.0,
  p_top                    = 50.,
  extrapolate              = 1,
  interp_method            = 1,
/
EOF
}
pint="/home/fital/WRF4G/util/postprocess/Fortran/p_interp"

for dir in ${datadir}/${exp}__*
do
  read expname dates trash <<< ${dir//__/ }
  read datei datef <<< ${dates//_/ }
  if test -f "${datei:0:4}/${fullexp}__${datei}.nc"; then
    echo "There is a full file for this date ($datei). Won't waste my time"
    continue
  fi
  expname=$(basename ${expname})
  #
  # plev
  #
  outnamep=${expname}__${datei}_plev.nc
  filesp=$(\ls -1 ${dir}/output/wrfout*.nc)
  echo Writing $outnamep
  for f in ${filesp}; do
    echo "$f included in this set"
    if ! ncdump -h ${f} | grep -q num_metgrid_levels ; then
      echo "Somehow $f was not filtered at running time"
      echo "Filtering now!"
      indir=$(dirname $f)
      infile=$(basename $f)
      pintnml ${indir} ${infile}
      ${pint} >& ${f}_PLEV.log
      mv ${f} ${f}.old
      mv ${f}_PLEV ${f}
    fi
  done
  python ${weajpy} -p \
    -v PSFC,GHT,Q2,T2,U10ER,V10ER,MSLP,RAIN,RH,TEMP \
    -r 1940-01-01_00:00:00 \
    -t ${weajtbl} -a ${weajatt} \
    -g ${geofile} \
    -o ${outnamep} \
    ${filesp}
  #
  # xtrm
  #
  outnamex=${expname}__${datei}_xtrm.nc
  filesx=$(\ls -1 ${dir}/output/wrfxtrm*.nc)
  echo Writing $outnamex
  python ${weajpy} \
    -v U10XER,V10XER,U10MER,V10MER,T2MAX,T2MIN \
    -r 1940-01-01_00:00:00 \
    -t ${weajtbl} -a ${weajatt} \
    -g ${geofile} \
    -o ${outnamex} \
    ${filesx}
  #
  # merge and delete
  #
  outname="${datei:0:4}/${fullexp}__${datei}.nc"
  mkdir -p ${datei:0:4}
  echo "Writing $outname"
  cdo merge ${outnamep} ${outnamex} ${outname}
  if test $(stat -c %s ${outname}) -ge ${sizethres}; then
    rm -f ${outnamep} ${outnamex}
  else
    echo "Something went wrong! keeping the original files"
  fi
done
