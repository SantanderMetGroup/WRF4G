#! /bin/bash
#PBS -q tetis

rundir="/oceano/gmeteo/SCRATCH/chus/tmp/tmp.$(date +'%Y%m%d%H%M%S%N')"
mkdir -p ${rundir}
cd ${rundir}

source /software/ScientificLinux/4.6/etc/bashrc

exp=Pswh2009

datadir="/vols/tetis/escena/METEO4G/WRF/experiments/${exp}"
#datadir="/oceano/gmeteo/SCRATCH/MDM.UC/experiments/${exp}"

pint="/oceano/gmeteo/WORK/chus/wrf4g/util/postprocess/Fortran/p_interp"

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
  path_to_geofile          = '/oceano/gmeteo/DATA/WRF/domains/Europe_15k',
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


for dir in ${datadir}/${exp}__*
do
  read expname dates trash <<< ${dir//__/ }
  read datei datef <<< ${dates//_/ }
  expname=$(basename ${expname})
  for file in ${dir}/output/wrfout*.nc
  do
    if test -f "${file}_PLEV"; then
      xptdsize=$( echo "print int($(stat -c %s ${file})*0.06)" | python )
      if test "$(stat -c %s ${file}_PLEV)" -ge ${xptdsize}; then
        echo "Cowardly refusing to overwrite ${file}_PLEV"
        continue
      fi
    fi
    echo Writing ${file}_PLEV
    indir=$(dirname $file)
    infile=$(basename $file)
    pintnml ${indir} ${infile}
    ${pint} >& ${file}_PLEV.log
  done
done
