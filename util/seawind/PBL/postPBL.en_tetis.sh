#! /bin/bash
#PBS -q tetis

source /software/ScientificLinux/4.6/etc/bashrc

exp="swhI1540_MultiBL"
pbl="1_1"
fullexp=SeaWind_I1540_MultiBL
sizethres=333981360

postdir="/oceano/gmeteo/WORK/markel/master/swhI1540_MultiBL/" #Cambiar por ¿?
datadir="/oceano/gmeteo/WORK/meteo4g/WRF/experiments/${exp}"

mkdir -p $postdir || exit
cd $postdir

weajdir="/oceano/gmeteo/WORK/markel/wrf4g/util/postprocess"
  weajpy="${weajdir}/wrfnc_extract_and_join.py"
  weajtbl="${weajdir}/wrfnc_extract_and_join.table"
  weajatt="${weajdir}/wrfnc_extract_and_join.gattr_ISEAWIND"
geofile="/oceano/gmeteo/WORK/MDM.UC/WRF/domains/Europe_15k/geo_em.d01.nc"

function pintnml(){
  idir=$1
  ifile=$2
  cat << EOF > namelist.pinterp
&io
  path_to_input            = '${idir}/',
  input_name               = '${ifile}',
  path_to_output           = '${idir}/',
  fields                   = 'Times,Q2,QVAPOR,T2,GHT,TT,U,V,W,RH,MSLP,PSFC,U10,V10,RAINC,RAINNC,PRES,ACLHF,ACHFX,LH,QFX,HFX,PBLH,UST,TKE_MYJ,EL_MYJ,QKE,Z0,POTEVP,REGIME',
  process                  = 'list',
  debug                    = .TRUE.,
  grid_filt                = 3,
  ntimes_filt              = 10,
  path_to_geofile          = '/oceano/gmeteo/DATA/WRF/domains/Europe_15k',
  geofile                  = 'geo_em.d01.nc'
/

&interp_in
  interp_levels            = 1000.,987.5,975.,962.5,950.,937.5,925.,912.5,900.,887.5,875.,850.,825.,800.,750.,700.,650.,600.,500.,
  p_top                    = 50.,
  extrapolate              = 1,
  interp_method            = 1,
  unstagger_grid           = .TRUE., 
/
EOF
}
pint="/oceano/gmeteo/WORK/chus/wrf4g/util/postprocess/Fortran/p_interp"

for dir in ${datadir}/${exp}__${pbl}__*
do
  read expname pbl dates trash <<< ${dir//__/ }
  read datei datef <<< ${dates//_/ }
  if test -f "${datei:0:4}/${fullexp}__${pbl}__${datei}.nc"; then
    echo "There is a full file for this date ($datei). Won't waste my time"
    continue
  fi
  expname=$(basename ${expname})
  #
  # plev
  #
  outnamep=${expname}__${pbl}__${datei}_plev.nc
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
    -v PSFC,GHT,Q2,T2,U10ER,V10ER,UER,VER,MSLP,W,RH,TEMP,Q2,QVAPOR,T2,RAINC,RAINNC,ACLHF,ACHFX,LH,QFX,HFX,PBLH,UST,Z0,POTEVP,REGIME \
    -r 1940-01-01_00:00:00 \
    -t ${weajtbl} -a ${weajatt} \
    -g ${geofile} \
    -o ${outnamep} \
    ${filesp}
  #
  # xtrm
  #
  outnamex=${expname}__${pbl}__${datei}_xtrm.nc
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
  outname="${datei:0:4}/${fullexp}__${pbl}__${datei}.nc"
  mkdir -p ${datei:0:4}
  echo "Writing $outname"
  cdo merge ${outnamep} ${outnamex} ${outname}
  if test $(stat -c %s ${outname}) -ge ${sizethres}; then
    rm -f ${outnamep} ${outnamex}
  else
    echo "Something went wrong! keeping the original files"
  fi
done
