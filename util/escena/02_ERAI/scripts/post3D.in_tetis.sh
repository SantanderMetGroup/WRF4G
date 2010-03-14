#! /bin/bash
thisdir=$(pwd)
scriptdir=$( (cd `dirname $0` && echo $PWD) )
basedir=$(dirname $scriptdir)
source ${scriptdir}/dirs
source ${scriptdir}/env.${HOSTNAME//.*/}

tmpdir=${thisdir}/tmpdir.`date +%Y%m%d%H%M%S%n`
mkdir -p ${tmpdir} && cd ${tmpdir} || exit

dom=d01

test -f geo_em.${dom}.nc || cp /oceano/gmeteo/WORK/MDM.UC/WRF/domains/ESCENA-UCLM2/geo_em.${dom}.nc .
mkdir -p $POSTDIR
mkdir -p ${POST2DIR}

function pintnml(){
  idir=$1
  ifile=$2
  cat << EOF > namelist.pinterp
&io
  path_to_input            = '${idir}/',
  input_name               = '${ifile}',
  path_to_output           = '${tmpdir}/',
  fields                   = 'GHT,TT,RH,MSLP,U,V,Times',
  process                  = 'list',
  debug                    = .TRUE.,
  grid_filt                = 3,
  ntimes_filt              = 10,
  path_to_geofile          = '${tmpdir}',
  geofile                  = 'geo_em.d01.nc'
/

&interp_in
  interp_levels            = 1000., 925., 850., 700., 600., 500., 400., 300., 100.,
  p_top                    = 50.,
  extrapolate              = 1,
  interp_method            = 1,
  unstagger_grid           = .TRUE., 
/
EOF
}
pint="/oceano/gmeteo/WORK/chus/wrf4g/util/postprocess/Fortran/p_interp"

function get_3h_data(){
  for year in $(seq 1989 2009); do
    for month in $(seq 1 12); do
      pmon=$(printf "%02d" ${month})
      outfile="${POSTDIR}/${expname}_3H_${year}${pmon}_3dvars.nc"
      if test -f ${outfile}; then
        echo "I won't overwrite ${outfile}."
        continue
      fi
      rm -f yearlist.txt
      cat ${scriptdir}/filelists/ESCENA_ERAI_files.${year} \
        | grep "d01_${year}${pmon}" \
        | while read f; do
        indir=$(dirname $f)
        infile=$(basename $f)
        pintnml ${indir} ${infile}
        ${pint} >& ${tmpdir}/${infile}_PLEV.log
        echo ${tmpdir}/${infile}_PLEV >> yearlist.txt
      done
      python ${WRFNCXJPY} \
        --from-file yearlist.txt \
        -o ${POSTDIR}/${expname}_3H_${year}${pmon}_3dvars.nc \
        -v UER,VER,RH,GHT,TEMP,MSLP \
        -r "1950-01-01_00:00:00" \
        -a ${WRFNCXJATTR} \
        -t ${WRFNCXJTBL} \
        -g geo_em.${dom}.nc \
        -p \
        >& ${expname}_3H_${year}_3dvars.log
      rm yearlist.txt
    done
  done
}

function procesa(){
  var=$1
  thelevels="$2"
  thetimes="$3"   # e.g: 12H_00:00,12:00
  theperiods="$4" # e.g: 1989-1993,1994-1997
  read freqname thetimes <<< ${thetimes/_/ }
  test "${thelevels}" != "SFC" && splitlevel="splitlevel" || splitlevel=""
  test "${thetimes}" != "All" && seltime="-seltime,${thetimes}" || seltime=""
  echo "Processing $var"
  for per in ${theperiods//,/ }; do
    echo -n "  Period $per:"
    for yr in $(seq ${per/-/ }); do
      echo -n " $yr"
      for mon in $(seq 1 12); do
        pmon=$(printf "%02d" ${mon})
        cdo -s ${splitlevel} ${seltime} \
          -selvar,${var} ${POSTDIR}/${expname}_3H_${yr}${pmon}_3dvars.nc \
          procesa_${yr}${pmon}_${var}
      done # mon
    done # yr
    echo
    if test "${thelevels}" = "SFC"; then
      echo "  Processing only the surface value"
      cdo -s cat procesa_??????_${var} \
        ${POST2DIR}/${expname}_${freqname}_${per}_${var}.nc
      rm -f procesa_??????_${var}
    else
      echo -n "  Levels:"
      for level in ${thelevels//,/ }; do
        echo -n " $level"
        plevel=$(printf "%04d00" ${level})
        cdo -s cat procesa_??????_${var}${plevel}.nc \
          ${POST2DIR}/${expname}_${freqname}_${per}_${var}${level}.nc
        rm -f procesa_??????_${var}${plevel}.nc
      done # level
      echo
    fi
  done # per
}

#
#   Main
#
#get_3h_data # Takes >48h !
procesa ta  1000,850,700,500 24H_00:00       1989-2008
procesa ua  850,700,500      12H_00:00,12:00 1989-2008
procesa va  850,700,500      12H_00:00,12:00 1989-2008
procesa hur 1000,925,850,700 \
  6H_00:00,06:00,12:00,18:00 \
  1989-1993,1994-1998,1999-2003,2004-2008
procesa zg  1000,925,850,700,500,600,500,400,300,100 \
  6H_00:00,06:00,12:00,18:00 \
  1989-1993,1994-1998,1999-2003,2004-2008
procesa psl SFC \
  3H_All \
  1989-1993,1994-1998,1999-2003,2004-2008
