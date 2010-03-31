#! /bin/bash
scriptdir=$( (cd `dirname $0` && echo $PWD) )
thisdir=$(pwd)
source /software/ScientificLinux/4.6/etc/bashrc
source ${scriptdir}/dirs
#
#  Get a private space to run
#
tmpdir=${thisdir}/tmpdir.`date +%Y%m%d%H%M%S%n`
mkdir -p ${tmpdir} && cd ${tmpdir} || exit

cp /oceano/gmeteo/DATA/WRF/domains/CORDEX_Africa50_SMG1/geo_em.d01.nc .

mkdir -p $POSTDIR
mkdir -p $POST2DIR

wxajcmd="python /oceano/gmeteo/WORK/chus/wrf4g/util/postprocess/wrfnc_extract_and_join.py -a /oceano/gmeteo/WORK/chus/wrf4g/util/postprocess/wrfnc_extract_and_join.gattr_CORDEX -t /oceano/gmeteo/WORK/chus/wrf4g/util/postprocess/wrfnc_extract_and_join.table -g ${tmpdir}/geo_em.d01.nc -r 1950-01-01_00:00:00"
tag=$(date +'%Y%m%d%H%M%S')
p_interp="/oceano/gmeteo/WORK/chus/wrf4g/util/postprocess/Fortran/p_interp"

function pintnml(){
  idir=$1
  ifile=$2
  cat << EOF > namelist.pinterp
&io
  path_to_input            = '${idir}/',
  input_name               = '${ifile}',
  path_to_output           = '${tmpdir}/',
  fields                   = 'GHT,TT,RH,MSLP,U,V,U10,V10,RAINTOT,T2,SWDOWN,GLW,Times',
  process                  = 'list',
  debug                    = .TRUE.,
  grid_filt                = 3,
  ntimes_filt              = 10,
  path_to_geofile          = '${tmpdir}',
  geofile                  = 'geo_em.d01.nc'
/

&interp_in
  interp_levels            = 925., 850., 700., 500.,200.,
  p_top                    = 50.,
  extrapolate              = 1,
  interp_method            = 1,
  unstagger_grid           = .TRUE., 
/
EOF
}

function get_yearmons(){
  idir=$1
  find ${idir} -name wrfout_d01_\*.nc \
    | sed -e 's/^.*wrfout_d01_\(.*\)..T......Z.nc$/\1/' \
    | sort -n | uniq
}

function get_filelist_yearmon() {
  exppath=$1
  yearmon=$2
  find ${exppath} -name wrfout_d01_${yearmon}\*.nc | sort
}

function simulation_path(){
  sname=$1
  cat ${scriptdir}/simulation_map.txt \
    | awk '/^\ *EXPBASE/{expbase=$2} $1=="'${sname}'" {printf "%s/%s\n",expbase,$2}'
}

function filelist_is_short(){
  flfile=$1
  yearmon=$2
  s1=$(date --utc '+%s' -d "${yearmon:0:4}-${yearmon:4:2}-01 1 month")
  s2=$(date --utc '+%s' -d "${yearmon:0:4}-${yearmon:4:2}-01")
  daysinmonth=$(python -c "print ($s1-$s2)/60/60/24")
  test "${daysinmonth}" -ne $(wc -l ${flfile})
}

function get_3h_data(){
  expname=$1
  exppath=$(simulation_path ${expname})
  for yearmon in $(get_yearmons ${exppath}); do
      outfile="${POSTDIR}/${expname}_3H_${yearmon}_3dvars.nc"
      if test -f ${outfile}; then
        echo "I won't overwrite ${outfile}."
        continue
      fi
      rm -f filelist.txt
      get_filelist_yearmon ${exppath} ${yearmon} \
        | while read f; do
        if ! ncdump -h ${f} | grep -q num_metgrid_levels ; then
          echo "$f was not filtered at running time"
          echo "Filtering now!"
          indir=$(dirname $f)
          infile=$(basename $f)
          pintnml ${indir} ${infile}
          ${p_interp} >& ${tmpdir}/${infile}_PLEV.log
          echo ${tmpdir}/${infile}_PLEV >> filelist.txt
        else
          echo ${f} >> filelist.txt
        fi
      done
      if filelist_is_short filelist.txt ${yearmon} ; then
        echo "Too few files for month ${yearmon:4:2}: $(wc -l filelist.txt)"
        continue
      fi
      ${wxajcmd} \
        --from-file filelist.txt \
        -o ${POSTDIR}/${expname}_3H_${yearmon}_3dvars.nc \
        -v RH,GHT,TEMP,MSLP,T2,RAIN,U10ER,V10ER,SWDOWN,GLW \
        -p \
        >& ${expname}_3H_${yearmon}_3dvars.log
      rm filelist.txt
  done
}

function procesa(){
  expname=$1
  var=$2
  thelevels="$3"
  thetimes="$4"   # e.g: 12H_00:00,12:00
  theperiods="$5" # e.g: 1989-1993,1994-1997
  read freqname thetimes <<< ${thetimes/_/ }
  test "${thelevels}" != "SFC" && splitlevel="splitlevel" || splitlevel=""
  test "${thetimes}" != "All" && seltime="-seltime,${thetimes}" || seltime=""
  case ${freqname} in
    DM) ovar=$var      ; freqname=DM ; seltime="-settime,${thetimes} -daymean" ;;
    DX) ovar=${var}max ; freqname=DM ; seltime="-settime,${thetimes} -chname,${var},${ovar} -daymax" ;;
    DN) ovar=${var}min ; freqname=DM ; seltime="-settime,${thetimes} -chname,${var},${ovar} -daymin" ;;
    DA) ovar=$var      ; freqname=DM ; seltime="-settime,${thetimes} -daysum" ;;
     *) ovar=$var
       ;;
  esac
  echo "Processing $ovar"
  for per in ${theperiods//,/ }; do
    rm -f procesa_??????_*
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
      ofile="${POST2DIR}/${expname}_${freqname}_${per}_${ovar}.nc"
      rm -f ${ofile}
      cdo -s cat procesa_??????_${var} ${ofile}
    else
      echo -n "  Levels:"
      for level in ${thelevels//,/ }; do
        echo -n " $level"
        plevel=$(printf "%04d00" ${level})
        ofile="${POST2DIR}/${expname}_${freqname}_${per}_${ovar}${level}.nc"
        rm -f ${ofile}
        cdo -s cat procesa_??????_${var}${plevel}.nc ${ofile}
      done # level
      echo
    fi
    rm -f procesa_??????_*
  done # per
}

exp=SEN2
for sim in CUBM CUKF BLMY MPW6 RARR LSRU BLPX # CTRL
do
  get_3h_data CORDEX_UC_WRF_${exp}${sim}
  procesa CORDEX_UC_WRF_${exp}${sim} pr  SFC DA_00:00 1998-1998
  procesa CORDEX_UC_WRF_${exp}${sim} uas SFC DM_00:00 1998-1998
  procesa CORDEX_UC_WRF_${exp}${sim} vas SFC DM_00:00 1998-1998
  procesa CORDEX_UC_WRF_${exp}${sim} zg  850,500,200 DM_00:00 1998-1998
  procesa CORDEX_UC_WRF_${exp}${sim} hur 850,500,200 DM_00:00 1998-1998
  procesa CORDEX_UC_WRF_${exp}${sim} ta  850,500,200 DM_00:00 1998-1998
  procesa CORDEX_UC_WRF_${exp}${sim} tas SFC DM_00:00 1998-1998
  procesa CORDEX_UC_WRF_${exp}${sim} tas SFC DX_00:00 1998-1998
  procesa CORDEX_UC_WRF_${exp}${sim} tas SFC DN_00:00 1998-1998
done
