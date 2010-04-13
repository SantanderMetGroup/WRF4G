function months_in_season(){
  season=$1
  case ${season} in
    JFM) echo "1,2,3" ;;
    AMJ) echo "4,5,6" ;;
    JAS) echo "7,8,9" ;;
    OND) echo "10,11,12" ;;
  esac
}

function region_boundaries(){
  rname=$1
  case ${rname} in
       AM) echo "-10,5,29,36" ;;
     WA-N) echo "-10,10,7.5,15" ;;
     WA-S) echo "-10,10,5,7.5" ;;
    CA-NH) echo "10,25,0,10" ;;
    CA-SH) echo "10,25,-10,0" ;;
    SA-WN) echo "14,20,-28,-22" ;;
    SA-WS) echo "14,20,-35,-28" ;;
     SA-E) echo "20,36,-35,-22" ;;
       EA) echo "30,40,-15,0" ;;
       EH) echo "34,40,7,15" ;;
  esac
}

function calc_seasclim(){
  label=$1; ifile=$2; ofile=$3
  cdo timmean -selmon,$(months_in_season ${label}) ${ifile} ${ofile}
}

function calc_areamac(){
  # Monthly annual cycle averaged over and area
  # defined on the CRU grid
  regionname=$1; ifile=$2; ofile=$3; shift 3; cdoopt="$*"
  cdo remapbil,CRU_mask.nc $ifile s1
  cdo outputf,"%g",1 -ymonmean -fldmean -sellonlatbox,$(region_boundaries $regionname) -mul -setctomiss,0 CRU_mask.nc $cdoopt s1 > ${ofile}
  rm -f s1
}

function calc_area3dc(){
  # 3-hourly daily cycle averaged over and area
  # defined on the CRU grid
  regionname=$1; sea=$2; ifile=$3; ofile=$4; shift 3; cdoopt="$*"
  cdo selmon,$(months_in_season ${sea}) $ifile s1
  cdo remapbil,CRU_mask.nc s1 s2
  cdo fldmean -sellonlatbox,$(region_boundaries $regionname) -mul -setctomiss,0 CRU_mask.nc s2 s3
  cdo splithour s3 s4_
  for f in s4_??.nc; do
    cdo timmean $f ${f/.nc/mean.nc}
  done
  cdo cat s4_??mean.nc s5
  cdo outputf,"%g",1 s5 > ${ofile}
  rm -f s[1235] s4_*
}

function calc_latt(){
  lonrange=$1; ifile=$2; ofile=$3; shift 3; cdoopt="$*"
  cdo remapbil,CRU_mask.nc $ifile s1
  cdo runmean,3 -zonmean -sellonlatbox,${lonrange},-90,90 $cdoopt s1 ${ofile}
  rm -f s1
}

function calc_lattlandonly(){
  lonrange=$1; ifile=$2; ofile=$3; shift 3; cdoopt="$*"
  cdo remapbil,CRU_mask.nc $ifile s1
  cdo runmean,3 -zonmean -sellonlatbox,${lonrange},-90,90 -mul -setctomiss,0 CRU_mask.nc $cdoopt s1 ${ofile}
  rm -f s1
}
