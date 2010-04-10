#! /bin/bash

function get_color(){
  case "$1" in
    CRU) echo "27/158/119" ;;
   TRMM) echo "217/95/2" ;;
   CTRL) echo "117/112/179" ;;
      *) echo "0/0/0" ;;
  esac
}

function get_lightcolor(){
  case "$1" in
    CRU) echo "102/194/165" ;;
   TRMM) echo "252/141/98" ;;
   CTRL) echo "141/160/203" ;;
      *) echo "150/150/150" ;;
  esac
}

function get_faintcolor(){
  case "$1" in
    CRU) echo "179/226/205" ;;
   TRMM) echo "253/205/172" ;;
   CTRL) echo "203/213/232" ;;
      *) echo "210/210/210" ;;
  esac
}


colors=(     "27/158/119"  "217/95/2"   "117/112/179" "231/41/138"  "102/166/30" "230/171/2"  "166/118/29")
lightcolors=("102/194/165" "252/141/98" "141/160/203" "231/138/195" "166/216/84" "255/217/47" "229/196/148")
faintcolors=("179/226/205" "253/205/172" "203/213/232" "244/202/228" "230/245/201" "255/242/174" "241/226/204")

for i in $(seq 0 6)
do
  colorsrgb[$i]="$(echo ${colors[$i]} | awk -F/ '{print $1/255, $2/255, $3/255}')"
done

function py_envolvente(){
paste $* > s1
python << EOF
import string
maxs=[]
mins=[]
for line in open("s1"):
  spline = map(float, line.split())
  maxs.append(max(spline))
  mins.append(min(spline))
for i in range(1,len(maxs)+1):
  print i, maxs[i-1]
for i in range(len(maxs),0,-1):
  print i, mins[i-1]
EOF
rm -f s1
}

function dump_region_border(){
  rname=$1
  read lo1 lo2 la1 la2 <<< $(region_boundaries ${rname} | tr ',' ' ')
  echo "${lo1} ${la1}"
  echo "${lo1} ${la2}"
  echo "${lo2} ${la2}"
  echo "${lo2} ${la1}"
  echo "${lo1} ${la1}"
}

function dump_region_center(){
  rname=$1
  read lo1 lo2 la1 la2 <<< $(region_boundaries ${rname} | tr ',' ' ')
  python -c "print 0.5*(${lo1}+${lo2}), 0.5*(${la1}+${la2})"
}

function get_vardes(){
  var=$1
  case ${var} in
    pr) echo "Precipitation" ;;
    tas) echo "2m Temperature";;
    tasmax) echo "2m maximum temperature";;
    tasmin) echo "2m minimum temperature";;
  esac
}

function get_varunits(){
  var=$1
  case ${var} in
    pr) echo "mm/day" ;;
    tas*) echo "degC";;
  esac
}

function get_varrange(){
  var=$1
  case ${var} in
    pr) echo "0/9" ;;
    tas) echo "5/40";;
 tasmax) echo "10/45";;
 tasmin) echo "0/30";;
  esac
}

function get_vardz(){
  var=$1
  case ${var} in
    pr) echo "1" ;;
    tas*) echo "5";;
  esac
}

function plot_climatology(){
  var=$1
  ifile=$2
  plotter="plot_africa.gmt.sh"
  varopt="var $var"
  case $var in
      pr) plotopt="" ;;
    tas*) plotopt="has_height_dim addc -273.15 cpt cpts/tas.cpt" ;;
     ua*)
       plotopt="has_height_dim zmin 0 zmax 12 dz 1 cpt gray cptreverse"
       plotter="plot_africa_wind.gmt.sh" 
       ifile2=${ifile/ua/va} 
       varopt="u $var v ${var/u/v}"
       ;;
       *) echo "plot_climatology: uknownk var: $var"; exit ;;
  esac
  bash ${plotter} ${ifile} ${ifile2} \
    ${varopt} \
    is_curvilinear rec 0 \
    ${plotopt}
}

function plot_bias(){
  var=$1
  ifile=$2
  plotter="plot_africa.gmt.sh"
  varopt="var $var"
  case $var in
      pr) plotopt="cpt cpts/bias_pr.cpt" ;;
    tas*) plotopt="has_height_dim cpt cpts/bias_tas.cpt" ;;
     ua*)
       plotopt="has_height_dim zmin 0 zmax 2 dz 0.2 cpt gray cptreverse"
       plotter="plot_africa_wind.gmt.sh" 
       ifile2=${ifile/ua/va} 
       varopt="u $var v ${var/u/v}"
       ;;
      *) echo "plot_climatology: uknownk var: $var"; exit ;;
  esac
  bash ${plotter} ${ifile} ${ifile2} \
    var ${var} \
    is_curvilinear rec 0 \
    ${plotopt}
}

function montafigs_sclim() {
  var=$1
  sea=$2
  cat << EOF > monta_figs.cfg
monta7up.fig ${FIGSDIR}/sclim${sea}_${var}.fig png
  pr
    ${var}
  Precipitation
    $(get_vardes $var)
  mm/day
    $(get_varunits $var)
  JFM
    ${sea}
EOF
}

function montafigs_sdiff() {
  var=$1
  sea=$2
  cat << EOF > monta_figs.cfg
monta7up.fig ${FIGSDIR}/sdiff${sea}_${var}.fig png
  pr
    ${var}
  Precipitation
    $(get_vardes $var)
  mm/day
    $(get_varunits $var)
  Climatology
    Differences
  JFM
    ${sea}
  clim
    diff
  CTRL_sdiff
    CTRL_sclim
  CTRL_sclim${sea}_${var}.scale
    RARR_sdiff${sea}_${var}.scale
EOF
}

function montafigs_mac() {
  var=$1
  cat << EOF > monta_figs.cfg
montaareaavg.fig ${FIGSDIR}/mac_${var}.fig png
  pr
    ${var}
  Precipitation
    $(get_vardes $var)
EOF
}

