region=$1
var=$2
hlsim=$3 # highlight simulation 
if [ ${#} -lt 2 ]; then
  echo "Usage: $(basename $0) <region> <var> [<highlight sim>]"
  exit
fi

source dirs
source plot_util.sh

period="1998-1998"
experiment="SEN2"

OUT="figs/mac_${region}_${var}.eps"

R="-R0.5/12.5/$(get_varrange $var)"
Roverlay="-R0/1/0/1"
J="-JX10c/8c"
B="-B1:month:/$(get_vardz $var):${var}($(get_varunits $var)):WeSn"
OK="-O -K"
xpos=0.05; xlen=0.05; xspace=0.02
ypos=0.95; dy=0.05

function legend(){
  text="$1"
  psxy $Roverlay $J -N ${line} $OK >> $OUT <<- EOF
	$(echo "$xpos" | bc)          $ypos
	$(echo "$xpos + $xlen" | bc)  $ypos
	EOF
  pstext $Roverlay $J -N $OK >> $OUT <<- EOF
	$(echo "$xpos + $xlen + $xspace" | bc)   $ypos 10 0 0 ML $text
	EOF
  ypos=$(echo "$ypos - $dy" | bc)
}

gmtset PAGE_ORIENTATION portrait
gmtset PLOT_DEGREE_FORMAT dddF
gmtset PAPER_MEDIA a4+
gmtset HEADER_FONT_SIZE 20p
gmtset LABEL_FONT_SIZE 20p

psxy $R $J /dev/null -K > $OUT
psbasemap $R $J $B $OK >> $OUT
ltnumber=0
#
#   envolvente
#
py_envolvente ${DIAGDIR}/CORDEX_UC_WRF_${experiment}*_MAC_${region}_${period}_${var}.txt \
  | psxy $R $J -L -G$(get_faintcolor CTRL) $OK >> $OUT
line="-W30,$(get_faintcolor CTRL)"
legend "WRF ENS"
let ltnumber++
#
#   CRU
#
fname="${DIAGDIR}/CRUTS30AFR_MAC_${region}_${period}_${var}.txt"
line="-W10,$(get_color CRU)"
cat $fname \
  | nl \
  | psxy $R $J ${line} $OK >> $OUT
legend "CRU"
let ltnumber++
#
#   TRMM
#
fname="${DIAGDIR}/TRMM_MAC_${region}_${period}_${var}.txt"
line="-W10,$(get_color TRMM)"
cat $fname \
  | nl \
  | psxy $R $J ${line} $OK >> $OUT
legend "TRMM"
let ltnumber++
#
#   WRF
#
for sim in CTRL ${hlsim} # CUBM CUKF BLMY MPW6 RARR LSRU BLPX
do
  fname="${DIAGDIR}/CORDEX_UC_WRF_${experiment}${sim}_MAC_${region}_${period}_${var}.txt"
  line="-W5,$(get_color ${sim})"
  cat $fname \
    | nl \
    | psxy $R $J ${line} $OK >> $OUT
  legend "$sim"
  let ltnumber++
done
#
#   Title
#
pstext $Roverlay $J -N $OK >> $OUT <<- EOF
	0.95 0.95 15 0 0 MR $(get_vardes $var) in ${region}
	EOF
psxy $R $J /dev/null -O >> $OUT

echo "$OUT done."

\rm -f .gmtdefaults4 .gmtcommands4
