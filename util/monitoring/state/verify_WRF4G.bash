#!/bin/bash
if test $1 = '-h'
then
echo "**************************"
echo "***   Shell to verify  ***"
echo "***  WRF4G simulations ***"
echo "**************************"
echo "verify_WRF4G.bash"
echo "   Follows '/home/fital/fital/estudios/www/experiments_WRF4G.inf'"
echo "   Hindcast: H [header] [folder] [DATEi] [DATEf] [SIMlength] [SIMfrq] [size] [OUTfrq]"
echo "   Climatic: C [header] [folder] [DATEi] [DATEf] [CHUNKlength] [size] [OUTfrq]"
else
cd /home/fital/bats
file=$0
llfile=`(expr length $file)`
path=`(expr $llfile - 18)`
foresthome=`(expr substr $file 1 $path)`
foresthome=${foresthome}/..
foresthome=/home/fital
rootsh=/home/fital/estudios/www

c1=\"
numprojects=`wc -l ${rootsh}/experiments_WRF4G.inf | awk '{print $1}'`
iproj=1

rm ${rootsh}/WRF4Gstate.inf 
TODAY=`date +%Y-%m-%d_%H:%M:%S`
echo "##WRF4G experiments state on "${TODAY} > ${rootsh}/WRF4Gstate.inf
(
cat << EOF
<HTML>
<head>
<link rel="stylesheet" type="text/css" href="WRF4Gstate.css" />
<link rel="shortcut icon" href="wrf4g_sq_32x32.png" />
<title>WRF4G experiments state</title>
</head>
<body>
<div class="title">
<img src="wrf4gp.png"></img>
<b>WRF4G experiments state on ${TODAY}</b>
</div>
<div class="notdone">
<iframe src=empty.html name="notfinished" frameborder="0" width="100%" height="100%"></iframe>
</div>
<div class="experiments">
<table>
<tr><th>experiment</th><th>State</th><th>%</th><th>simulations</th></tr>
EOF
) > ${rootsh}/WRF4Gstate.html

while test ${iproj} -le $numprojects
do

  kind=`awk -f ${foresthome}/AWK/column.awk row=${iproj} col=1 ${rootsh}/experiments_WRF4G.inf`
  if test ${kind} = 'H'
  then
    header=`awk -f ${foresthome}/AWK/column.awk row=${iproj} col=2 ${rootsh}/experiments_WRF4G.inf`
    folder=`awk -f ${foresthome}/AWK/column.awk row=${iproj} col=3 ${rootsh}/experiments_WRF4G.inf`
    datei=`awk -f ${foresthome}/AWK/column.awk row=${iproj} col=4 ${rootsh}/experiments_WRF4G.inf`
    datef=`awk -f ${foresthome}/AWK/column.awk row=${iproj} col=5 ${rootsh}/experiments_WRF4G.inf`
    Length=`awk -f ${foresthome}/AWK/column.awk row=${iproj} col=6 ${rootsh}/experiments_WRF4G.inf`
    frq=`awk -f ${foresthome}/AWK/column.awk row=${iproj} col=7 ${rootsh}/experiments_WRF4G.inf`
    size=`awk -f ${foresthome}/AWK/column.awk row=${iproj} col=8 ${rootsh}/experiments_WRF4G.inf`
    output=`awk -f ${foresthome}/AWK/column.awk row=${iproj} col=9 ${rootsh}/experiments_WRF4G.inf`

    echo "HINDCAST Experiment: "${header}

    rm ${rootsh}/verifying-datesSIM_d1.inf >& /dev/null
    rm ${rootsh}/NOTverifyed-datesSIM_d1.inf >& /dev/null

    ${foresthome}/bats/verify_WRF4GsimH.bash ${folder} ${datei} ${datef} ${Length} ${frq} ${header} ${size} ${output} >& ${rootsh}/${header}_verify.log
    mv verifying-datesSIM_d1.inf ${rootsh}/${header}_verifying-datesSIM_d1.inf
    mv NOTverifyed-datesSIM_d1.inf ${rootsh}/${header}_NOTverifyed-datesSIM_d1.inf

    numfinished=`wc -l ${rootsh}/${header}_verifying-datesSIM_d1.inf | awk '{print $1}'`
    numNOTfinished=`wc -l ${rootsh}/${header}_NOTverifyed-datesSIM_d1.inf | awk '{print $1}'`
  else
    header=`awk -f ${foresthome}/AWK/column.awk row=${iproj} col=2 ${rootsh}/experiments_WRF4G.inf`
    folder=`awk -f ${foresthome}/AWK/column.awk row=${iproj} col=3 ${rootsh}/experiments_WRF4G.inf`
    datei=`awk -f ${foresthome}/AWK/column.awk row=${iproj} col=4 ${rootsh}/experiments_WRF4G.inf`
    datef=`awk -f ${foresthome}/AWK/column.awk row=${iproj} col=5 ${rootsh}/experiments_WRF4G.inf`
    Length=`awk -f ${foresthome}/AWK/column.awk row=${iproj} col=6 ${rootsh}/experiments_WRF4G.inf`
    size=`awk -f ${foresthome}/AWK/column.awk row=${iproj} col=7 ${rootsh}/experiments_WRF4G.inf`
    output=`awk -f ${foresthome}/AWK/column.awk row=${iproj} col=8 ${rootsh}/experiments_WRF4G.inf`

    echo "CLIMATIC Experiment: "${header}

   rm ${rootsh}/verifying-datesSIM_d1.inf >& /dev/null
   rm ${rootsh}/NOTverifyed-datesSIM_d1.inf >& /dev/null

    ${foresthome}/bats/verify_WRF4GsimC.bash ${folder} ${datei} ${datef} ${Length} ${header} ${size} ${output} >& ${rootsh}/${header}_verify.log 
    mv verifying-datesSIM_d1.inf ${rootsh}/${header}_verifying-datesSIM_d1.inf
    mv NOTverifyed-datesSIM_d1.inf ${rootsh}/${header}_NOTverifyed-datesSIM_d1.inf

    numfinished=`wc -l ${rootsh}/${header}_verifying-datesSIM_d1.inf | awk '{print $1}'`
    numNOTfinished=`cat ${rootsh}/${header}_NOTverifyed-datesSIM_d1.inf | grep : | wc -l | awk '{print $1}'`
  fi
  numfinished=`expr $numfinished - 1`
  numNOTfinished=`expr $numNOTfinished - 1`
  numTOT=`expr $numfinished + $numNOTfinished`
  numfinished5=`expr $numfinished '*' 20`
  numfinished10=`expr $numfinished '*' 100`
  fraction=`expr $numfinished / $numTOT`
  fraction5=`expr $numfinished5 / $numTOT`
  fraction10=`expr $numfinished10 / $numTOT`
  echo ${header}" "${numfinished}" "${numNOTfinished} >> ${rootsh}/WRF4Gstate.inf

# HTML format
##
bar=`${foresthome}/bats/multiple_character.bash X $fraction5`
echo "<tr><td>"${header}"</td><td>"${bar}"</td><td>"${fraction10}"</td><td><a class="${c1}"link1"${c1}" href="${c1}${header}"_NOTverifyed.html"${c1}" target="${c1}"notfinished"${c1}">not done</a></td></tr>" >> ${rootsh}/WRF4Gstate.html
rm ${rootsh}/${header}_NOTverifyed.html
(
cat << EOF
<html>
<body>
EOF
) > ${rootsh}/${header}_NOTverifyed.html
tail -n $numNOTfinished ${rootsh}/${header}_NOTverifyed-datesSIM_d1.inf | awk '{print substr($2,1,10)"_"substr($4,1,10)"<br>"}' >> ${rootsh}/${header}_NOTverifyed.html
(
cat << EOF
</body>
</html>
EOF
) >> ${rootsh}/${header}_NOTverifyed.html
if test $numNOTfinished -eq 0
then
(
cat << EOF
<html>
<body>
SIMULATIONS FINISHED -- simulations finished
</body>
</html>
EOF
) > ${rootsh}/${header}_NOTverifyed.html
fi
#exit
iproj=`expr $iproj + 1`
### End of projects
done
size=`df -h | grep meteo4g | awk '{print $1}'`
ocupied=`df -h | grep meteo4g | awk '{print $2}'`
left=`df -h | grep meteo4g | awk '{print $3}'`
percen=`df -h | grep meteo4g | awk '{print $4}'`
sizeSCR=`ssh oceano df -h /oceano/gmeteo/SCRATCH | awk '{print $1}' | tail -n 1`
ocupiedSCR=`ssh oceano df -h /oceano/gmeteo/SCRATCH | awk '{print $2}' | tail -n 1`
leftSCR=`ssh oceano df -h /oceano/gmeteo/SCRATCH | awk '{print $3}' | tail -n 1`
percenSCR=`ssh oceano df -h /oceano/gmeteo/SCRATCH | awk '{print $4}' | tail -n 1`
cat ${rootsh}/WRF4Gstate.inf
(
cat << EOF
</table>
meteo4g statistics. space left: ${left} occupied percentage: ${percen}<br>
SCRATCH statistics. space left: ${leftSCR} occupied percentage: ${percenSCR}
</div>
</body>
</HTML>
EOF
) >> ${rootsh}/WRF4Gstate.html
fi
