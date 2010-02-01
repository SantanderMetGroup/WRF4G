#!/bin/bash 

statistics_yesnot () {
# Function to give some statistics from YES/NOT values 
# Variables:
#   numTOT: Total sum of YES+NOT
#   fraction: Percentage (in per 1) of YES/numTOT
#   fraction5: Percentage (in per 5) of YES/numTOT
#   fraction10: Percentage (in per 10) of YES/numTOT

# Output as:
#   arg1: $numTOT $fraction $fraction5 $fraction10 

  numYES=$1
  numNOT=$2
  numTOT=`expr ${numYES}  + ${numNOT} `
  numYES5=`expr ${numYES}  '*' 20`
  numYES10=`expr ${numYES} '*' 100`
  fraction=`expr ${numYES} / ${numTOT} `
  fraction5=`expr ${numYES5} / ${numTOT} `
  fraction10=`expr ${numYES10} / ${numTOT} `

  echo $numTOT" "$fraction" "$fraction5" "$fraction10
}
#######   #######   #######   #######   #######   #######   #######   
   #######   #######   #######   #######   #######   #######   #######

if test $1 = '-h'
then
  echo "****************************************"
  echo "***      Shell to serach 'causes'    ***"
  echo "*** of ending of 'WRF4G' simulations ***"
  echo "****************************************" 
  echo "WRF4F_causes.bash"
  echo "   Reading WRF4G simulation information from '/oceano/gmeteo/users/lluis/estudios/www/experiments_WRF4G.inf'"
  echo "   Hindcast: H [header] [folder] [DATEi] [DATEf] [SIMlength] [SIMfrq] [KIND]" 
  echo "   Climatic: C [header] [folder] [DATEi] [DATEf] [CHUNKlength]"

else
cd /oceano/gmeteo/users/lluis/bats 
file=$0
llfile=`expr length $file`
path=`expr $llfile - 18`
foresthome=`expr substr $file 1 $path`
foresthome=${foresthome}/..
rootsh='/oceano/gmeteo/users/lluis/estudios/www'

cd ${rootsh}

c1=\"
numprojects=`wc -l ${rootsh}/experiments_WRF4G.inf | awk '{print $1}'`
iproj=1

rm ${rootsh}/WRF4Gcauses.inf 
TODAY=`date +%Y-%m-%d_%H:%M:%S`
echo "##WRF4G experiments causes on "${TODAY} > ${rootsh}/WRF4Gcauses.inf
echo "## title[1] num_done[2] num_NOT_done[3] num_success[4] num_NOTscucess[5] num_NOTrsl[7]" >> ${rootsh}/WRF4Gcauses.inf
(
cat << EOF
<HTML>
<head>
<link rel="stylesheet" type="text/css" href="WRF4Gstate.css" />
<link rel="shortcut icon" href="wrf4g_sq_32x32.png" />
<title>causes of WRF4G experiments</title>
</head>
<body>
<div class="title">
<img src="wrf4gp.png"></img>
<b>WRF4G experiments' causes on ${TODAY}</b>
</div>
<div class="notsuccess">
<iframe src=empty.html name="notsuccess" frameborder="0" width="100%" height="100%"></iframe>
</div>
<div class="notdone">
<iframe src=empty.html name="notfinished" frameborder="0" width="100%" height="100%"></iframe>
</div>
<div class="errs">
<iframe src=empty.html name="rslerrors" frameborder="0" width="100%" height="100%"></iframe>
</div>
<div class="outs">
<iframe src=empty.html name="rslouts" frameborder="0" width="100%" height="100%"></iframe>
</div>
<div class="experiments">
<table>
<tr><th>experiment</th><th></th><th>% done</th><th></th><th>not done</th><th>not succ.</th><th> </th><th>not rsl</th><th></th></tr>
EOF
) > ${rootsh}/WRF4Gcauses.html

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
    kind=`awk -f ${foresthome}/AWK/column.awk row=${iproj} col=8 ${rootsh}/experiments_WRF4G.inf`

    echo "HINDCAST Experiment: "${header}

    rm ${rootsh}/success-datesSIM_d1.inf >& /dev/null
    rm ${rootsh}/NOTsuccess-datesSIM_d1.inf >& /dev/null

    runbash=1
    if test ${runbash} -eq 1
    then
      ${foresthome}/bats/causes_WRF4GsimH.bash ${folder} ${datei} ${datef} ${Length} ${frq} ${header} ${kind} >& ${rootsh}/${header}_causes.log
      mv verifying-datesSIM_d1.inf ${rootsh}/${header}_verifying-datesSIM_d1.inf
      mv NOTverifyed-datesSIM_d1.inf ${rootsh}/${header}_NOTverifyed-datesSIM_d1.inf
      mv success-datesSIM_d1.inf ${rootsh}/${header}_success-datesSIM_d1.inf
      mv NOTsuccess-datesSIM_d1.inf ${rootsh}/${header}_NOTsuccess-datesSIM_d1.inf
      mv NOTrsl-datesSIM_d1.inf ${rootsh}/${header}_NOTrsl-datesSIM_d1.inf
    fi

    numfinished=`wc -l ${rootsh}/${header}_verifying-datesSIM_d1.inf | awk '{print $1}'`
    numNOTfinished=`wc -l ${rootsh}/${header}_NOTverifyed-datesSIM_d1.inf | awk '{print $1}'`
    numsuccess=`wc -l ${rootsh}/${header}_success-datesSIM_d1.inf | awk '{print $1}'`
    numNOTsuccess=`wc -l ${rootsh}/${header}_NOTsuccess-datesSIM_d1.inf | awk '{print $1}'`
    numNOTrsl=`wc -l ${rootsh}/${header}_NOTrsl-datesSIM_d1.inf | awk '{print $1}'`
  else
    header=`awk -f ${foresthome}/AWK/column.awk row=${iproj} col=2 ${rootsh}/experiments_WRF4G.inf`
    folder=`awk -f ${foresthome}/AWK/column.awk row=${iproj} col=3 ${rootsh}/experiments_WRF4G.inf`
    datei=`awk -f ${foresthome}/AWK/column.awk row=${iproj} col=4 ${rootsh}/experiments_WRF4G.inf`
    datef=`awk -f ${foresthome}/AWK/column.awk row=${iproj} col=5 ${rootsh}/experiments_WRF4G.inf`
    Length=`awk -f ${foresthome}/AWK/column.awk row=${iproj} col=6 ${rootsh}/experiments_WRF4G.inf`

    echo "CLIMATIC Experiment: "${header}

   rm ${rootsh}/success-datesSIM_d1.inf >& /dev/null
   rm ${rootsh}/NOTsuccess-datesSIM_d1.inf >& /dev/null

    ${foresthome}/bats/causes_WRF4GsimC.bash ${folder} ${datei} ${datef} ${Length} ${header} >& ${rootsh}/${header}_causes.log 
    mv success-datesSIM_d1.inf ${rootsh}/${header}_success-datesSIM_d1.inf
    mv NOTsuccess-datesSIM_d1.inf ${rootsh}/${header}_NOTsuccess-datesSIM_d1.inf

    numsuccess=`wc -l ${rootsh}/${header}_success-datesSIM_d1.inf | awk '{print $1}'`
    numNOTsuccess=`cat ${rootsh}/${header}_NOTsuccess-datesSIM_d1.inf | grep : | wc -l | awk '{print $1}'`
  fi
# Finished statistics
##
  numfinished=`expr ${numfinished}  - 1`
  numNOTfinished=`expr ${numNOTfinished}  - 1`
  fraction5=`statistics_yesnot ${numfinished} ${numNOTfinished} | awk '{print $3}'`
  fraction10=`statistics_yesnot ${numfinished} ${numNOTfinished} | awk '{print $4}'`

# NOT success statistics
##
  numsuccess=`expr ${numsuccess}  - 1`
  numNOTsuccess=`expr ${numNOTsuccess}  - 1`
  fractionS10=`statistics_yesnot ${numsuccess} ${numNOTsuccess} | awk '{print $4}'`
  numNOTrsl=`expr ${numNOTrsl} - 1`
  numrsl=${numNOTsuccess}
  fractionR10=`statistics_yesnot ${numrsl} ${numNOTrsl} | awk '{print $4}'`

  echo ${header}" "${numfinished}" "${numNOTfinished}" "${numsuccess}" "${numNOTsuccess}" "${numNOTrsl} >> ${rootsh}/WRF4Gcauses.inf

# HTML format
##
  bar=`${foresthome}/bats/multiple_character.bash x ${fraction5}`
  echo "<tr><td>"${header}"</td><td>"${bar}"</td><td>"${fraction10}"</td><td><a class="${c1}"link1"${c1}" href="${c1}${header}"_NOTverifyed.html"${c1}" target="${c1}"notfinished"${c1}">not done</a></td><td>"${numNOTfinished}"</td><td>${numrsl}</td><td><a class="${c1}"link1"${c1}" href="${c1}${header}"_NOTsuccess.html"${c1}" target="${c1}"notsuccess"${c1}">not suc.</a></td><td>"${numNOTrsl}"</td><td><a class="${c1}"link1"${c1}" href="${c1}${header}"_NOTrsl.html"${c1}" target="${c1}"notsuccess"${c1}">not rsl</a></td></tr>" >> ${rootsh}/WRF4Gcauses.html
# List of NOT success
##
  rm ${rootsh}/${header}_NOTsuccess.html
(
cat << EOF
<html>
<head>
<link rel="stylesheet" type="text/css" href="WRF4Gstate.css" />
</head>
<body>
<div class="subtitle">
'Not success' simulations
</div>
EOF
) > ${rootsh}/${header}_NOTsuccess.html
  tail -n $numNOTsuccess ${rootsh}/${header}_NOTsuccess-datesSIM_d1.inf | awk '{print $1substr($4,1,10)"_"substr($6,1,10)" <a class=\"link2\" href=NOTsuccessERROR-"substr($4,1,10)"_"substr($6,1,10)"SIM_d1.inf.html target=\"rslerrors\">error.rsl</a> <a class=\"link2\" href=NOTsuccessOUT-"$1substr($4,1,10)"_"substr($6,1,10)"SIM_d1.inf.html target=\"rslouts\">out.rsl</a><br>"}' >> ${rootsh}/${header}_NOTsuccess.html
(
cat << EOF
</body>
</html>
EOF
) >> ${rootsh}/${header}_NOTsuccess.html
  if test $numNOTsuccess -eq 0
  then
(
cat << EOF
<html>
<head>
<link rel="stylesheet" type="text/css" href="WRF4Gstate.css" />
</head>
<body>
<div class="subtitle">
EMPTY -- empty
</div>
</body>
</html>
EOF
) > ${rootsh}/${header}_NOTsuccess.html
  fi

# List of NOT rsl 
##
  rm ${rootsh}/${header}_NOTrsl.html
(
cat << EOF
<html>
<head>
<link rel="stylesheet" type="text/css" href="WRF4Gstate.css" />
</head>
<body>
<div class="subtitle">
'NOT rsl' simulations
</div>
EOF
) > ${rootsh}/${header}_NOTrsl.html
  tail -n $numNOTrsl ${rootsh}/${header}_NOTrsl-datesSIM_d1.inf | awk '{print $2substr($3,1,10)"_"substr($5,1,10)"<br>"}' >> ${rootsh}/${header}_NOTrsl.html
(
cat << EOF
</body>
</html>
EOF
) >> ${rootsh}/${header}_NOTrsl.html
  if test $numNOTrsl -eq 0
  then
(
cat << EOF
<html>
<head>
<link rel="stylesheet" type="text/css" href="WRF4Gstate.css" />
</head>
<body>
<div class="subtitle">
EMPTY -- empty
</div>
</body>
</html>
EOF
) > ${rootsh}/${header}_NOTrsl.html
  fi

# List of NOT done 
##
  rm ${rootsh}/${header}_NOTverifyed.html
(
cat << EOF
<html>
<head>
<link rel="stylesheet" type="text/css" href="WRF4Gstate.css" />
</head>
<body>
<div class="subtitle">
'NOT done' simulations
</div>
EOF
) > ${rootsh}/${header}_NOTverifyed.html
  tail -n $numNOTfinished ${rootsh}/${header}_NOTverifyed-datesSIM_d1.inf | awk '{print $2substr($3,1,10)"_"substr($5,1,10)"<br>"}' >> ${rootsh}/${header}_NOTverifyed.html
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
<head>
<link rel="stylesheet" type="text/css" href="WRF4Gstate.css" />
</head>
<body>
<div class="subtitle">
FINISHED -- finished
</div>
</body>
</html>
EOF
) > ${rootsh}/${header}_NOTverifyed.html
  fi
#  exit
  iproj=`expr $iproj + 1`
### End of projects
done
size=`df -h | grep meteo4g | awk '{print $1}'`
ocupied=`df -h | grep meteo4g | awk '{print $2}'`
left=`df -h | grep meteo4g | awk '{print $3}'`
percen=`df -h | grep meteo4g | awk '{print $4}'`
cat ${rootsh}/WRF4Gcauses.inf
(
cat << EOF
</table>
meteo4g statistics. space left: ${left} occupied percentage: ${percen}
</div>
</body>
</HTML>
EOF
) >> ${rootsh}/WRF4Gcauses.html

fi
