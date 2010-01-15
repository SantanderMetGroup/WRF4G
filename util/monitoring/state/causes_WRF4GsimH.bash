#!/bin/bash 
ascii_raw_html () {
# Function to create a raw 'html' file (with <br> at the end of each line) of a given ascii file
  foresth=$1
  file=$2
  numlines=`wc -l ${file} | awk '{print $1}'`
  echo "<br>" > br.inf
  echo " " > $2.html
  ilin=1
  while test ${ilin} -le ${numlines}
  do
#      line=$(head -n ${ilin} $2 | tail -n 1)
#      cat <<- EOF >> $2.html
#      ${line}<br>
#      EOF
       head -n ${ilin} $2 | tail -n 1 > line.inf
       cat line.inf br.inf >> $2.html 
    ilin=`expr $ilin + 1`
  ### end number of lines
  done
  echo "</body>" >> $2.html
  echo "</HTML>" >> $2.html
  rm line.inf
}

tar_path () {
# Function to give a path of a particular file inside a tar.gz compressed file
  foresth=$1
  targzfile=$2
  compfile=$3
  tar tvfz ${targzfile} | grep ${compfile} > linetar.inf
  ncol=`cat linetar.inf | wc -w | awk '{print $1}'`
  pathfile=`awk -f ${foresth}/AWK/column.awk row=1 col=${ncol} linetar.inf`
  Lpathfile=`expr length ${pathfile}`
  Lpath=`expr $Lpathfile - 15`
  path=`expr substr ${pathfile} 1 ${Lpath}`
  echo ${path}
}
#######   #######   #######   #######   #######   #######   #######   
   #######   #######   #######   #######   #######   #######   #######   
if test $1 = '-h'
then
echo "****************************"
echo "***     Causes a set of  ***" 
echo "*** HINDACST simulations ***"
echo "****************************"
echo "causes_WRF4GsimH.bash 'FOLDER' (simulation folder) \
'DATEi'([YYYY][MM][DD][HH][MI][SS]) 'DATEf'([YYYY][MM][DD][HH][MI][SS]) \
'FRQ'(frequency of simulations in hour) 'LNG'(length of simulations in hour) 'HEAD'(header of simulations) 'KIND'(S: serie, P:paralel)" 
else
file=$0
llfile=`expr length $file`
path=`expr $llfile - 22`
foresthome=`expr substr $file 1 $path`
foresthome=${foresthome}/..
rootsh=`pwd`

c1="'"
c2="$"
c3='`'
c4=\"

idom=1

# Domain dates
##
idate=$2
fdate=$3

interval=`expr $4 '*' 3600`
SIMinterval=`expr $5 '*' 3600`

# Simulation dates
##

echo "idate: "$idate" fdate: "$fdate
iyyyy1=`expr substr $idate 1 4`
imm1=`expr substr $idate 5 2`
idd1=`expr substr $idate 7 2`
ihh1=`expr substr $idate 9 2`
imi1=`expr substr $idate 11 2`
iss1=`expr substr $idate 13 2`

difdates=`${foresthome}/bats/seconds_between-dates.bash ${idate} ${fdate}`
numsteps=`expr $difdates / $interval`

echo "Initial date: "$iyyyy1"/"$imm1"/"$idd1" "$ihh1":"$imi1":"$iss1
echo "WRF simulations every: "$interval" seconds "$numsteps" number of simulations of "$SIMinterval" seconds long"

istep=1
hours=`expr $ihh1 '*' 3600`
minutes=`expr $imi1 '*' 60`
seconds=`expr $hours + $minutes`

stepdate=`date +%c -u -d "$iyyyy1$imm1$idd1 $seconds seconds"`
rm ${rootsh}/verifying-datesSIM_d${idom}.inf > /dev/null
rm ${rootsh}/NOTverifyed-datesSIM_d${idom}.inf > /dev/null
rm ${rootsh}/success-datesSIM_d${idom}.inf > /dev/null
rm ${rootsh}/NOTsuccess-datesSIM_d${idom}.inf > /dev/null
rm ${rootsh}/NOTrsl-datesSIM_d${idom}.inf > /dev/null

echo "Folder: "$1 > ${rootsh}/verifying-datesSIM_d${idom}.inf
echo "Folder: "$1 > ${rootsh}/NOTverifyed-datesSIM_d${idom}.inf
echo "Folder: "$1 > ${rootsh}/success-datesSIM_d${idom}.inf
echo "Folder: "$1 > ${rootsh}/NOTsuccess-datesSIM_d${idom}.inf
echo "Folder: "$1 > ${rootsh}/NOTrsl-datesSIM_d${idom}.inf
echo "#0@ "$idate

echo "sim dom "$idom" t-step: 0 "$idate

cat << EOF > ${rootsh}/errortitle
<HTML>  
<head>
<link rel="stylesheet" type="text/css" href="WRF4Gstate.css" />
</head>
<body>
<div class="subtitle">
rsl.error
</div>
EOF

cat << EOF > ${rootsh}/outtitle
<div class="subtitle">
rsl.out
</div>
EOF

rm -rf ${rootsh}/WRF4Gcauses
mkdir ${rootsh}/WRF4Gcauses
while test $istep -le $numsteps
do
  cd ${rootsh}/WRF4Gcauses

  stepdate=`date +%c -u -d"$stepdate $interval seconds"`
  stepdateF=`date +%c -u -d"$stepdate $SIMinterval seconds"`
  stepdate0=`date +%Y%m%d%H%M%S -u -d"$stepdate"`
  stepdate0F=`date +%Y%m%d%H%M%S -u -d"$stepdateF"`
  SIMi=`date +%Y%m%d%H -u -d"$stepdate"`
  SIMf=`date +%Y%m%d%H -u -d"$stepdateF"`

  echo "#"$istep"@ "$stepdate0" --> "${stepdate0F} 

  if test -d $1/$6${SIMi}_${SIMf}
  then
    if test -f $1/$6${SIMi}_${SIMf}/output/current_date.txt
    then
      currentDATE=`cat $1/$6${SIMi}_${SIMf}/output/current_date.txt`
      if test ${currentDATE} -eq ${SIMf}'0000'
      then
        echo "#"$istep"@ "$stepdate0" --> "${stepdate0F} >> ${rootsh}/verifying-datesSIM_d${idom}.inf 
        echo "#"$istep"@ "$stepdate0" --> "${stepdate0F} >> ${rootsh}/success-datesSIM_d${idom}.inf 
      else
# NOT finished simulation
##
        if ! test -f $1/$6${SIMi}_${SIMf}/log.tar.gz
        then
          echo "#"$istep"@ "$stepdate0" --> "${stepdate0F} >> ${rootsh}/NOTverifyed-datesSIM_d${idom}.inf
        else
          if test $7 == 'P'
          then
            numRSL=`tar tvfz $1/$6${SIMi}_${SIMf}/log.tar.gz | grep rsl_wrf | wc -l` 
          else
            rslpath=`tar_path ${foresthome} $1/$6${SIMi}_${SIMf}/log.tar.gz wrf_${SIMi}.out`
            tar xvfz $1/$6${SIMi}_${SIMf}/log.tar.gz ${rslpath} 
            numRSL=`wc -l ${rslpath}/wrf_${SIMi}.out | awk '{print $1}'`
            if test ${numRSL} -lt 5
            then
              numRSL=0
            fi
          fi
          if test ${numRSL} -gt 1 
          then
            echo $6" #"$istep"@ "$stepdate0" --> "${stepdate0F} >> ${rootsh}/NOTsuccess-datesSIM_d${idom}.inf
            echo "NOT SUCCESS -- not success"
            echo "Let see which errors give 'rsl.[error/out].[nnnn]' files..."
            rm -rf ${rootsh}/WRF4Gcauses/* 
            rm ${rootsh}/NOTsuccessERROR-$6${SIMi}_${SIMf}SIM_d${idom}.inf > /dev/null
            rm ${rootsh}/NOTsuccessOUT-$6${SIMi}_${SIMf}SIM_d${idom}.inf > /dev/null
            if test $7 == 'P'
            then
              rslpath=`tar_path ${foresthome} $1/$6${SIMi}_${SIMf}/log.tar.gz rsl.error.0000` 
              tar xvfz $1/$6${SIMi}_${SIMf}/log.tar.gz ${rslpath}
              tail -n 2 ${rslpath}/rsl.error.* > ${rootsh}/NOTsuccessERROR-$6${SIMi}_${SIMf}SIM_d${idom}.inf
              tail -n 2 ${rslpath}/rsl.out.* > ${rootsh}/NOTsuccessOUT-$6${SIMi}_${SIMf}SIM_d${idom}.inf
            else
              tail -n 2 ${rslpath}/wrf_${SIMi}.out > ${rootsh}/NOTsuccessERROR-$6${SIMi}_${SIMf}SIM_d${idom}.inf
              echo "serial SIMULATION -- SERIAL simulation" > ${rootsh}/NOTsuccessOUT-$6${SIMi}_${SIMf}SIM_d${idom}.inf
            fi
            ascii_raw_html ${foresthome} ${rootsh}/NOTsuccessERROR-$6${SIMi}_${SIMf}SIM_d${idom}.inf
            ascii_raw_html ${foresthome} ${rootsh}/NOTsuccessOUT-$6${SIMi}_${SIMf}SIM_d${idom}.inf
            cat ${rootsh}/errortitle ${rootsh}/NOTsuccessERROR-$6${SIMi}_${SIMf}SIM_d${idom}.inf.html > ${rootsh}/stepA
            mv ${rootsh}/stepA ${rootsh}/NOTsuccessERROR-$6${SIMi}_${SIMf}SIM_d${idom}.inf.html
            cat ${rootsh}/outtitle ${rootsh}/NOTsuccessOUT-$6${SIMi}_${SIMf}SIM_d${idom}.inf.html > ${rootsh}/stepA
            mv ${rootsh}/stepA ${rootsh}/NOTsuccessOUT-$6${SIMi}_${SIMf}SIM_d${idom}.inf.html
#            exit
          else
            echo "#"$istep"@ "$stepdate0" --> "${stepdate0F} >> ${rootsh}/NOTrsl-datesSIM_d${idom}.inf
###   End of RSLverification
          fi
        fi
### End of currentdate verification
      fi
    else
      echo "#"$istep"@ "$stepdate0" --> "${stepdate0F} >> ${rootsh}/NOTverifyed-datesSIM_d${idom}.inf
    echo $6${SIMi}_${SIMf}" 'current_date.txt' does not exist !!!!"
### End of existence of 'current_date.txt'
    fi 
  else 
    echo "#"$istep"@ "$stepdate0" --> "${stepdate0F} >> ${rootsh}/NOTverifyed-datesSIM_d${idom}.inf
    echo $6${SIMi}_${SIMf}" FOLDER does not exist !!!!"
  fi

  istep=`expr $istep + 1`
#  exit
### End time-steps
done
echo "Verifyed simulation dates saved in "${rootsh}/verifying-datesSIM_d${idom}.inf
echo "NOT verifyed simulation dates saved in "${rootsh}/NOTverifyed-datesSIM_d${idom}.inf
echo "Simulations without 'rsl' dates saved in "${rootsh}/NOTrsl-datesSIM_d${idom}.inf
echo "Simulations without 'SUCCESS' dates saved in "${rootsh}/NOTsuccess-datesSIM_d${idom}.inf
echo "Simulations with 'rsl.error/out' last two lines information saved in "${rootsh}/NOTsuccess[ERROR/OUTPUT]-[head][DATEi]_[DATEe]SIM_d${idom}.inf
fi
