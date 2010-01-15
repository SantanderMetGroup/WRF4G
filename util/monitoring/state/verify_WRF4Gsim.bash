#!/bin/bash 
if test $1 = '-h'
then
echo "***************************"
echo "***     Verifying a     ***" 
echo "*** YEAR of simulations ***"
echo "***************************"
echo "verify_WRF4Gsim.bash 'FOLDER' (simulation folder) \
'DATEi'([YYYY][MM][DD][HH][MI][SS]) 'DATEf'([YYYY][MM][DD][HH][MI][SS]) \
'FRQ'(frequency of simulations in hour) 'LNG'(length of simulations in hour) 'HEAD'(header of simulations) 'SIZE'(total size of wrfout_*, in byte) 'OUTfrq'(output frequency in hour)" 
else
file=$0
llfile=`(expr length $file)`
path=`(expr $llfile - 20)`
foresthome=`(expr substr $file 1 $path)`
foresthome=${foresthome}/..
rootsh=`pwd`

c1="'"
c2="$"
c3='`'

idom=1

# Domain dates
##
idate=$2
fdate=$3

interval=`expr $4 '*' 3600`
SIMinterval=`expr $5 '*' 3600`
Noutput=`expr $5 / $8`
Noutput=`expr $Noutput + 1`
echo "Noutput: "$Noutput

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
rm ${rootsh}/verifying-datesSIM_d${idom}.inf
rm ${rootsh}/NOTverifyed-datesSIM_d${idom}.inf

echo "Folder: "$1 > ${rootsh}/verifying-datesSIM_d${idom}.inf
echo "Folder: "$1 > ${rootsh}/NOTverifyed-datesSIM_d${idom}.inf
echo "#0@ "$idate

echo "sim dom "$idom" t-step: 0 "$idate

while test $istep -le $numsteps
do
  cd ${rootsh}

  stepdate=`date +%c -u -d"$stepdate $interval seconds"`
  stepdateF=`date +%c -u -d"$stepdate $SIMinterval seconds"`
  stepdate0=`date +%Y%m%d%H%M%S -u -d"$stepdate"`
  stepdate0F=`date +%Y%m%d%H%M%S -u -d"$stepdateF"`
  SIMi=`date +%Y%m%d%H -u -d"$stepdate"`
  SIMf=`date +%Y%m%d%H -u -d"$stepdateF"`

  echo "#"$istep"@ "$stepdate0" --> "${stepdate0F} 

  if test -d $1/$6${SIMi}_${SIMf}
  then
    nTOT=`ls -1 $1/$6${SIMi}_${SIMf}/output/wrfout* | wc -l`
    TOT=`du -c $1/$6${SIMi}_${SIMf}/output/wrfout* | grep total | awk '{print $1}'`
    if test ${nTOT} -ne $Noutput
    then
      echo "#"$istep"@ "$stepdate0" --> "${stepdate0F} >> ${rootsh}/NOTverifyed-datesSIM_d${idom}.inf
    else
      echo "#"$istep"@ "$stepdate0" --> "${stepdate0F} >> ${rootsh}/verifying-datesSIM_d${idom}.inf
##      echo ${stepdate0F} > $1/$6${SIMi}_${SIMf}/output/current_date.txt
    fi
  else
    echo "#"$istep"@ "$stepdate0" --> "${stepdate0F} >> ${rootsh}/NOTverifyed-datesSIM_d${idom}.inf
    echo $6${SIMi}_${SIMf}" FOLDER does not exist !!!!"
  fi

  istep=`expr $istep + 1`
### End time-steps
done
echo "Verifyed simulation dates saved in "${rootsh}/verifying-datesSIM_d${idom}.inf
echo "NOT verifyed simulation dates saved in "${rootsh}/NOTverifyed-datesSIM_d${idom}.inf
fi
