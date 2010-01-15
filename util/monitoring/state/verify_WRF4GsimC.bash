#!/bin/bash
if test $1 = '-h'
then
echo "****************************"
echo "***  Verifying a set of  ***" 
echo "*** CLIMATIC simulations ***"
echo "****************************"
echo "verify_WRF4GsimC.bash 'FOLDER' (simulation folder) \
'DATEi'([YYYY][MM][DD][HH][MI][SS]) 'DATEf'([YYYY][MM][DD][HH][MI][SS]) \
'CHK'(chunk size in hour) 'HEAD'(header of simulations) 'SIZE'(total size of wrfout_*, in byte) 'OUTfrq'(output frequency in hour)" 
else
file=$0
llfile=`expr length $file`
path=`expr $llfile - 21`
foresthome=`expr substr $file 1 $path`
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

TOTseconds=`${foresthome}/bats/seconds_between-dates.bash ${idate} ${fdate}`

length=`expr $7 '*' 3600`
Noutput=`expr ${TOTseconds} / ${length}`
Noutput=`expr $Noutput + 1`
lchunk=`expr $4 '*' 3600`
Nchunk=`expr ${TOTseconds} / ${lchunk}`
Nchunk=`expr ${Nchunk} - 1`
echo "Noutput: "$Noutput" Nchunk: "${Nchunk}

# Simulation dates
##

echo "idate: "$idate" fdate: "$fdate
iyyyy1=`expr substr $idate 1 4`
imm1=`expr substr $idate 5 2`
idd1=`expr substr $idate 7 2`
ihh1=`expr substr $idate 9 2`
imi1=`expr substr $idate 11 2`
iss1=`expr substr $idate 13 2`

echo "Initial date: "$iyyyy1"/"$imm1"/"$idd1" "$ihh1":"$imi1":"$iss1
echo "Total number of outputs:"${Noutput}" every "$7" hours"

stepdate=`date +%c -u -d "$iyyyy1$imm1$idd1 $seconds seconds"`
rm ${rootsh}/verifying-datesSIM_d${idom}.inf
rm ${rootsh}/NOTverifyed-datesSIM_d${idom}.inf

echo "Folder: "$1 > ${rootsh}/verifying-datesSIM_d${idom}.inf
echo "Folder: "$1 > ${rootsh}/NOTverifyed-datesSIM_d${idom}.inf
echo "#0@ "$idate

echo "sim dom "$idom" t-step: 0 "$idate

# Total outputs
##
hours=`expr $ihh1 '*' 3600`
minutes=`expr $imi1 '*' 60`
seconds=`expr $hours + $minutes + ${iss1}`

iidate=`date +%c -u -d "$iyyyy1$imm1$idd1 $seconds seconds"`

nTOT=`ls -1 $1/$5/output/wrfout*.nc | wc -l`
finisheds=`expr ${nTOT} '*' $7 '*' 3600`
finishedate=`date +%Y%m%d%H%M%S -u -d"${iidate} ${finisheds} seconds"`
Noutchunk=`expr ${lchunk} / ${length}`
Lchunk=`expr ${finisheds} / ${lchunk}`
Lchunk1=`expr ${Lchunk} + 1`

lackchunk=`expr ${Noutchunk} '*' ${Lchunk1} - ${nTOT}`
#lacksim=`expr ${lacksim} - ${Lchunk} '*' ${lchunk}`

iyyyy1=`expr substr $finishedate 1 4`
imm1=`expr substr $finishedate 5 2`
idd1=`expr substr $finishedate 7 2`
ihh1=`expr substr $finishedate 9 2`
imi1=`expr substr $finishedate 11 2`
iss1=`expr substr $finishedate 13 2`

# Not finished simulations of a chunk
##
istep=1
hours=`expr $ihh1 '*' 3600`
minutes=`expr $imi1 '*' 60`
seconds=`expr $hours + $minutes + ${iss1}`
interval=`expr $7 '*' 3600`

stepdate=`date +%c -u -d "$iyyyy1$imm1$idd1 $seconds seconds"`

while test $istep -le ${lackchunk}
do
  cd ${rootsh}

  stepdate=`date +%c -u -d"$stepdate $interval seconds"`
  stepdate0=`date +%Y%m%d%H%M%S -u -d"$stepdate"`

  echo "#"$istep"@ "$stepdate0 

  echo "#"$istep"@ "$stepdate0 >> ${rootsh}/NOTverifyed-datesSIM_d${idom}.inf

  istep=`expr $istep + 1`
### End time-steps
done

# finished chunks
##
finishedchunkS=`expr ${Lchunk} '*' ${lchunk}`

iyyyy1=`expr substr $idate 1 4`
imm1=`expr substr $idate 5 2`
idd1=`expr substr $idate 7 2`
ihh1=`expr substr $idate 9 2`
imi1=`expr substr $idate 11 2`
iss1=`expr substr $idate 13 2`

hours=`expr $ihh1 '*' 3600`
minutes=`expr $imi1 '*' 60`
seconds=`expr $hours + $minutes + ${iss1}`

stepdate=`date +%c -u -d "$iyyyy1$imm1$idd1 $seconds seconds"`

ichunk=1

while test ${ichunk} -le ${Lchunk}
do
  cd ${rootsh}
  ichunk0=`expr ${ichunk} - 1`
  secondsi=`expr ${ichunk0} '*' ${lchunk}`
  secondsf=`expr ${ichunk} '*' ${lchunk}`

  stepdate=`date +%c -u -d"$stepdate $secondsi seconds"`
  stepdateF=`date +%c -u -d"$stepdate $secondsf seconds"`
  stepdate0=`date +%Y%m%d%H%M%S -u -d"$stepdate"`
  stepdate0F=`date +%Y%m%d%H%M%S -u -d"$stepdateF"`

  echo "#"$ichunk"@ "$stepdate0" --> "${stepdate0F} 

  echo "#"$ichunk"@ "$stepdate0" --> "${stepdate0F} >> ${rootsh}/verifying-datesSIM_d${idom}.inf

  ichunk=`expr $ichunk + 1`

done

# lacked chunks
##
finishedchunkS=`expr ${Lchunk} '*' ${lchunk}`

iyyyy1=`expr substr $idate 1 4`
imm1=`expr substr $idate 5 2`
idd1=`expr substr $idate 7 2`
ihh1=`expr substr $idate 9 2`
imi1=`expr substr $idate 11 2`
iss1=`expr substr $idate 13 2`

hours=`expr $ihh1 '*' 3600`
minutes=`expr $imi1 '*' 60`
seconds=`expr $hours + $minutes + $finishedchunkS`

stepIdate=`date +%c -u -d "$iyyyy1$imm1$idd1 $seconds seconds"`

ichunk=`expr ${Lchunk} + 0`

while test ${ichunk} -le ${Nchunk}
do
  cd ${rootsh}
  ichunk1=`expr ${ichunk} + 1`
  secondsi=`expr ${ichunk} '*' ${lchunk}`
  secondsf=`expr ${ichunk1} '*' ${lchunk}`

  stepdate=`date +%c -u -d"$stepIdate $secondsi seconds"`
  stepdateF=`date +%c -u -d"$stepIdate $secondsf seconds"`
  stepdate0=`date +%Y%m%d%H%M%S -u -d"$stepdate"`
  stepdate0F=`date +%Y%m%d%H%M%S -u -d"$stepdateF"`

  echo "#"$ichunk": "$stepdate0" --> "${stepdate0F} 

  echo "#"$ichunk": "$stepdate0" --> "${stepdate0F} >> ${rootsh}/NOTverifyed-datesSIM_d${idom}.inf

  ichunk=`expr $ichunk + 1`
done

echo "Verifyed simulation dates saved in "${rootsh}/verifying-datesSIM_d${idom}.inf
echo "NOT verifyed simulation dates saved in "${rootsh}/NOTverifyed-datesSIM_d${idom}.inf
fi
