#!/bin/bash
num_alph () {
# Shell function to return a number as a alphabetical letter starting at 'a'
  num=$1
  numchar=`expr 140 + ${num}`  
  if test ${numchar} -ge 148
  then
    numchar=`expr ${numchar} + 2`
  fi
  printf "\\$numchar"
}

draw_paint () {
# Shell to paint variable evolution
  fold=$1
  exp=$2
  variable=$3
  xp=$4
  yp=$5
  ndata=$6
  rootfold=`pwd`

  cat << EOF > ${fold}/${exp}_${variable}_X${xp}_Y${yp}.gpl
#GNUplot to paint ${variable} evolution
#set terminal postscript portrait color solid
set terminal png
set output '${exp}_${variable}_X${xp}_Y${yp}.png'
xlargeval=3600*24*30
set size ratio 100./${ndata}.
set size ${ndata}./100., 1. 
set xlabel "date ([YYYY]-[MM])"
set ylabel "${variable}"
set xdata time
set x2data time
set timefmt "%Y-%m-%d_%H:%M:%S"
set format x "%Y-%m"
set format x2 "%Y-%m"
set xtics xlargeval*2 
set x2tics xlargeval*2 
set mxtics 2 
set mx2tics 2 
set ytics mirror
set grid
set grid mxtics
plot '${exp}_${variable}_X${xp}_Y${yp}.dat' using 1:4 t "" w l lt 1 lw 3 
EOF
  cd ${fold}
  gnuplot ${exp}_${variable}_X${xp}_Y${yp}.gpl
  cd ${rootfold}
}

I_S4 () {
# Shell function to transform and integer to a string of 4 characters
  integer=$1

  string=${integer}

  if test ${integer} -gt 9
  then
    string='00'${integer}
  fi

  if test ${integer} -gt 99
  then
    string='0'${integer}
  fi

  if test ${integer} -lt 9
  then
    string='000'${integer}
  fi

  echo ${string}
}

portion_between () {
# Shell function to retrieve a section of a string betweem removeIN removeEND
  String=$1
  removeIN=$2
  removeEND=$3

  llstring=`expr length ${String}`
  dremove=`expr ${llstring} - ${removeEND} - ${removeIN}`

  removeIN1=`expr ${removeIN} + 1`
  portion=`expr substr ${String} ${removeIN1} ${dremove}`
  echo ${portion}
}

#######    #######   #######    #######    #######    #######    #######    #######    #######     
    #######   #######    #######    #######    #######    #######    #######    #######     
rootsh=`pwd`
c1=\"

#experiments='scne1'
experiments='scnc1 scne1 scne3'
variables='RAINCV RAINNCV T2 Q2 U10 V10'
outputpath=${rootsh}/simstate

outputdir='/gpfs/csic_projects/meteo/DATA/WRF/experiments/'
ncdfhome='/gpfs/csic_projects/meteo/software/ScientificLinux/5.3/netcdf/gcc_gfortran_zlib+hdf5_v4.1'
wrf4ghome='/home/fital/WRF4G'
Fcompiler='gfortran'

if test ! -f netCDFvariable
then
  ${Fcompiler} ${wrf4ghome}/util/postprocess/Fortran/netCDFvariable/netCDFvariable.f90 -L${ncdfhome}/lib -lnetcdf -lm -I${ncdfhome}/include -o netCDFvariable 
fi

#mkdir -p ${outputpath}
cat << EOF > ${outputpath}/simstate.html
<HTML>
<head>
<link rel="stylesheet" type"text/css" href="./simstate.css" />
<title>WRF4G simulation state</title>
<link rel="shortcut icon" href="images/wrf4g_sq_32x32.png" border="0"/>
</head>
<body>
<div class="header">
<img src="images/wrf4g.png" height="50px"></img> 
WRF4G graphical simulation state
</div>
<div class="menu">
Experiments<br>
<ul>
EOF

for iexp in ${experiments}
do
  cd ${rootsh}/${iexp}
  echo "Experiment: "${iexp}
  for branch in ${iexp}*
  do
    cd ${rootsh}/${iexp}
    echo ${branch}
    basepath=`cat ${branch}/wrf4g.conf | grep WRF4G_BASEPATH | grep -v '#'`
    basepath=`portion_between ${basepath} 16 1`
    iout=1
    for ifile in ${basepath}/experiments/${branch}/${branch}/output/wrfout*
    do
      Lfile=`expr length ${ifile}`
      idatefile=`expr ${Lfile} - 18`
      ymdfile=`expr substr ${ifile} ${idatefile} 8`
      sfile=`date +%s -d"${ymdfile}"` 
      slastfile='-10000000000'
      if test -f ${outputpath}/${branch}_lastfile.inf
      then
        lastfile=`cat ${outputpath}/${branch}_lastfile.inf`
        Lfile=`expr length ${lastfile}`
        idatefile=`expr ${Lfile} - 18`
        ymdLastfile=`expr substr ${lastfile} ${idatefile} 8`
        slastfile=`date +%s -d"${ymdLastfile}"` 
      fi 
# Including only new simulated time-steps
##
      addfile=0
      if test ${sfile} -gt ${slastfile}
      then
        echo ${ifile}
        cd ${rootsh}
        if test ${iout} -eq 1
        then
          xpoints=`${ncdfhome}/bin/ncdump -h ${ifile} | grep west_east | grep -v ',' | grep -v 'stag' | awk '{print $3}'`
          ypoints=`${ncdfhome}/bin/ncdump -h ${ifile} | grep south_north | grep -v ',' | grep -v 'stag' | awk '{print $3}'`
          echo "xpoints: "${xpoints}" ypoints: "${ypoints}
          xpoint2=`expr ${xpoints} / 2`
          ypoint2=`expr ${ypoints} / 2`
          xpoint2S=`I_S4 ${xpoint2}`
          ypoint2S=`I_S4 ${ypoint2}`
        fi
        iout=`expr ${iout} + 1`
        for variable in ${variables}
        do
          cat << EOF > ${rootsh}/namelist.netCDFvariable
&io
  ncfile               = '${ifile}',
  variable             = '${variable}',
  timestep             = -1,
  level                = 1,
  xpoint               = ${xpoint2},
  ypoint               = ${ypoint2},
  output_path          = '${outputpath}',
  lonlattime           = 1,
  longname             = 'XLONG',
  latname              = 'XLAT',
  timename             = 'Times',
/

# timstep < 0: all time-steps
# level < 0: all levels
# xpoint < 0: all xpoints
# ypoint < 0: all ypoints
EOF
          ./netCDFvariable > /dev/null
          if test ${slastfile} -eq -10000000000 
          then
            cat ${outputpath}/${variable}_T0all_Z0001_X${xpoint2S}_Y${ypoint2S}.dat > ${outputpath}/${branch}_${variable}_X${xpoint2S}_Y${ypoint2S}.dat
          else
            cat ${outputpath}/${variable}_T0all_Z0001_X${xpoint2S}_Y${ypoint2S}.dat >> ${outputpath}/${branch}_${variable}_X${xpoint2S}_Y${ypoint2S}.dat
          fi
          echo ${ifile} > ${outputpath}/${branch}_lastfile.inf
#         exit
### End of variables
        done
      addfile=1
      fi
#    exit
### End of files
    done
    if test ${addfile} -eq 0
    then
      lastfile=`cat ${outputpath}/${branch}_lastfile.inf`
      xpoints=`${ncdfhome}/bin/ncdump -h ${ifile} | grep west_east | grep -v ',' | grep -v 'stag' | awk '{print $3}'`
      ypoints=`${ncdfhome}/bin/ncdump -h ${ifile} | grep south_north | grep -v ',' | grep -v 'stag' | awk '{print $3}'`
      xpoint2=`expr ${xpoints} / 2`
      ypoint2=`expr ${ypoints} / 2`
      xpoint2S=`I_S4 ${xpoint2}`
      ypoint2S=`I_S4 ${ypoint2}`
    fi
# Drawing
##
    echo "Drawing..."
    ifig=1
    cat << EOF > ${outputpath}/${branch}.html
<HTML>
<head>
<link rel="stylesheet" type"text/css" href="./simstate.css" />
</head>
<body>
  <div class="titleCOS">
    ${branch}
  </div>
  <div class="menuCOS">
EOF
    htmlvariables=''
    for var in ${variables}
    do
      ndata=`wc -l  ${outputpath}/${branch}_${var}_X${xpoint2S}_Y${ypoint2S}.dat | awk '{print $1}'`
      draw_paint ${outputpath} ${branch} ${var} ${xpoint2S} ${ypoint2S} ${ndata}
      htmlvariables=${htmlvariables}' <a href='${c1}${branch}'_'${var}'_X'${xpoint2S}'_Y'${ypoint2S}'.html'${c1}' class='${c1}'lcos'${c1}' target='${c1}'cosimg'${c1}'>'${var}' X'${xpoint2S}' Y'${ypoint2S}'</a> | '
      cat << EOF > ${outputpath}/${branch}_${var}_X${xpoint2S}_Y${ypoint2S}.html
<HTML>
<head>
<link rel="stylesheet" type"text/css" href="./simstate.css" />
</head>
<body>
<div class="varimg">
${var} <br>
  <img src=${c1}${branch}_${var}_X${xpoint2S}_Y${ypoint2S}.png${c1}  height=${c1}450px${c1}></img>
</div>
</body>
</HTML>
EOF
      ifig=`expr ${ifig} + 1`
### End of variables
    done
    cat << EOF >> ${outputpath}/${branch}.html
  ${htmlvariables}
  </div>
  <div class="varimg">
    <iframe src="empty.html" width="100%" height="100%" name="cosimg"></iframe>
  </div>
</body>
</HTML>
EOF
    cat << EOF >> ${outputpath}/simstate.html
<li><a href="${branch}.html" class="l1" target="graphs">${branch}</a></li>
EOF

#  exit
### End of branches of an experiment
  done 

### End of exeperiments
done
cat << EOF >> ${outputpath}/simstate.html
</ul>
</div>
<div class="screen">
<iframe src="empty.html" name="graphs" border="0" width="100%" height="100%"></iframe>
</div>
</body>
</HTML>
EOF
