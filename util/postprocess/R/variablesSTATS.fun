variablesSTATS<-function(varname,Tstats,Ttot)
#
## L. Fita, December (2009), Universidad de Cantabria

### R function to give variable information (margins, colorbar, units, ...) of a
## CLWRF extreme-statistical 2D variable. Input arguments:
## [varname]: name of variable
## [Tstats]: clwrfH (in hour)
## [Ttot]: total time of WRF simulation

## Output given list as [result]
## [[1]]: minimum
## [[2]]: maximum
## [[3]]: colorbar
## [[4]]: units
## [[5]]: longvarname

####################################################
{
  blues<-colorRamp(c(rgb(1,1,1,1), rgb(0.5,0.5,1,1), rgb(0,0,1,1),
    rgb(1,0,1,1)))
  bluered<-colorRamp(c(rgb(0,0,1,1), rgb(1,1,1,1), rgb(1,0,0,1)))
  # T2MIN
  ##
  if (varname=="T2MIN"){
    min<-260
    max<-320
    colbar<-heat.colors(100)[100:1]
    units<-'K'
    longvarname<-paste('Minium temperature at 2m in ',Tstats,'h (', units, ')',
      sep="")} 
  # T2MAX
  ##
  if (varname=="T2MAX"){
    min<-260
    max<-320
    colbar<-heat.colors(100)[100:1]
    units<-'K'
    longvarname<-paste('Maxium temperature at 2m in ',Tstats,' h (', units,
      ')', sep="")} 
  # TT2MIN
  ##
  if (varname=="TT2MIN"){
    min<-0
    max<-Ttot
    colbar<-rainbow(100)
    units<-'minute'
    longvarname<-paste('Time of minium temperature at 2m in ', Tstats, ' h (',
      units,')', sep="")} 
  # TT2MAX
  ##
  if (varname=="TT2MAX"){
    min<-0
    max<-Ttot
    colbar<-rainbow(100)
    units<-'minute'
    longvarname<-paste('Time of maxium temperature at 2m in ', Tstats, ' h (',
      units, ')', sep="")} 
  # T2MEAN
  ##
  if (varname=="T2MEAN"){
    min<-260
    max<-320
    colbar<-heat.colors(100)[100:1]
    units<-'K'
    longvarname<-paste('Mean temperature at 2m in ', Tstats, ' h (', units, ')',
      sep="")}  
  # T2STD
  ##
  if (varname=="T2STD"){
    min<-0
    max<-5
    colbar<-heat.colors(100)[100:1]
    units<-'K'
    longvarname<-paste('Standard dev. of temperature at 2m in ', Tstats, ' h (',
      units,')',sep="")}  
  # Q2MIN
  ##
  if (varname=="Q2MIN"){
    min<-0.0
    max<-0.03
    colbar<-rgb(blues(seq(0,1,length=100)),max=255)
    units<-'kg kg-1'
    longvarname<-paste('Minium vapour mixing ratio at 2m in ',Tstats,'h (',
      units, ')', sep="")} 
  # Q2MAX
  ##
  if (varname=="Q2MAX"){
    min<-0.0
    max<-0.03
    colbar<-rgb(blues(seq(0,1,length=100)),max=255)
    units<-'kg kg-1'
    longvarname<-paste('Maxium vapour mixing ratio at 2m in ',Tstats,' h (',
      units, ')', sep="")} 
  # TQ2MIN
  ##
  if (varname=="TQ2MIN"){
    min<-0
    max<-Ttot
    colbar<-rainbow(100)
    units<-'minute'
    longvarname<-paste('Time of minium vapour mixing ratio at 2m in ', Tstats, 
      ' h (', units,')', sep="")} 
  # TQ2MAX
  ##
  if (varname=="TQ2MAX"){
    min<-0
    max<-Ttot
    colbar<-rainbow(100)
    units<-'minute'
    longvarname<-paste('Time of maxium vapour mixing ratio at 2m in ', Tstats, 
      ' h (', units, ')', sep="")} 
  # Q2MEAN
  ##
  if (varname=="Q2MEAN"){
    min<-0.0
    max<-0.03
    colbar<-rgb(blues(seq(0,1,length=100)),max=255)
    units<-'kg kg-1'
    longvarname<-paste('Mean vapour mixing ratio at 2m in ', Tstats, ' h (',
      units, ')', sep="")}  
  # Q2STD
  ##
  if (varname=="Q2STD"){
    min<-0.0
    max<-0.015
    colbar<-rgb(blues(seq(0,1,length=100)),max=255)
    units<-'kg kg-1'
    longvarname<-paste('Standard dev. of vapour mixing ratio at 2m in ', Tstats,
      ' h (', units,')',sep="")}  
  # U10MAX
  ##
  if (varname=="U10MAX"){
    min<--20
    max<-20
    colbar<-rgb(bluered(seq(0,1,length=100)),max=255)
    units<-'m s-1'
    longvarname<-paste('Maxium u-wind speed at 10m in ',Tstats,' h (', units,
      ')', sep="")} 
  # U10MEAN
  ##
  if (varname=="U10MEAN"){
    min<--20
    max<-20
    colbar<-rgb(bluered(seq(0,1,length=100)),max=255)
    units<-'m s-1'
    longvarname<-paste('Mean u-wind speed at 10m in ', Tstats, ' h (', units,
      ')', sep="")}  
  # U10STD
  ##
  if (varname=="U10STD"){
    min<-0
    max<-10
    colbar<-rgb(blues(seq(0,1,length=100)),max=255)
    units<-'m s-1'
    longvarname<-paste('Standard dev. of u-wind speed at 10m in ', Tstats, 
      ' h (',units,')',sep="")}  
  # V10MAX
  ##
  if (varname=="V10MAX"){
    min<--20
    max<-20
    colbar<-rgb(bluered(seq(0,1,length=100)),max=255)
    units<-'m s-1'
    longvarname<-paste('Maxium v-wind speed at 10m in ',Tstats,' h (', units,
      ')', sep="")} 
  # U10MEAN
  ##
  if (varname=="V10MEAN"){
    min<--20
    max<-20
    colbar<-rgb(bluered(seq(0,1,length=100)),max=255)
    units<-'m s-1'
    longvarname<-paste('Mean v-wind speed at 10m in ', Tstats, ' h (', units,
      ')', sep="")}  
  # V10STD
  ##
  if (varname=="V10STD"){
    min<-0
    max<-10
    colbar<-rgb(blues(seq(0,1,length=100)),max=255)
    units<-'m s-1'
    longvarname<-paste('Standard dev. of v-wind speed at 10m in ', Tstats, 
      ' h (',units,')',sep="")}  
  # SPDUV10MAX
  ##
  if (varname=="SPDUV10MAX"){
    min<-0
    max<-30
    colbar<-rgb(bluered(seq(0,1,length=100)),max=255)
    units<-'m s-1'
    longvarname<-paste('Maxium wind speed at 10m in ',Tstats,' h (', units,
      ')', sep="")} 
  # TSPDUVMAX
  ##
  if (varname=="TSPDUV10MAX"){
    min<-0
    max<-Ttot
    colbar<-rainbow(100)
    units<-'minute'
    longvarname<-paste('Time of maxium wind speed at 10m in ', Tstats, 
      ' h (', units, ')', sep="")} 
  # SPDUV10MEAN
  ##
  if (varname=="SPDUV10MEAN"){
    min<-0
    max<-30
    colbar<-rgb(bluered(seq(0,1,length=100)),max=255)
    units<-'m s-1'
    longvarname<-paste('Mean wind speed at 10m in ', Tstats, ' h (', units,
      ')', sep="")}  
  # SPDVUV10STD
  ##
  if (varname=="SPDUV10STD"){
    min<-0
    max<-15
    colbar<-rgb(blues(seq(0,1,length=100)),max=255)
    units<-'m s-1'
    longvarname<-paste('Standard dev. of wind speed at 10m in ', Tstats, 
      ' h (',units,')',sep="")}  
  # RAINCVMAX
  ##
  if (varname=="RAINCVMAX"){
    min<-0
    max<-0.005
    colbar<-rgb(blues(seq(0,1,length=100)),max=255)
    units<-'kg m-2 s-1'
    longvarname<-paste('Maxium cumulus precipitation flux in ',Tstats,' h (',
      units, ')', sep="")} 
  # TRAINCVMAX
  ##
  if (varname=="TRAINCVMAX"){
    min<-0
    max<-Ttot
    colbar<-rainbow(100)
    units<-'minute'
    longvarname<-paste('Time of maxium cumulus precipitation flux in ', Tstats, 
      ' h (', units, ')', sep="")} 
  # RAINCVMEAN
  ##
  if (varname=="RAINCVMEAN"){
    min<-0
    max<-0.005
    colbar<-rgb(blues(seq(0,1,length=100)),max=255)
    units<-'kg m-2 s-1'
    longvarname<-paste('Mean cumulus precipitation flux in ', Tstats, ' h (',
      units, ')', sep="")}  
  # RAINCVSTD
  ##
  if (varname=="RAINCVSTD"){
    min<-0
    max<-0.0025
    colbar<-rgb(blues(seq(0,1,length=100)),max=255)
    units<-'m s-1'
    longvarname<-paste('Standard dev. of cumulus precipitation flux in ', Tstats,
      ' h (',units,')',sep="")}  
  # RAINNCVMAX
  ##
  if (varname=="RAINNCVMAX"){
    min<-0
    max<-0.005
    colbar<-rgb(blues(seq(0,1,length=100)),max=255)
    units<-'kg m-2 s-1'
    longvarname<-paste('Maxium grid scale precipitation flux in ',Tstats,' h (',
      units, ')', sep="")}
  # TRAINNCVMAX
  ##
  if (varname=="TRAINNCVMAX"){
    min<-0
    max<-Ttot
    colbar<-rainbow(100)
    units<-'minute'
    longvarname<-paste('Time of maxium grid scale precipitation flux in ',
      Tstats, ' h (', units, ')', sep="")} 
  # RAINNCVMEAN
  ##
  if (varname=="RAINNCVMEAN"){
    min<-0
    max<-0.005
    colbar<-rgb(blues(seq(0,1,length=100)),max=255)
    units<-'kg m-2 s-1'
    longvarname<-paste('Mean grid scale precipitation flux in ', Tstats, ' h (',
      units, ')', sep="")}  
  # RAINNCVSTD
  ##
  if (varname=="RAINNCVSTD"){
    min<-0
    max<-0.0025
    colbar<-rgb(blues(seq(0,1,length=100)),max=255)
    units<-'m s-1'
    longvarname<-paste('Standard dev. of grid scale precipitation flux in ',
      Tstats, ' h (',units,')',sep="")}  
  return<-list(min,max,colbar,units,longvarname)
  return
}

