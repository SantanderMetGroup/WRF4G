######################################################################
## R para pasar los datos del QuikScat a NetCDF.                    ##
##                                                                  ##
######################################################################


##Programa de Lluis para sacar os datos del binario de /oceano/gmeteo/DATA/SSMI/QuikScat/
####################################################

dates_list_between<-function(Idate,jump,Fdate)
#
## L. Fita, August (2009), Universidad de Cantabria

### R function to create list of dates starting one date [Idate]
## ([AAAA][MM][DD][HH][MI][SS], string) every [jump] (seconds, integer) until
## [Fdate] ([AAAA][MM][DD][HH][MI][SS], string)

## Output given in matrix dates(secTOT/jump+1,2): 
###       col1: jump number    col2: date ([AAAA][MM][DD][HH][MI][SS], string)

####################################################
{
Idate<-as.Date(Idate,format="%Y%m%d%H%M%S")
Fdate<-as.Date(Fdate,format="%Y%m%d%H%M%S")

secTOT<-as.difftime(Fdate-Idate, units="secs")
secTOT<-as.numeric(secTOT,units="secs")
secTOT

message<-paste('Initial date:',Idate,'Final date:',Fdate,
'seconds between dates:',secTOT,'jump every',jump,'seconds',sep=" ")
message

njumps<-as.integer(secTOT/jump)
dates<-matrix(0,nrow=(njumps+1)*2)
dim(dates)<-c(njumps+1,2)

Idate<-as.POSIXlt(Idate,format="%Y%m%d%H%M%S")

olddate<-Idate
dates[1,1]<-0
dates[1,2]<-as.character(olddate,format="%Y%m%d%H%M%S")

for (ijump in 1:njumps) {
newdate<-olddate+jump
dates[ijump+1,1]<-ijump
dates[ijump+1,2]<-as.character(newdate,format="%Y%m%d%H%M%S")
olddate<-newdate
}

dates

## Output file (if it is desired)
##write(t(dates),file="date_list-between.dat",ncolumns=2)
}

###
## 
# End of dates_list_between function

bit3_8<-function(bit)
  .Fortran("bit38", as.integer(bit), newbit=integer(1), NAOK=FALSE)$newbit
  
### ### ### ### ### ### ### ### ### ### ### ### ### ### 

## http://www.ssmi.com/qscat/qscat_description.html
## Byte Values

##The data values fall between 0 and 255. Specific values have been reserved:
##0 to 250 	= 	valid geophysical data
##251 	= 	not used for scatterometers
##252 	= 	not used for scatterometers
##253 	= 	scatterometer observations exist, but are bad
##254 	= 	no scatterometer observations
##255 	= 	land mass

##The data values between 0 and 250 need to be scaled to obtain meaningful geophysical data. To scale the data:
##Time: 	        either multiply by 	6.0 	        to get 	0 to 1440 minute of day UTC
##  	                or multiply by 	        0.1 	        to get 	0.0 to 24.0 hour of day UTC
##Wind Speed: 	        multiply by 	        0.2 	        to get 	0 to 50.0 meters/sec
##Wind Direction: 	multiply by 	        1.5 	        to get 	0 to 360.0 degrees
##Rain Flag: 	        extract first bit 	  	        to get 	0 = no rain; 1 = rain
##Radiometer Rain: 	extract bits 3 to 8 	(x/2) - 0.5 	to get 	0 to 31 km*mm/hr

 ## ## ## ## ## ##  ## ## ## ## ## ##  ## ## ## ## ## ##  ## ## ## ## ## ## 

rootR<-system('pwd',intern=TRUE)

dates<-dates_list_between("20011111000000",24*3600,"20011112000000")

Ndates<-length(dates)
dim(dates)<-c(Ndates/2,2)

Ndates<-1
idat<-1
#for (idat in 1:Ndates) {

dates[idat,2]
ftpdir<-paste(rootR,'/',"ftp.ssmi.com/qscat/bmaps_v03a/y",sep="")
posfile<-paste(ftpdir, substr(dates[idat,2],1,4), '/m',
  substr(dates[idat,2],5,6), sep="") 
file<-paste(posfile,'/',substr(dates[idat,2],1,8),'.gz',sep="")
outputdir<-paste(rootR,'/',substr(dates[idat,2],1,4),sep="")
outputdir

# Moving files
##
instruction<-paste('cp',file,outputdir,sep=" ")
system(instruction)
instruction<-paste('cp ',file,' ./',sep="")
system(instruction)
instruction<-paste('rm ',outputdir,'/',substr(dates[idat,2],1,8),sep="")
system(instruction)
instruction<-paste('gzip -d ', outputdir, '/', substr(dates[idat,2],1,8)
,'.gz',sep="") 
system(instruction)
instruction<-paste('rm ',substr(dates[idat,2],1,8),'.gz',sep="")
system(instruction)
filename<-paste(outputdir,'/',substr(dates[idat,2],1,8),sep="")

binfile<-file(filename,open="rb")
QuikScat<-readBin(binfile, integer(), n=1440*720*4*2, size=1, signed=FALSE)
length(QuikScat)
dim(QuikScat)<- c(1440,720,4,2)

longs<-seq(0.125,360,360/1440)
lats<-seq(-89.875,89.875,180/720)

dim(longs)<-1440
dim(lats)<-720

matlongs<-rep(longs,times=720)
matlats<-rep(lats,each=1440)

dim(matlongs)<-c(1440,720)
dim(matlats)<-c(1440,720)

dim(QuikScat[,,1,1])
date<-matrix(substr(dates[idat,2],1,8),1440*720)

QuikScat<-ifelse(QuikScat>=251,NA,QuikScat)


##############################################################

##Funciones
################################

#Manual para la librería ncdf: http://cirrus.ucsd.edu/~pierce/ncdf/ncdf-manual.pdf 

createNetCDF   <- function(filename, lon, lat){
  
  #Parece que para usar create.ncdf hay que darle una variable ya definida como argumento, 
  # WSP_UP en este caso. Esto hace que la función sea muy poco general.

  lat_dim <- dim.def.ncdf( "LAT", "degreesN", lat, create_dimvar=TRUE )
  lon_dim <- dim.def.ncdf( "LON", "degreesE", lon, create_dimvar=TRUE )
  var1    <- var.def.ncdf( "WSP_UP", "m/s", list(lon_dim, lat_dim), -9999, "wind_speed_up", prec="single")
  nc1     <- create.ncdf( filename, var1)
  close.ncdf(nc1)
}

createVariable <- function(filename, varname, unit, longname="", prec="single"){

  nc1  <- open.ncdf(filename, write=TRUE) 
  var1 <- var.def.ncdf( varname, unit, list(nc1$dim$LON, nc1$dim$LAT), -9999, longname, prec)
  var.add.ncdf( nc1, var1)
  close.ncdf(nc1)
} 

writeVariable  <- function(filename, varname, a, b){ #a, b coordenadas de la variable en la matriz de QuikScat.

  nc1  <- open.ncdf(fileout, write=TRUE)  
  vals <- QuikScatreord[lons.mask, lats.mask, a, b] #Aquí podrían ir otros datos cualesquiera.
  put.var.ncdf( nc1, varname, vals) 
  close.ncdf(nc1)
}  
  
##Programa para hacer el NetCDF
##################################
library(ncdf)

#Definir el dominio aquí LAT(-89.975,89.875), LON(-179.875,179.875):
rangolon <- c(-20.875, 35.625)
rangolat <- c(28.125, 56.375)

QuikScatreord <- array(0,dim=c(1440,720,4,2))
QuikScatreord[1:(1440/2),,,]<-QuikScat[(1440/2+1):1440,,,]
QuikScatreord[(1440/2+1):1440,,,]<-QuikScat[1:(1440/2),,,] ##Reordena la matriz QuikScat para poderla tomar de -180 a +180.
QuikScatreord[,,2, 1]   <- QuikScatreord[,,2, 1]*0.2
QuikScatreord[,,2, 2]   <- QuikScatreord[,,2, 2]*0.2
QuikScatreord[,,1, 1]   <- QuikScatreord[,,1, 1]*0.1
QuikScatreord[,,1, 2]   <- QuikScatreord[,,1, 2]*0.1
QuikScatreord[,,3, 1]   <- QuikScatreord[,,3, 1]*1.5
QuikScatreord[,,3, 2]   <- QuikScatreord[,,3, 2]*1.5

fileout<-paste(outputdir,'/',substr(dates[idat,2],1,8),'.nc',sep="")

origlats <- seq(-89.875, 89.875, 0.25)
origlons <- c(seq(-(180-0.125), 0, 0.25), seq(0.125, 180, 0.25))

lons.mask <- origlons >=  rangolon[1] & origlons <= rangolon[2] #Acotan el dominio del que queremos extraer los datos.
lats.mask <- origlats >=  rangolat[1] & origlats <= rangolat[2] #
lats <- origlats[lats.mask]
lons <- origlons[lons.mask]

createNetCDF(fileout, lons, lats) ##Crea el NetCDF con la variable WPS_UP ya definida.

#Aquí se crean las otras variables.

createVariable(fileout, "WSP_DOWN" , "m/s")
createVariable(fileout, "TIMEUP"   , "min_since_0")
createVariable(fileout, "TIMEDOWN" , "min_since_0") 
createVariable(fileout, "WDIR_UP"  , "deg")
createVariable(fileout, "WDIR_DOWN", "deg") 
createVariable(fileout, "U", "m/s")
createVariable(fileout, "V", "m/s")

#Aquí se graban los datos de la matriz QuikScat en el NetCDF

writeVariable(fileout, "WSP_UP"  , 2, 1)
writeVariable(fileout, "WSP_DOWN", 2, 2)
writeVariable(fileout, "TIMEUP"  , 1, 1)
writeVariable(fileout, "TIMEDOWN", 1, 2) 
writeVariable(fileout, "WDIR_UP" , 3, 1)
writeVariable(fileout, "WDIR_DOWN",3 ,2)

nc1  <- open.ncdf(fileout, write=TRUE)  
  valsU <- QuikScatreord[lons.mask, lats.mask, 2, 1]*sin((pi/180)*QuikScatreord[lons.mask,lats.mask, 3, 1])
  valsV <- QuikScatreord[lons.mask, lats.mask, 2, 1]*cos((pi/180)*QuikScatreord[lons.mask,lats.mask, 3, 1])
  put.var.ncdf( nc1, "U", valsU) 
  put.var.ncdf( nc1, "V", valsV) 
  close.ncdf(nc1)

#}
