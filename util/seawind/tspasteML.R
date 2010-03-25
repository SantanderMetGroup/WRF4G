##Program to paste SeaWind tslists in MeteoLab format.
####################################################################################
##Functions.
##################################
datetime <- function(name, time=TRUE){

  year   <- substr(name,  7, 10)
  month  <- substr(name, 11, 12)
  day    <- substr(name, 13, 14)
  hour   <- substr(name, 16, 17)
  min    <- substr(name, 19, 20)
  
  if (time){
    output <- paste(year, "-", month, "-", day, " ", hour, ":", min, ":00", sep="") 
  }
  else{
    output <- paste(year, "-", month, "-", day, sep="")
  }

  return(output)
}

#Function for interpolate to a new timestep.
#input:data to interpolate.oldtimes:Old time series in mins. newpas:new timestep
timestep_interp <- function(input, oldtimes, newstep){

  newtimes <- seq(newstep, max(oldtimes), newstep)
  output   <- approx(oldtimes, input, xout=newtimes)
  return(output)

}

##Program
##################################

##Configuración:
##-----------------------
pastetime <- 18 #Hora del "empalme", puede ser 6 12 18 o 24.
initime   <- 6  #Hora a la que empiezan los 'chunks'.
inputdir  <- "/vols/tetis/meteo4g/SEAWIND/SeaWind_N3028RF06__1948_2009/1991"
outputdir <- "/vols/tetis/meteo4g/SEAWIND/tslists/SeaWind_N3028RF06__1948_2009/1991_form"

#Formato de la salida. Las variables son:
#t, q, u, v, psfc, glw, gsw, hfx, lh, tsk, tslb(1), rainc, rainnc
formats <- c("%3.1f", "%1.6f", "%2.1f", "%2.1f", "%6.1f", "%3.5f", "%3.5f", "%2.5f",
             "%3.6f", "%3.5f", "%3.1f", "%3.2f", "%3.2f")  

##-----------------------

setwd(inputdir)
tarfiles <- system("ls -1 *.gz", intern=TRUE)    
system(paste("mkdir ", outputdir, sep="")) 
system(paste("mkdir ", outputdir, "/data", sep=""))       
system(paste("cp /oceano/gmeteo/WORK/markel/R/Variables.txt ", outputdir, sep=""))
system(paste("cp /oceano/gmeteo/WORK/markel/R/Stations.txt " , outputdir, sep=""))
datadir <- paste(outputdir, "/data", sep="")

for (tarfile in tarfiles){

  #We get the file and the date.
  tsdate  <- datetime(tarfile)

  #Unzip files.
  system(paste("tar -xzvf", tarfile, sep=" "), intern=TRUE)

  #List unzipped files and then loop.
  tsfiles  <- system("ls -1 *.TS", intern=TRUE) 

  for (tsfile in tsfiles){
    
    #We get the data and the varnames.
    if (substr(tarfile, 11, 14)=="0101"){
      
      data     <- read.table(tsfile, skip=1) 
      command  <- paste("cat /oceano/gmeteo/WORK/markel/R/Variables.txt", " | awk '{print $1}' ", "| tr -d ',' ", sep="")
      varnames <- system(command, intern=TRUE)

      #Creates a dir for each variable.
      if (tsfile=="001.d01.TS"){
	for (varname in varnames){
	  system(paste("mkdir ", datadir, "/", varname, sep=""))
	}
      }

      #Calculate the timesteps that we want.
      timestep1 <- (data[2,2] - data[1,2])*60 #Timestep in mins.
      nlim <- ((24 -initime + pastetime)*60)/timestep1 # Number of timesteps to write.

      #Get the initial and the final dates.
      inidate <- as.POSIXct(data[1,2]*3600, origin=tsdate) 
      inidate <- as.character(inidate, format="%Y-%m-%d")
      year    <- as.character(inidate, format="%Y")
      enddate <- datetime(tarfiles[length(tarfiles)], time=FALSE)
      
      #Filenames and directories.
      filename <- paste(substr(tsfile,1,3), ".txt", sep="")
      filedirs <- c(paste(datadir, "/", varnames[1:14], "/", filename, sep=""))

      #Write the data.
      for (j in 1:14){
        cat(paste(inidate, ",", enddate, "\n", sep=""), file=filedirs[j])
      }
      write.table(data[1:nlim, 2]  , file=filedirs[1], append=TRUE,
        quote    = FALSE, sep="      ", 
        col.names= FALSE, row.names=FALSE)
      for (k in 1:13){
	write.table(sprintf(formats[k], data[1:nlim, 5 + k])  , file=filedirs[1 + k], append=TRUE,
	  quote    = FALSE, sep="      ", 
	  col.names= FALSE, row.names=FALSE)   
      }
    }
    else{
      #Paste the other days at the end of the file.
      data  <- read.table(tsfile, skip=1) 
     
      #Calculate the timesteps that we want.
      timestep <- (data[2,2] - data[1,2])*60 #Timestep in mins.
      nlim  <- ((24 + abs(pastetime - initime))*60)/timestep # Number of timesteps to write.
      iskip <- (abs(pastetime - initime)*60)/timestep + 1 #Number of timesteps to skip in order to paste correctly.

      #Interpolate the files witha different timestep.
      if (timestep != timestep1){
        for (m in 1:13){
	  interp <- timestep_interp(data[iskip:nlim, m + 5], data[iskip:nlim, 2]*60, timestep1, (iskip - 1)*timestep + timestep1)  
          if (m==1){data2  <- array(0, c(length(interp[[2]]), 14))}
          data2 <- as.data.frame(data2)
	  data2[, 1]  <- interp[[1]]/60
	  if (m != 2){data2[, m + 1]  <- interp[[2]]}
        }   
	#Write the data
	  
        filename<- paste(substr(tsfile,1,3), ".", substr(tarfile, 7, 10), ".d01.TS", sep="")#Filename to write.

        write.table(data2[, 1], file=filedirs[1], append=TRUE,
          quote    = FALSE, sep="      ", 
          col.names= FALSE, row.names=FALSE)
        for (k in 1:13){
	  write.table(sprintf(formats[k], data2[, 1 + k])  , file=filedirs[1 + k], append=TRUE,
	    quote    = FALSE, sep="      ", 
	    col.names= FALSE, row.names=FALSE) 
        }  
      }
      else{     

	#Filenames and directories.
	filename <- paste(substr(tsfile,1,3), ".txt", sep="")
	filedirs <- c(paste(datadir, "/", varnames[1:14], "/", filename, sep=""))
	
	write.table(data[iskip:nlim, 2]  , file=filedirs[1], append=TRUE,
	  quote    = FALSE, sep="      ", 
	  col.names= FALSE, row.names=FALSE)
	for (l in 1:13){
	  write.table(sprintf(formats[l], data[iskip:nlim, 5 + l]) , file=filedirs[1 + l], append=TRUE,
	    quote    = FALSE, sep="      ", 
	    col.names= FALSE, row.names=FALSE)   
	}
      }
    }
  }
  print(paste(tarfile, "processed"))
}
timestep <- as.POSIXct(timestep1*60, origin="1999-1-1 00:00:00")
timestep <- as.character(timestep1, format="%H:%M:%S")
setwd(outputdir)
system(paste("sed -i 's/@timestep@/'",timestep,"'/' Variables.txt", sep=""))


