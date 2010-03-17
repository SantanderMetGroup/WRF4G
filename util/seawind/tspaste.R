##Program to paste SeaWind tslists.
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

##Program
##################################

tarfiles <- system("ls -1 *.gz", intern=TRUE)

pastetime <- 18 #Hora del "empalme", puede ser 6 12 18 o 24.
initime   <- 6  #Hora a la que empiezan las simulaciones.
                #NOTA:No funciona si initime y pastetime son de días diferentes, por ejemplo 18 y 6.
              

for (tarfile in tarfiles){

  #We get the file and the date.
  tsdate  <- datetime(tarfile)

  #Unzip files.
  system(paste("tar -xzvf", tarfile, sep=" "), intern=TRUE)

  #List unzipped files and then loop.
  tsfiles <- system("ls -1 *.TS", intern=TRUE)

  for (tsfile in tsfiles){
    
    #We get the header and the base time only for the first day of the year.
    if (substr(tarfile, 11, 14)=="0101"){
      
      header<- read.table(tsfile, nrow=1) 
      data  <- read.table(tsfile, skip=1) 

      #Calculate the timesteps that we want.
      timestep <- (data[2,2] - data[1,2])*60 #Timestep in mins.
      nlim <- ((24 -initime + pastetime)*60)/timestep # Number of timesteps to write.

      #Get the date and times and put them into the data array.
      times   <- as.POSIXct(data[1:nlim,2]*3600, origin=tsdate) 
      times   <- as.character(times, format="%Y%m%d%H%M%S")
      data    <- cbind(times, data[1:nlim,])

      filename<- paste(substr(tsfile,1,3), ".", substr(tarfile, 7, 10), ".d01.TS", sep="")#Filename to write.

      #Write the data.
      write.table(header, file=paste("./output/", filename, sep=""), quote=FALSE , col.names=FALSE, row.names=FALSE)
      write.table(data  , file=paste("./output/", filename, sep=""), append=TRUE,
      quote    = FALSE, sep="      ", 
      col.names= FALSE, row.names=FALSE)
    }
    else{
      #Paste the other days at the end of the file.
      data  <- read.table(tsfile, skip=1) 

      #Calculate the timesteps that we want.
      timestep <- (data[2,2] - data[1,2])*60 #Timestep in mins.
      nlim <- ((24 + abs(pastetime - initime))*60)/timestep # Number of timesteps to write.
      iskip<- (abs(pastetime - initime)*60)/timestep + 1 #Number of timesteps to skip in order to paste correctly.

      #Get the date and times and put them into the data array
      times <- as.POSIXct(data[iskip:nlim,2]*3600, origin=tsdate) 
      times <- as.character(times, format="%Y%m%d%H%M%S")
      data <- cbind(times, data[iskip:nlim,])

      filename<- paste(substr(tsfile,1,3), ".", substr(tarfile, 7, 10), ".d01.TS", sep="")#Filename to write.
      write.table(header, file=paste("./output/", filename, sep=""), quote=FALSE , col.names=FALSE, row.names=FALSE, append=TRUE)
      write.table(data  , file=paste("./output/", filename, sep=""), append=TRUE, 
      quote    = FALSE, sep="      ", 
      col.names= FALSE, row.names=FALSE)
    }
  }
  print(paste(tarfile, "processed"))
}

