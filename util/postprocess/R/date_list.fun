dates_list<-function(Idate,jump,secTOT)
#
## L. Fita, August (2009), Universidad de Cantabria

### R function to give a list of dates starting one date [Idate]
## ([AAAA][MM][DD][HH][MI][SS], string) every [jump] (seconds, integer) for a
## [secTOT, integer] total of seconds

## Output given in matrix dates(secTOT/jump+1,2): 
###       col1: jump number    col2: date ([AAAA][MM][DD][HH][MI][SS], string)

####################################################
{
njumps<-as.integer(secTOT/jump)
dates<-matrix(0,nrow=(njumps+1)*2)
dim(dates)<-c(njumps+1,2)

Idate<-as.POSIXlt(Idate,format="%Y%m%d%H%M%S")

message<-paste('Initial date ',Idate,' every ',jump,' for ',secTOT,' seconds',
  sep="") 
message

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
##write(t(dates),file="date_list.dat",ncolumns=2)
}

###
## 
# End of dates_list function
