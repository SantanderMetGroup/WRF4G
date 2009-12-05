#
## L. Fita, December (2009), Universidad de Cantabria
#Create a 'tslist' file from a csv stations list
csvfile<-'PtosSeaWind.csv'

numString<-function(num,Nchar)
#
## L. Fita, December (2009), Universidad de Cantabria

### R function to transform a number to a character String (filling with zeros).
## Input arguments:
## [num]: number
## [Nchar]: number of character of resultant string

## Output given String as [result]

####################################################
{
  numS<-toString(num)
  NnumS<-nchar(numS)
  if (NnumS < Nchar) {
    result<-''
    for (ichar in 1:(Nchar-NnumS)){
    result<-paste('0', result, sep="")}
    result<-paste(result, numS, sep="")}
  else {
  result<-numS}
  result
}


data<-read.csv(csvfile,header=FALSE)
data<-as.matrix(data)
Ndata<-nrow(data)
Ndata
numTS<-1:Ndata
dim(numTS)<-Ndata
numTS<-apply(numTS, 1, numString, Nchar=3)
dataTOT<-cbind(data[,1],numTS,data[,c(2,3)])
##dataTOT
writeLines(c('#-----------------------------------------------#',
'# 24 characters for name | pfx |  LAT  |   LON  |',
'#-----------------------------------------------#'),'tslist')
write.table(cbind(sprintf("%-24s",dataTOT[,1]), sprintf("T/%3s  ",dataTOT[,2]),
  sprintf("%7.3f",as.real(dataTOT[,4])),
  sprintf("%7.3f",as.real(dataTOT[,3]))), file='tslist', row.names=FALSE,
  col.names=FALSE, quote=FALSE, append=TRUE)  
