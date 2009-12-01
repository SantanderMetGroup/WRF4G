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
    result<-'0'
    for (ichar in 1:(Nchar-NnumS-1)){
    result<-paste('0', result, sep="")}
    result<-paste(result, numS, sep="")}
  else {
  result<-numS}
  result
}
