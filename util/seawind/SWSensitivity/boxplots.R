reanalisis <- c('N','E')
params <- c('1','2','7')
vars <- c('pr','tas','tasmax','tasmin')
column <- 'bias'
column <- 'corr'
column <- 'stdr'


pdf(sprintf("%s.pdf",column), width=10, height=4)
par(mar=c(3,2.5,1.5,0.5),cex=1.5,las=1)
layout(t(c(1,2,3,4)))

for (var in vars){
  first.time <- T
  for (RA in reanalisis) {
    for (PHYS in params) {
      file <- sprintf("SeaWind_%s1540_BL%s_RF0618_stats_%s.txt", RA, PHYS, var)
      t <- read.table(file, header=T)
      if (first.time){
        data <- data.frame(row.names=rownames(t))
        first.time <- F
      }
      data[sprintf("%s%s",RA,PHYS)] <- t[column]
    }
  }
  if (substr(var,1,3)=='tas' & column=='bias')
    boxplot(data, main=var, ylim=c(-10,10))
  else
    boxplot(data, main=var)
}

dev.off()

