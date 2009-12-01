domaindef<-function(domainf)
#
## L. Fita, December (2009), Universidad de Cantabria

### R function to obtain wrf domain information. Input arguments:
## [domainf]: name of domain (with path 'geo_em.d[nn].nc')

## Output given as list [return]

## [[1]]: matrix longitudes
## [[2]]: matrix latitudes
## [[3]]: matrix landsea mask
## [[4]]: x-axe range
## [[5]]: y-axe range

####################################################
{
  ncdomain<-open.ncdf(domainfile)
  longs<-get.var.ncdf(ncdomain, "XLONG_M", start=c(1,1,1), count=c(-1,-1,1))
  lats<-get.var.ncdf(ncdomain, "XLAT_M", start=c(1,1,1), count=c(-1,-1,1))
  landsea<-get.var.ncdf(ncdomain, "LANDMASK", start=c(1,1,1), count=c(-1,-1,1))
  dx<-nrow(longs)
  dy<-ncol(longs)
  return<-list(longs,lats,landsea,dx,dy)
  return
}
