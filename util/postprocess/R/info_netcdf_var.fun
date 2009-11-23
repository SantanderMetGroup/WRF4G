info_netcdf_var<-function(nc0,vars)

#
## L. Fita, August (2009), Universidad de Cantabria

### R function to give information from certain variables [vars] of a netcdf
## file [nc0]

### Output given in string matrix infVARnetcdf: 
##      col1: variable name    col2: variable id    
##      col3: units            col4: num. dimensions

####################################################
{
nvarnc<-nc0$nvars
nvars<-length(variables)

varncnames<-matrix(0,nvarnc)
dim(varncnames)<-nvarnc
pos_ncvar<-matrix(0,nvars)
infVARnetcdf<-matrix(0,nvars*4)
dim(infVARnetcdf)<-c(nvars,4)

for (ivar in 1:nvarnc) {
  varncnames[ivar]<-nc0$var[[ivar]]$name}
  
for (ivar in 1:nvars) {
  pos_ncvar[ivar]<-which(varncnames==vars[ivar])
  ncvar<-nc0$var[[pos_ncvar[ivar]]]
  infVARnetcdf[ivar,1]<-vars[ivar]
  infVARnetcdf[ivar,2]<-ncvar$id
  infVARnetcdf[ivar,3]<-ncvar$units
  infVARnetcdf[ivar,4]<-ncvar$ndims}

infVARnetcdf

}

###
##
# End of info_netcdf_var
