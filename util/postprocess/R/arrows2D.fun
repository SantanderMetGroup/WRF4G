arrows2D<-function(imgname, u, v, units, lcoast, scale, lngth, every, colorv,
  imagekind, show)
#
## L. Fita, December (2009), Universidad de Cantabria

### R function to draw a 2D vector field via 'arrows' instruction. Input
## arguments:
## [imgname]: name of image (with extension)
## [u],[v]: 2D vector field to be drawn (in matix form)
## [units]: wins speed units
## [lcoast]: 2D field of landsea mask to draw coastal line 
##   (if [lcoast]== 'notdrawn' it is not drawn)
## [scale]: scale of arrows respect maximum wind speed
## [lngth]: length of arrows
## [every]: frequency of plotting arrows
## [colorv]: color of arrows
## [imgkind]: kind of image: 'pdf', 'png', 'ps'
## [show]: Wheter figure should be shown (1: yes, rest: no)

## Output given figure as [imgname]

####################################################
{ 

  if (imagekind == 'png') {png(imgname)}
  if (imagekind == 'pdf') {pdf(imgname)}
  if (imagekind == 'ps') {postscript(imgname, bg="white", horizontal=FALSE,
      paper="default")}
  dx<-nrow(u)
  dy<-ncol(u)
  ptsx<-seq(1,dx,every)
  ptsy<-seq(1,dy,every)
  speed<-sqrt(u**2+v**2)
  maxspeed<-max(speed)
  xpos<-matrix(rep(1:dx,every=dy)/dx,nrow=dx,ncol=dy)
  ypos<-matrix(rep(1:dy,each=dx)/dy,nrow=dx,ncol=dy)
  unorm<-u/maxspeed
  vnorm<-v/maxspeed
  dx_u<-xpos+unorm*scale*lngth/dx
  dy_v<-ypos+vnorm*scale*lngth/dy
  TITmaxwind<-paste('max. wind:',sprintf("%5.3f",maxspeed),units,sep=" ")  
  
  if (is(lcoast)[[1]] != 'character')
  contour(lcoast,col="black", nlevels=1, drawlabels=FALSE, xaxt="n", xaxs="i",
    yaxt="n", yaxs="i", bty="n", main=TITmaxwind, asp=dy/dx) 
  else
  plot(xpos[ptsx,ptsy],  ypos[ptsx,ptsy], type="p", cex=0.01)
  arrows(xpos[ptsx,ptsy],  ypos[ptsx,ptsy], dx_u[ptsx,ptsy], dy_v[ptsx,ptsy],
    col=colorv, lwd=2,
    length=4*scale*lngth*speed[ptsx,ptsy]/(maxspeed*sqrt(dx**2+dy**2)))  
  dev.off()

  if (show==1) {
    if (imagekind == 'png') {viewer<-'display'}
    if (imagekind == 'pdf') {viewer<-'xpdf'}
    if (imagekind == 'ps') {viewer<-'display'}
    
    instruction<-paste(viewer, imgname, '&', sep=" ")
    system(instruction)}
}
