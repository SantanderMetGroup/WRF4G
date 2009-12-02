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
## [imgkind]: kind of image: 'pdf', 'png', 'ps', 'jpg'
## [imgsize]: size in pixels of x dimension y dimension as imgsize*dy/dx
## [show]: Wheter figure should be shown (1: yes, rest: no)

## Output given figure as [imgname]

####################################################
{ 

  dx<-nrow(u)
  dy<-ncol(u)

  if (imagekind == 'jpg') {jpeg(imgname, bg="white", width=imgsize,
    height=imgsize*dy/dx)}
  if (imagekind == 'pdf') {pdf(imgname, bg="white", width=imgsize,
    height=imgsize*dy/dx)}
  if (imagekind == 'png') {png(imgname, bg="white", width=imgsize,
    height=imgsize*dy/dx)}
  if (imagekind == 'ps') {postscript(imgname, bg="white", horizontal=FALSE,
      paper="default", width=imgsize, height=imgsize*dy/dx)}
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
  
  if (is(lcoast)[[1]] != 'character'){
  contour(lcoast, col="black", nlevels=1, drawlabels=FALSE, xaxt="n", xaxs="i",
    yaxt="n", yaxs="i", bty="n", main=TITmaxwind, cex.main=4, lwd=imgsize/480,
    cex=imgsize/480, asp=dy/dx)}
  else {
  plot(xpos[ptsx,ptsy], ypos[ptsx,ptsy], type="p", xaxt="n", xaxs="i", yaxt="n",
    yaxs="i", bty="n", xlab="", ylab="", main=TITmaxwind, cex.main=4,
    cex=imgsize/480, asp=dy/dx)} 
  arrows(xpos[ptsx,ptsy],  ypos[ptsx,ptsy], dx_u[ptsx,ptsy], dy_v[ptsx,ptsy],
    col=colorv, lwd=imgsize*4/480, length=imgsize*4/480*scale*lngth*speed[ptsx,
    ptsy]/(maxspeed*sqrt(dx**2+dy**2)))   
  dev.off()

  if (show==1) {
    if (imagekind == 'spg') {viewer<-'display'}
    if (imagekind == 'pdf') {viewer<-'xpdf'}
    if (imagekind == 'png') {viewer<-'display'}
    if (imagekind == 'ps') {viewer<-'display'}
    
    instruction<-paste(viewer, imgname, '&', sep=" ")
    system(instruction)}
}
