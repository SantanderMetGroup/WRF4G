arrows2D<-function(imgname, u, v, units="m/s", title="", lcoast, scale, lngth, every, colorv,
  imagekind, imgsize, show=0)
#
## L. Fita, December (2009), Universidad de Cantabria

### R function to draw a 2D vector field via 'arrows' instruction. Input
## arguments:
## [imgname]: name of image (with extension)
## [u],[v]: 2D vector field to be drawn (in matix form)
## [units]: wins speed units. m/s if not specified.
## [title]: Main title of the plot. Empty if not specified. 
## [lcoast]: 2D field of landsea mask to draw coastal line 
##   (if [lcoast]== 'notdrawn' it is not drawn)
## [scale]: scale of arrows respect maximum wind speed
## [lngth]: length of arrows
## [every]: frequency of plotting arrows
## [colorv]: color of arrows
## [imgkind]: kind of image: 'pdf', 'png', 'ps', 'jpg'
## [imgsize]: size in pixels of x dimension y dimension as imgsize*dy/dx
## [show]: Wheter figure should be shown (1: yes, rest: no) No if not specified.

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
  
  if (is(lcoast)[[1]] != 'character'){
  contour(lcoast, col="black", nlevels=1, drawlabels=FALSE, xaxt="n", xaxs="i",
    yaxt="n", yaxs="i", bty="n", main=title, cex.main=2, lwd=imgsize/480,
    cex=imgsize/480, asp=dy/dx)}
  else {
  plot(xpos[ptsx,ptsy], ypos[ptsx,ptsy], type="p", xaxt="n", xaxs="i", yaxt="n",
    yaxs="i", bty="n", xlab="", ylab="", main=title, cex.main=2, col="white",
    cex=imgsize/480, asp=dy/dx)} 
  arrows(xpos[ptsx,ptsy],  ypos[ptsx,ptsy], dx_u[ptsx,ptsy], dy_v[ptsx,ptsy],
    col=colorv, lwd=0.75*imgsize/480, length=0.75*lngth/sqrt(dx**2+dy**2))   

  dev.off()

  if (show==1) {
    if (imagekind == 'spg') {viewer<-'display'}
    if (imagekind == 'pdf') {viewer<-'xpdf'}
    if (imagekind == 'png') {viewer<-'display'}
    if (imagekind == 'ps')  {viewer<-'display'}
    
    instruction<-paste(viewer, imgname, '&', sep=" ")
    system(instruction)}
}
