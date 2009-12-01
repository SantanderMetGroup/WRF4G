figure2D<-function(field, lcoast, imagename, minval, maxval, colbar,
  longvarname, barkind, imagekind, show)
#
## L. Fita, August (2009), Universidad de Cantabria

### R function to draw a 2D field via 'image' instruction. Input arguments
## [field]: 2D field to be drawn (in matix form)
## [lcoast]: 2D field of landsea mask to draw coastal line 
##   (if [lcoast]== 'notdrawn' it is not drawn)
## [imagename]: Name of image (with extension)
## [minval]: Minimum value of the field
## [maxval]: Maximum value of the field
## [colbar]: Color vector with the complete set of colors
## [longvarname]: Variable long name with units
## [barkind]: Where to draw the bar color. There 4 options:
##    vl: Vertically on left side of figure
##    hb: Horizontally on bottom side of figure
##    vr: Vertically on right side of figure
##    ht: Horizontally on top side of figure
## [imagekind] Kind of file to record on disk png, pdf or ps.
## [show]: Wheter figure should be shown (1: yes, rest: no)

## Output given figure as [imagename]

####################################################
{
    dx<-nrow(field)
    dy<-ncol(field)
    stepsval<-(maxval-minval)/4
    if (imagekind == 'png') {png(imagename,width=480,height=480*dy/dx)}
    if (imagekind == 'pdf') {pdf(imagename,width=480,height=480*dy/dx)}
    if (imagekind == 'ps') {postscript(imagename, bg="white", horizontal=FALSE,
      paper="default",width=480,height=480*dy/dx)}

    image(field, col=colbar, zlim=c(minval, maxval), xaxs="i", yaxs="i",
      asp=dy/dx, xaxt="n", yaxt="n", bty="n")   
    contour(field, col="black", nlevels=10, zlim=c(minval, maxval),
      drawlabels=TRUE, asp=dy/dx, add=TRUE)
    if (is(lcoast)[[1]] != 'character') {
    contour(lcoast, col="gray", nlevels=1, drawlabels=FALSE, asp=dy/dx,
      add=TRUE)}
    par(xpd=NA)
  if (barkind=='hb') {
# Horizontal color bar bottom
##
    symbols(seq(0.1,0.9,0.1), matrix(-0.04,9), rectangles=cbind(matrix(0.1,9),
      matrix(0.05,9)), bg=colbar[seq(1,length(colbar),length(colbar)/9)], 
      fg="gray",                                     ## gray borders
      inches=FALSE, add=TRUE)
    text(seq(0.1,0.9,0.2), matrix(-0.1,4), labels=seq(minval, maxval, 
      stepsval))
    text(0.5,-0.15,longvarname)}

  if (barkind=='ht') {
# Horizontal color bar top
##
    symbols(seq(0.1,0.9,0.1), matrix(1.04,9), rectangles=cbind(matrix(0.1,9),
      matrix(0.05,9)), bg=colbar[seq(1,length(colbar),length(colbar)/9)], 
      fg="gray",                                     ## gray borders
      inches=FALSE, add=TRUE)
    text(seq(0.1,0.9,0.2), matrix(1.1,4), labels=seq(minval, maxval, 
      stepsval))
    text(0.5,1.15,longvarname)}

    if (barkind=='vl') {
# Vertical color bar left
##
    symbols(matrix(-0.04,9), seq(0.1,0.9,0.1), rectangles=cbind(matrix(0.05,9),
      matrix(0.1,9)), bg=colbar[seq(1,length(colbar),length(colbar)/9)], 
      fg="gray", inches=FALSE, add=TRUE)
    text(matrix(-0.08,4), seq(0.1,0.9,0.2), labels=seq(minval, maxval, 
      stepsval), srt=90)
    text(-0.12, 0.5,longvarname, srt=90)}

    if (barkind=='vr') {
# Vertical color bar right
##
    symbols(matrix(1.025,9), seq(0.1,0.9,0.1), rectangles=cbind(matrix(0.04,9),
      matrix(0.1,9)), bg=colbar[seq(1,length(colbar),length(colbar)/9)], 
      fg="gray", inches=FALSE, add=TRUE)
    text(matrix(1.03,4), seq(0.1,0.9,0.2), labels=seq(minval, maxval, stepsval),
      srt=90)
    text(1.065, 0.5,longvarname, srt=90)}

    dev.off()
    if (show==1) {
      if (imagekind == 'png') {viewer<-'display'}
      if (imagekind == 'pdf') {viewer<-'xpdf'}
      if (imagekind == 'ps') {viewer<-'display'}
    
      instruction<-paste(viewer,imagename,'&',sep=" ")
      system(instruction)}
}
