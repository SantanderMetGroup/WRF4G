
inputfiles="$*"

FNAMEOUT="wrf_domain_on_the_globe.eps"
RJFLAG="-R0/360/-90/90 -JG20/0/15c"
RJFLAG="-R-11/1/42/44 -JM20/15c"

function get_borders(){
  python << EOF
from Scientific.IO.NetCDF import NetCDFFile
from sys import argv
nc = NetCDFFile("$1", "r")
latsm = nc.variables["XLAT_M"][0]
lonsm = nc.variables["XLONG_M"][0]
ix = 0
for iy   in range(latsm.shape[0]):
  print "%9.4f %9.4f" % (lonsm[iy,ix], latsm[iy,ix])
iy = len(latsm)-1
for ix in range(latsm.shape[1]):
  print "%9.4f %9.4f" % (lonsm[iy,ix], latsm[iy,ix])
ix = len(latsm[0])-1
for iy in range(latsm.shape[0]-1,0,-1):
  print "%9.4f %9.4f" % (lonsm[iy,ix], latsm[iy,ix])
iy = 0
for ix in range(latsm.shape[1]-1,0,-1):
  print "%9.4f %9.4f" % (lonsm[iy,ix], latsm[iy,ix])
EOF
}

pscoast $RJFLAG -Bg30/g30 -Dh -A0/0/1 -G164/204/161 -K > kk.eps
for geo in ${inputfiles}
do
  get_borders $geo | psxy $RJFLAG -W3,255/0/0 -N -O -K >> kk.eps
done
psxy /dev/null $RJFLAG -O >> kk.eps

exit

fixbb kk.eps $FNAMEOUT
rm -f kk.eps

exit

convert -quality 100 \
        -density 300 \
	$FNAMEOUT $(basename $FNAMEOUT .eps).jpg
