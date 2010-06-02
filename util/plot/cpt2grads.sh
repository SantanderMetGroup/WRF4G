#!bin/bash
#Function by Markel García to transform GMT cpt files on to grads batch code to define easily color scales.
#Usage: colorscale [scale] [min level] [increment] [max level] 
#Color model must be RGB, no HSV support.

function cpt2grads(){
scale=$1
inilev=$2
inc=$3
endlev=$4
nlev=$(seq ${inilev} ${inc} ${endlev} | wc -w)

makecpt -C${scale} -T${inilev}/${endlev}/${inc} -Z | awk '/^[^A-Z]/ && NR > 3 {print $1, $2, $3, $4, "\n", $5, $6, $7, $8}'\
                                                   | awk '{print "set rgb ", $2, $3, $4}' \
                                                   | uniq | cat -n                        \
                                                   | awk '{print $2, $3, $1 + 16, $4, $5, $6}'

echo 'set clevs ' $(seq ${inilev} ${inc} ${endlev})
echo 'set ccols ' $(seq 17 $(expr 17 + $(expr ${nlev} - 1)))
}
