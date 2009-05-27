#! /bin/bash
sdate=$1

if test -z "$sdate"; then
  echo "Launch date missing"
  exit
fi

export PATH="/oceano/gmeteo/WORK/chus/wrf4g/ui/scripts:$PATH"

cd /oceano/gmeteo/WORK/chus/experimentos/operativo

syy=${sdate:0:4}
smm=${sdate:4:2}
sdd=${sdate:6:2}
shh=${sdate:8:2}
read fyy fmm fdd fhh <<<`date '+%Y %m %d %H' -d "${syy}-${smm}-${sdd} ${shh}:00 84 hours"`

fecha="${syy}-${smm}-${sdd}_${shh}:00:00"
fechafin="${fyy}-${fmm}-${fdd}_${fhh}:00:00"

cat << EOF > wrf.input
experiment_name = "oper${sdate}"
start_date = "${fecha}"
end_date = "${fechafin}"
EOF

cat wrf.input.in >> wrf.input

wrf4g_submitter.sh
