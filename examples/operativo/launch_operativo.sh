#! /bin/bash
sdate=$1

if test -z "$sdate"; then
  echo "Launch date missing"
  exit
fi
#
#  Some hard-coded paths to make it portable...
#
export PATH="/oceano/gmeteo/WORK/chus/wrf4g/ui/scripts:$PATH"
cd /oceano/gmeteo/WORK/chus/experimentos/operativo

syy=${sdate:0:4}
smm=${sdate:4:2}
sdd=${sdate:6:2}
shh=${sdate:8:2}
read fyy fmm fdd fhh <<<`date '+%Y %m %d %H' -d "${syy}-${smm}-${sdd} ${shh}:00 84 hours"`

fecha="${syy}-${smm}-${sdd}_${shh}:00:00"
fechafin="${fyy}-${fmm}-${fdd}_${fhh}:00:00"

cat << EOF | cat - wrf.input.in > wrf.input
experiment_name = "oper${sdate}"
start_date = "${fecha}"
end_date = "${fechafin}"
EOF

wrf4g_submitter.sh
#
#  Postprocess
#
pids=$(awk '{printf ":%s", $1}' pids.oper${sdate})
sed -e 's/^\ *sdate\ *=.*$/sdate='${sdate}'/' post_operatorio.sh > postop.sh
chmod +x postop.sh
qsub -W depend=afterany${pids} postop.sh
rm -f postop.sh
