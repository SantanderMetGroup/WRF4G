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
#
#  Prepare for the GRID
#
#source /software/ScientificLinux/4.6/etc/env/egee.sh
source /software/ScientificLinux/4.6/etc/env/eelaprod.sh
#echo "RZ98cd!!" | voms-proxy-init -voms $VO -hours 48 -pwstdin

syy=${sdate:0:4}
smm=${sdate:4:2}
sdd=${sdate:6:2}
shh=${sdate:8:2}
read fyy fmm fdd fhh <<<`date '+%Y %m %d %H' -d "${syy}-${smm}-${sdd} ${shh}:00 84 hours"`

fecha="${syy}-${smm}-${sdd}_${shh}:00:00"
fechafin="${fyy}-${fmm}-${fdd}_${fhh}:00:00"

ln -sf wrf4g.conf.grid wrf4g.conf
cat << EOF | cat - wrf.input.in > wrf.input
experiment_name = "oper${sdate}"
start_date = "${fecha}"
end_date = "${fechafin}"
multiphysics_combinations = "\
4,1,1,1,1,1,1/
4,2,1,1,7,7,2/
4,1,1,1,2,2,1
"
EOF
#
#  register the input data 
#  NOTE: not necessary. The /oceano/gmeteo/DATA directory is already visible through gsiftp.
#
#vcp 
#
#  Send the simulations
#
wrf4g_submitter.sh
#
#  Postprocess
#
#pids=$(awk '{printf ":%s", $1}' pids.oper${sdate})
#sed -e 's/^\ *sdate\ *=.*$/sdate='${sdate}'/' post_operatorio.sh > postop.sh
#chmod +x postop.sh
#qsub -W depend=afterany${pids} postop.sh
#rm -f postop.sh
