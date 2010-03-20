#! /bin/bash
source /software/ScientificLinux/4.6/etc/bashrc
scriptdir=$( (cd `dirname $0` && echo $PWD) )

EXPDIR="/vols/tetis/meteo4g/WRF/experiments"
wxajcmd="python /oceano/gmeteo/WORK/chus/wrf4g/util/postprocess/wrfnc_extract_and_join.py -a /oceano/gmeteo/WORK/chus/wrf4g/util/postprocess/wrfnc_extract_and_join.gattr_ISEAWIND -t /oceano/gmeteo/WORK/chus/wrf4g/util/postprocess/wrfnc_extract_and_join.table -g /oceano/gmeteo/DATA/WRF/domains/Europe_15k/geo_em.d01.nc -r 1950-01-01_00:00:00"
tag=$(date +'%Y%m%d%H%M%S')

function get_mask(){
  #
  #  Get a CF mask file
  #
  ${wxajcmd} -v LANDMASK --single-record -o Europe_15k.nc
  #
  #  Remove the relaxation zone and set the water to missval
  #
  cdo setctomiss,0 -maskindexbox,11,379,11,239 /oceano/gmeteo/WORK/lluis/estudios/SEAWIND/diff_res/Europe_15k.nc Europe_15k_mask.nc
}

function get_eobs(){
  #
  #  Get the EOBS data for the period and interpolate to the WRF grid and mask
  #
  for vars in rr_pr tg_tas tn_tasmin tx_tasmax
  do
    read eobsvar ipccvar <<< ${vars/_/ }
    cdo seldate,2001-10-01,2001-12-31 /oceano/gmeteo/DATA/ECA/EOBS/${eobsvar}_0.25deg_reg_v2.0.nc tmp.nc
    cdo remapdis,Europe_15k_mask.nc tmp.nc tmp2.nc
    cdo chname,${eobsvar},${ipccvar} -div tmp2.nc Europe_15k_mask.nc EOBS_DM_200110_200112_${ipccvar}.nc
  done
  cdo merge EOBS_DM_200110_200112_*.nc EOBS_DM_200110_200112.nc
  rm EOBS_DM_200110_200112_*.nc
  cdo timavg EOBS_DM_200110_200112.nc EOBS_clim_200110_200112.nc
  cdo sub EOBS_DM_200110_200112.nc EOBS_clim_200110_200112.nc EOBS_anom_200110_200112.nc
  cdo timstd EOBS_DM_200110_200112.nc EOBS_std_200110_200112.nc
  cdo div EOBS_anom_200110_200112.nc EOBS_std_200110_200112.nc EOBS_stdanom_200110_200112.nc
  cdo fldmean EOBS_DM_200110_200112.nc EOBS_smean_200110_200112.nc
  cdo sub EOBS_DM_200110_200112.nc EOBS_smean_200110_200112.nc EOBS_sanom_200110_200112.nc
  cdo fldstd EOBS_DM_200110_200112.nc EOBS_sstd_200110_200112.nc
  cdo div EOBS_sanom_200110_200112.nc EOBS_sstd_200110_200112.nc EOBS_sstdanom_200110_200112.nc
}

function process_realization() {
  #
  #  function to process one single realization
  #
  prettyname=$1
  reaname=$(awk '/'$prettyname'/{print $2}' ${scriptdir}/simulation_map.txt)
  if test -f ${prettyname}18_1H.nc; then
    echo "Cowardly refusing to overwrite ${prettyname}18_1H.nc. Delete it yourself (if you dare!)"
  else
    for dir in ${EXPDIR}/${reaname}__*
    do
      dates=${dir//*__/ }
      read datei datef <<< ${dates//_/ }
      ${wxajcmd} -v T2,RAIN,U10ER,V10ER -o s1.nc ${dir}/output/wrfout*.nc
      date1=$(date --utc '+%Y-%m-%dT%H:%M:%S' -d "${datei:0:8} 18 hours")
      date2=$(date --utc '+%Y-%m-%dT%H:%M:%S' -d "${datei:0:8} 41 hours")
      outname=${tag}__$(echo ${date1:0:13} | tr -d 'T-').nc
      echo Writing $outname
      cdo seldate,${date1},${date2} s1.nc ${outname}
    done
    rm -f s2.nc
    cdo cat ${tag}__*.nc s2.nc
    cdo seldate,2001-10-01,2001-12-31 s2.nc ${prettyname}18_1H.nc
  fi
  rm -f ${tag}__*.nc s2.nc
  cdo daysum -selvar,pr ${prettyname}18_1H.nc ${prettyname}18_DM_pr.nc
  cdo daymean -addc,-273.15 -selvar,tas ${prettyname}18_1H.nc ${prettyname}18_DM_tas.nc
  cdo chname,tas,tasmin -addc,-273.15 -daymin -selvar,tas ${prettyname}18_1H.nc ${prettyname}18_DM_tasmin.nc
  cdo chname,tas,tasmax -addc,-273.15 -daymax -selvar,tas ${prettyname}18_1H.nc ${prettyname}18_DM_tasmax.nc
  cdo merge ${prettyname}18_DM_*.nc s3.nc
  cdo div s3.nc Europe_15k_mask.nc ${prettyname}18_DM.nc
  rm -f ${prettyname}18_DM_*.nc
}

function process_continuous_realization() {
  #
  #  function to process one single continuous realization
  #
  #  e.g: SeaWind_N1540_BL1_SN
  prettyname=$1
  reaname=$(awk '/'$prettyname'/{print $2}' ${scriptdir}/simulation_map.txt)
  if test -f ${prettyname}_1H.nc; then
    echo "Cowardly refusing to overwrite ${prettyname}_1H.nc. Delete it yourself (if you dare!)"
  else
    ${wxajcmd} -v T2,RAIN,U10ER,V10ER -o s1.nc ${EXPDIR}/${reaname}/output/wrfout*.nc
    cdo seldate,2001-10-01,2001-12-31 s1.nc ${prettyname}_1H.nc
  fi
  rm -f s1.nc
  cdo daysum -selvar,pr ${prettyname}_1H.nc ${prettyname}_DM_pr.nc
  cdo daymean -addc,-273.15 -selvar,tas ${prettyname}_1H.nc ${prettyname}_DM_tas.nc
  cdo chname,tas,tasmin -addc,-273.15 -daymin -selvar,tas ${prettyname}_1H.nc ${prettyname}_DM_tasmin.nc
  cdo chname,tas,tasmax -addc,-273.15 -daymax -selvar,tas ${prettyname}_1H.nc ${prettyname}_DM_tasmax.nc
  cdo merge ${prettyname}_DM_*.nc s3.nc
  cdo div s3.nc Europe_15k_mask.nc ${prettyname}_DM.nc
  rm -f ${prettyname}_DM_*.nc
}

function get_bias(){
  prettyname=$1
  cdo timavg ${prettyname}_DM.nc  ${prettyname}_clim.nc
  cdo sub ${prettyname}_clim.nc EOBS_clim_200110_200112.nc \
    ${prettyname}_bias.nc
}

function get_corr(){
  prettyname=$1
  cdo sub ${prettyname}_DM.nc ${prettyname}_clim.nc ${prettyname}_anom.nc
  cdo timstd ${prettyname}_anom.nc ${prettyname}_std.nc
  cdo div ${prettyname}_anom.nc ${prettyname}_std.nc ${prettyname}_stdanom.nc
  cdo timavg -mul ${prettyname}_stdanom.nc EOBS_stdanom_200110_200112.nc ${prettyname}_corr.nc
}

function get_stdratio(){
  prettyname=$1
  cdo div ${prettyname}_std.nc EOBS_std_200110_200112.nc ${prettyname}_stdratio.nc
}

function get_rmse(){
  prettyname=$1
  cdo timstd -sub ${prettyname}_anom.nc EOBS_anom_200110_200112.nc ${prettyname}_rmse.nc
}

function get_sbias(){
  prettyname=$1
  cdo fldmean ${prettyname}_DM.nc  ${prettyname}_smean.nc
  cdo sub ${prettyname}_smean.nc EOBS_smean_200110_200112.nc \
    ${prettyname}_sbias.nc
}
 
function get_scorr(){
  prettyname=$1
  cdo sub ${prettyname}_DM.nc ${prettyname}_smean.nc ${prettyname}_sanom.nc
  cdo fldstd ${prettyname}_sanom.nc ${prettyname}_sstd.nc
  cdo div ${prettyname}_sanom.nc ${prettyname}_sstd.nc ${prettyname}_sstdanom.nc
  cdo fldmean -mul ${prettyname}_sstdanom.nc EOBS_sstdanom_200110_200112.nc ${prettyname}_scorr.nc
}
 
function get_sstdratio(){
  prettyname=$1
  cdo div ${prettyname}_sstd.nc EOBS_sstd_200110_200112.nc ${prettyname}_sstdratio.nc
}
 
function get_srmse(){
  prettyname=$1
  cdo fldstd -sub ${prettyname}_sanom.nc EOBS_sanom_200110_200112.nc ${prettyname}_srmse.nc
}

function ascii_dump(){
  thefile=$1
  thevar=$2
  region="-sellonlatbox,-10,5,35,44"
  region=""
  cdo -s outputf,%e,1 ${region} -selvar,${thevar} ${thefile} | awk '$1>-99999999'
}

function dump_stats() {
  prettyname=$1
  thevar=$2
  ascii_dump ${prettyname}_bias.nc ${thevar} > tmp.b
  ascii_dump ${prettyname}_corr.nc ${thevar} > tmp.c
  ascii_dump ${prettyname}_stdratio.nc ${thevar} > tmp.s
  ascii_dump ${prettyname}_rmse.nc ${thevar} > tmp.r
  paste tmp.[bcsr] | awk 'BEGIN{print "bias corr stdr rmse"}{printf "%7f %7f %7f %7f\n", $1,$2,$3,$4}'
  rm -f tmp.[bcsr]
}
#
#   Process all
#
#get_mask
#get_eobs
#for RA in N E I
#do
#  for PHYS in BL1 BL2 BL7
#  do
#    process_realization_18 SeaWind_${RA}1540_${PHYS}_RF06
#    get_bias SeaWind_${RA}1540_${PHYS}_RF0618
#    get_corr SeaWind_${RA}1540_${PHYS}_RF0618
#    get_stdratio SeaWind_${RA}1540_${PHYS}_RF0618
#    get_rmse SeaWind_${RA}1540_${PHYS}_RF0618
#    #get_sbias SeaWind_${RA}1540_${PHYS}_RF0618
#    #get_scorr SeaWind_${RA}1540_${PHYS}_RF0618
#    #get_sstdratio SeaWind_${RA}1540_${PHYS}_RF0618
#    #get_srmse SeaWind_${RA}1540_${PHYS}_RF0618
#    for var in pr tas tasmax tasmin; do
#      dump_stats SeaWind_${RA}1540_${PHYS}_RF06 ${var} > SeaWind_${RA}1540_${PHYS}_RF0618_stats_${var}.txt
#    done
#  done
#done

for real in SeaWind_N1540_BL1_SN SeaWind_N1540_BL1_GN
do
  process_continuous_realization ${real}
  get_bias ${real}
  get_corr ${real}
  get_stdratio ${real}
  get_rmse ${real}
done

