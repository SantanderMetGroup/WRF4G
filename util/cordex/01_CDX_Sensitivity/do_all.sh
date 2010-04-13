#! /bin/bash
scriptdir=$( (cd `dirname $0` && echo $PWD) )
source /software/ScientificLinux/4.6/etc/bashrc || source /home/usr/etc/bashrc
source ${scriptdir}/dirs
source ${scriptdir}/diagnostics.sh
source ${scriptdir}/plot_util.sh

#tmpdir="$(pwd)/tmpdir.$(date +'%Y%m%d%H%M%S')"
#mkdir -p ${tmpdir} && cd ${tmpdir} || exit

experiment="SEN2"
period="1998-1998"
bname="${POST2DIR}/CORDEX_UC_WRF_${experiment}"

#
#  Winds and moisture at isobaric levels
#
level=850
sea=JFM
var=qua
if echo ${var} | grep -q ua ; then
  for comp in ua va; do
    if test "$var" = "qua"; then
      cdo setname,${var/ua/${comp}} -mul ${OBSDIR}/ERAIN_DM_${period}_hus${level}.nc ${OBSDIR}/ERAIN_DM_${period}_${comp}${level}.nc s1
    else
      cp ${OBSDIR}/ERAIN_DM_${period}_${var/ua/${comp}}${level}.nc s1
    fi
    calc_seasclim ${sea} s1 ${DIAGDIR}/ERAIN_sclim${sea}_${period}_${var/ua/${comp}}${level}.nc && rm s1
  done
  bash plot_africa_wind.gmt.sh \
    ${DIAGDIR}/ERAIN_sclim${sea}_${period}_${var}${level}.nc \
    ${DIAGDIR}/ERAIN_sclim${sea}_${period}_${var/ua/${comp}}${level}.nc \
    u ${var} v ${var/ua/${comp}}
else
  calc_seasclim ${sea} ${OBSDIR}/ERAIN_DM_${period}_${var}${level}.nc ${DIAGDIR}/ERAIN_sclim${sea}_${period}_${var}${level}.nc
  bash plot_africa.gmt.sh ${DIAGDIR}/ERAIN_sclim${sea}_${period}_${var}${level}.nc var ${var}
fi
#
#  LatT plots
#
var=pr
for lonrange in 27,40_27E40E_landonly -10,10_10W10E
do
  read lons label landonly <<< ${lonrange//_/ }
#  ofile="${DIAGDIR}/TRMM_LatT_${label}_${period}_${var}.nc"
#  calc_latt${landonly} $lons ${OBSDIR}/TRMM_DM_${period}_${var}.nc ${ofile}
#  bash plot_latt.gmt.sh ${ofile}
  for sim in CTRL CUBM CUKF BLMY MPW6 RARR LSRU BLPX
  do
#    ofile="${DIAGDIR}/$(basename ${bname}${sim}_LatT_${label}_${period}_${var}.nc)"
#    calc_latt${landonly} ${lons} ${bname}${sim}_DM_${period}_${var}.nc ${ofile}
#    bash plot_latt.gmt.sh ${ofile}
    montafigs_latt ${var} ${sim} ${label}
    python monta_figs.py monta_figs.cfg && rm monta_figs.cfg
  done
done

for var in tas tasmax tasmin pr
do
  for sea in JFM AMJ JAS OND
  do
    for sim in CTRL CUBM CUKF BLMY MPW6 RARR LSRU BLPX
    do
      #
      #  Seasonal climatologies
      #
      oname="${DIAGDIR}/$(basename ${bname}${sim}_sclim${sea}_${var}.nc)"
      calc_seasclim ${sea} ${bname}${sim}_DM_${period}_${var}.nc ${oname}
      plot_climatology ${var} ${oname}
      #
      #  Seasonal biases w.r.t. CTRL
      #
      if test "${sim}" != "CTRL"; then
        onameb="${DIAGDIR}/$(basename ${bname}${sim}_sdiff${sea}_${var}.nc)"
        cdo sub ${DIAGDIR}/$(basename ${bname}${sim}_sclim${sea}_${var}.nc) \
          ${DIAGDIR}/$(basename ${bname}CTRL_sclim${sea}_${var}.nc) \
          ${onameb}
        plot_bias ${var} ${onameb}
      fi
    done # sim
    montafigs_sclim ${var} ${sea}
    python monta_figs.py monta_figs.cfg && rm monta_figs.cfg
    montafigs_sdiff ${var} ${sea}
    python monta_figs.py monta_figs.cfg && rm monta_figs.cfg
  done # sea
done # var

for var in vas uas
do
  for sea in JFM AMJ JAS OND
  do
    for sim in CTRL CUBM CUKF BLMY MPW6 RARR LSRU BLPX
    do
      #
      #  Seasonal climatologies
      #
      oname="${DIAGDIR}/$(basename ${bname}${sim}_sclim${sea}_${var}.nc)"
      calc_seasclim ${sea} ${bname}${sim}_DM_${period}_${var}.nc ${oname}
      #
      #  Seasonal biases w.r.t. CTRL
      #
      if test "${sim}" != "CTRL"; then
        onameb="${DIAGDIR}/$(basename ${bname}${sim}_sdiff${sea}_${var}.nc)"
        cdo sub ${DIAGDIR}/$(basename ${bname}${sim}_sclim${sea}_${var}.nc) \
          ${DIAGDIR}/$(basename ${bname}CTRL_sclim${sea}_${var}.nc) \
          ${onameb}
      fi
    done # sim
  done # sea
done # var


#
#   Reference seasonal climatologies TRMM, CRU, ERAIN
#
var=pr
for sea in JFM AMJ JAS OND; do
#  cdo remapbil,CRU_mask.nc ${OBSDIR}/TRMM_MM_${period}_${var}.nc s1
#  calc_seasclim ${sea} s1 ${DIAGDIR}/TRMM_sclim${sea}_${period}_${var}.nc
#  bash plot_africa.gmt.sh ${DIAGDIR}/TRMM_sclim${sea}_${period}_${var}.nc var pr validrange 0,100
  # CRU - they are not masked! (TODO: mask them before this point.. in OBSDIR, with get_cru.sh)
  cdo mul -setctomiss,0 CRUTS30AFR_sftlf.nc ${OBSDIR}/CRUTS30AFR_MM_${period}_${var}.nc s1
  calc_seasclim ${sea} s1 ${DIAGDIR}/CRUTS30AFR_sclim${sea}_${period}_${var}.nc
  bash plot_africa.gmt.sh ${DIAGDIR}/CRUTS30AFR_sclim${sea}_${period}_${var}.nc var pr validrange 0,100
done

for var in tasmax tasmin tas
do
  for sea in JFM AMJ JAS OND; do
    # they are not masked!
    cdo mul -setctomiss,0 CRUTS30AFR_sftlf.nc ${OBSDIR}/CRUTS30AFR_MM_${period}_${var}.nc s1
    calc_seasclim ${sea} s1 ${DIAGDIR}/CRUTS30AFR_sclim${sea}_${period}_${var}.nc
    bash plot_africa.gmt.sh ${DIAGDIR}/CRUTS30AFR_sclim${sea}_${period}_${var}.nc var ${var} validrange -90,100 cpt cpts/tas.cpt
  done
done

var=tas
for sea in JFM AMJ JAS OND; do
  cdo remapbil,CRU_mask.nc ${OBSDIR}/ERAIN_MM_${period}_${var}.nc s1
  calc_seasclim ${sea} s1 ${DIAGDIR}/ERAIN_sclim${sea}_${period}_${var}.nc
  bash plot_africa.gmt.sh ${DIAGDIR}/ERAIN_sclim${sea}_${period}_${var}.nc var tas validrange -90,100
done
#
#   Bias CRU-TRMM
#
var=pr
for sea in JFM AMJ JAS OND
do
  oname="${DIAGDIR}/CRUTS30AFR_sbias${sea}_${period}_${var}.nc"
  cdo sub ${DIAGDIR}/CRUTS30AFR_sclim${sea}_${period}_${var}.nc ${DIAGDIR}/TRMM_sclim${sea}_${period}_${var}.nc ${oname}
  bash plot_africa.gmt.sh ${oname} var ${var} validrange -50,50 cpt cpts/bias_${var}.cpt
done
#
#   Bias ERAIN-CRU
#
var=tas
for sea in JFM AMJ JAS OND
do
  oname="${DIAGDIR}/ERAIN_sbias${sea}wrtCRU_${period}_${var}.nc"
  cdo sub ${DIAGDIR}/ERAIN_sclim${sea}_${period}_${var}.nc ${DIAGDIR}/CRUTS30AFR_sclim${sea}_${period}_${var}.nc ${oname}
  bash plot_africa.gmt.sh ${oname} var ${var} validrange -50,50 cpt cpts/bias_${var}.cpt
done
#
#   Bias against TRMM
#
var=pr
for sim in CTRL CUBM CUKF BLMY MPW6 RARR LSRU BLPX
do
  for sea in JFM AMJ JAS OND
  do
    oname="${DIAGDIR}/$(basename ${bname}${sim}_sbias${sea}_${var}.nc)"
    cdo remapbil,CRU_mask.nc ${DIAGDIR}/$(basename ${bname}${sim}_sclim${sea}_${var}.nc) s1
    cdo sub s1 ${DIAGDIR}/TRMM_sclim${sea}_${period}_${var}.nc ${oname}
    bash plot_africa.gmt.sh ${oname} var pr validrange -50,50 cpt cpts/bias_pr.cpt
  done
  montafigs_bias ${var} ${sim} TRMM
done
for sea in JFM AMJ JAS OND
do
  montafigs_bias8trmm ${var} ${sea} 
done
#
#   Bias against CRU
#
for var in tas tasmax tasmin # pr 
do
  for sea in JFM AMJ JAS OND; do
    for sim in CTRL CUBM CUKF BLMY MPW6 RARR LSRU BLPX
    do
      oname="${DIAGDIR}/$(basename ${bname}${sim}_sbias${sea}wrtCRU_${var}.nc)"
      cdo remapbil,CRU_mask.nc ${DIAGDIR}/$(basename ${bname}${sim}_sclim${sea}_${var}.nc) s1
      cdo sub \
        $(test "${var:0:2}" = "ta" && echo "-addc,-273.15") s1 \
        ${DIAGDIR}/CRUTS30AFR_sclim${sea}_${period}_${var}.nc \
        ${oname}
      bash plot_africa.gmt.sh ${oname} var ${var} validrange -50,50 cpt cpts/bias_${var}.cpt
    done
    montafigs_bias8 ${var} ${sea}
  done
done
var=tas
for sim in CTRL CUBM CUKF BLMY MPW6 RARR LSRU BLPX
do
  montafigs_bias ${var} ${sim} CRUTS30AFR ERAIN
done
#
#  Monthly annual cycles
#
for var in pr tas tasmax tasmin
do
#  for region in AM WA-N WA-S CA-NH CA-SH SA-WN SA-WS SA-E EA EH
#  do
#    calc_areamac ${region} ${OBSDIR}/CRUTS30AFR_MM_${period}_${var}.nc ${DIAGDIR}/CRUTS30AFR_MAC_${region}_${period}_${var}.txt
#    calc_areamac ${region} ${OBSDIR}/TRMM_MM_${period}_${var}.nc ${DIAGDIR}/TRMM_MAC_${region}_${period}_${var}.txt
#    for sim in CTRL CUBM CUKF BLMY MPW6 RARR LSRU BLPX
#    do
#      calc_areamac ${region} ${bname}${sim}_DM_${period}_${var}.nc ${DIAGDIR}/$(basename ${bname}${sim}_MAC_${region}_${period}_${var}.txt) \
#        $(test "${var:0:2}" = "ta" && echo "-addc,-273.15")
#    done
#    bash plot_mac.gmt.sh ${region} ${var}
#    # rm ${DIAGDIR}/*_MAC_${region}_${period}_${var}.txt
#  done
  montafigs_mac $var
  python monta_figs.py monta_figs.cfg && rm monta_figs.cfg
done  

#
#  3-hourly daily cycles
#
for sea in JFM AMJ JAS OND
do
  for var in pr # tas
  do
#    for region in AM WA-N WA-S CA-NH CA-SH SA-WN SA-WS SA-E EA EH
#    do
#      #calc_area3dc ${region} ${sea} ${OBSDIR}/ERAIN_3H_${period}_${var}.nc ${DIAGDIR}/CRUTS30AFR_MAC_${region}_${period}_${var}.txt
#      calc_area3dc ${region} ${sea} ${OBSDIR}/TRMM_3H_${period}_${var}.nc ${DIAGDIR}/TRMM_3DC${sea}_${region}_${period}_${var}.txt
#      for sim in CTRL CUBM CUKF BLMY MPW6 RARR LSRU BLPX
#      do
#        calc_area3dc ${region} ${sea} ${bname}${sim}_3H_${period}_${var}.nc ${DIAGDIR}/$(basename ${bname}${sim}_3DC${sea}_${region}_${period}_${var}.txt) \
#          $(test "${var:0:2}" = "ta" && echo "-addc,-273.15")
#      done
#      bash plot_3dc.gmt.sh ${region} ${sea} ${var}
#      #rm ${DIAGDIR}/*_3DC${sea}_${region}_${period}_${var}.txt
#    done # region
    montafigs_3dc $var $sea
    python monta_figs.py monta_figs.cfg && rm monta_figs.cfg
  done # var
done # sea


