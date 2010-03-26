#! /bin/bash
source dirs

function plot_climatologies(){
  for var in pr tas tasmax tasmin; do
    case $var in
        pr) plotopt="" ;;
      tas*) plotopt="has_height_dim addc -273.15 cpt cpts/tas.cpt" ;;
    esac
    for sim in CTRL CUBM CUKF BLMY LSRU MPW6 RARR; do
    bash plot_africa.gmt.sh ${POSTDIR}/CORDEX_UC_WRF_SENS${sim}_clim_${var}_1998.nc \
      title "Climatology for $sim ($var)" \
      var ${var} \
      is_curvilinear rec 0 \
      ${plotopt}
    done
  done
}

function plot_wind_climatologies(){
  for var in uas; do
    case $var in
      uas) plotopt="has_height_dim zmin 0 zmax 12 dz 1 cpt gray cptreverse" ;;
    esac
    for sim in CTRL CUBM CUKF BLMY LSRU MPW6 RARR; do
    bash plot_africa_wind.gmt.sh \
      ${POSTDIR}/CORDEX_UC_WRF_SENS${sim}_clim_${var}_1998.nc \
      ${POSTDIR}/CORDEX_UC_WRF_SENS${sim}_clim_${var/u/v}_1998.nc \
      u $var v ${var/u/v} \
      title "Climatology for $sim ($var)" \
      is_curvilinear rec 0 \
      ${plotopt}
    done
  done
}

function plot_climdiffs(){
  for var in pr tas tasmax tasmin; do
    case $var in
        pr) plotopt="cpt cpts/bias_pr.cpt" ;;
      tas*) plotopt="has_height_dim cpt cpts/bias_tas.cpt" ;;
    esac
    for sim in CUBM CUKF BLMY LSRU MPW6 RARR; do
    bash plot_africa.gmt.sh ${POSTDIR}/CORDEX_UC_WRF_SENS${sim}_diff_${var}_1998.nc \
      title "Diffs for $sim ($var)" \
      var ${var} \
      is_curvilinear rec 0 \
      ${plotopt}
    done
  done
}

function plot_wind_climdiffs(){
  for var in uas; do
    case $var in
      uas) plotopt="has_height_dim zmin 0 zmax 2 dz 0.2 cpt gray cptreverse" ;;
    esac
    for sim in CUBM CUKF BLMY LSRU MPW6 RARR; do
    bash plot_africa_wind.gmt.sh \
      ${POSTDIR}/CORDEX_UC_WRF_SENS${sim}_diff_${var}_1998.nc \
      ${POSTDIR}/CORDEX_UC_WRF_SENS${sim}_diff_${var/u/v}_1998.nc \
      u $var v ${var/u/v} \
      title "Diffs for $sim ($var)" \
      is_curvilinear rec 0 \
      ${plotopt}
    done
  done
}

function plot_spread(){
  for var in pr tas tasmax tasmin; do
    set_vars_for ${var}
    bash plot_africa.gmt.sh ${POSTDIR}/CORDEX_UC_WRF_SENS_spread_${var}.nc \
      title "Multi-param spread ($var)" \
      var ${var} \
      is_curvilinear rec 0 \
      ${plotopt}
  done
}

#plot_climatologies
#plot_wind_climatologies
#plot_climdiffs
#plot_wind_climdiffs
python monta_figs.py monta_figs.clim

