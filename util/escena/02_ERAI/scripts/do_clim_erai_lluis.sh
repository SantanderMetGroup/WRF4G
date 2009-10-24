#!/bin/bash
#How to mount a netCDF file of ERA-Interim climatologies for 1900-2003 period of tasmax and tasmin in ºC
# eraI__1990_2003__[DM/sclim/clim]_[tasmax/tasmin].nc
#   DM: Dayly values
#   sclim: seasonal climatologies
#   clim: climatology
####
#   tasmax: daily maximum 2m temperature 
#   tasmin: daily minimum 2m temperature

if test $1 = '-h' 
then
echo "********************"
echo "*** climatologic ***"
echo "***  values for  ***"
echo "*** ERA-interim  ***"
echo "***   with cdo   ***"
echo "********************"
echo "eraI_climnatic.cdo.bash 'BIT'"
echo "BIT as sumatory of___________"
echo "   1: Construction of ERA-INTERIM file of variable 2T (167 code in ECMWF)"
echo "   2: Selection of interval"
echo "   4: Selection of maximum/minimum temperature"
echo "   8: Change name and units of variable"
echo "  16: Calculation of means"
echo "  32: Make yearaverages of each dataset"
else
rootsh=`pwd`
source /software/ScientificLinux/4.6/etc/bashrc
foresthome=/oceano/gmeteo/users/lluis
bit=`${foresthome}/bats/decimal2binary.bash $1 6 | grep base2: | awk '{print $2}'`
byte1=`expr substr $bit 6 1`
byte2=`expr substr $bit 5 1`
byte3=`expr substr $bit 4 1`
byte4=`expr substr $bit 3 1`
byte5=`expr substr $bit 2 1`
byte6=`expr substr $bit 1 1`

# CDO instructions
##

#Construction of ERA-INTERIM file of variable 2T (167 code in ECMWF)
if test $byte1 -eq 1
then
cdo -f nc -t ecmwf cat /oceano/gmeteo/DATA/ECMWF/INTERIM/escena/*/INTERIM_*_SFC_167.128.grb ./eraI__2T.nc 
fi

#Selection of interval
if test $byte2 -eq 1
then
cdo selyear 1990/2008 eraI__2T.nc eraI__1990_2003__2T.nc
fi

#Selection of maximum/minimum temperature
if test $byte3 -eq 1
then
cdo daymax eraI__1990_2003__2T.nc eraI__1990_2003__tasmax0.nc
cdo daymin eraI__1990_2003__2T.nc eraI__1990_2003__tasmin0.nc
fi

#Change name and units of variable
if test $byte4 -eq 1
then
cdo -chname,T2M,tasmax -addc -273.15 eraI__1990_2003__tasmax0.nc eraI__1990_2003__tasmax.nc
cdo -chname,T2M,tasmin -addc -273.15 eraI__1990_2003__tasmin0.nc eraI__1990_2003__tasmin.nc
fi
##
# Unchanged 'long var name' and 'units' in netCDF files !! 
##

# Calculation of means
if test $byte5 -eq 1
then
cdo daymean eraI__1990_2003__tasmax.nc eraI__1990_2003_DM__tasmax0.nc
cdo daymean eraI__1990_2003__tasmin.nc eraI__1990_2003_DM__tasmin0.nc
cdo seasmean eraI__1990_2003__tasmax.nc eraI__1990_2003_sclim__tasmax0.nc
cdo seasmean eraI__1990_2003__tasmin.nc eraI__1990_2003_sclim__tasmin0.nc
cdo yearmean eraI__1990_2003__tasmax.nc eraI__1990_2003_clim__tasmax0.nc
cdo yearmean eraI__1990_2003__tasmin.nc eraI__1990_2003_clim__tasmin0.nc
fi

# dates of each climatic file
datdates=`cdo showdate eraI__1990_2003_DM__tasmax0.nc`
seasondates=`cdo showdate eraI__1990_2003_sclim__tasmax0.nc`
yeardates=`cdo showdate eraI__1990_2003_clim___tasmax0.nc`

# Make yearaverages of each dataset & invert latitudes
if test $byte6 -eq 1
then
cdo -invertlat -ydaymean eraI__1990_2003_DM__tasmax0.nc eraI__1990_2003_DM__tasmax.nc
cdo -invertlat -ydaymean eraI__1990_2003_DM__tasmin0.nc eraI__1990_2003_DM__tasmin.nc
cdo -invertlat -yseasmean eraI__1990_2003_sclim__tasmax0.nc eraI__1990_2003_sclim__tasmax.nc
cdo -invertlat -yseasmean eraI__1990_2003_sclim__tasmin0.nc eraI__1990_2003_sclim__tasmin.nc
cdo -invertlat -ymonmean eraI__1990_2003_clim__tasmax0.nc eraI__1990_2003_clim__tasmax.nc
cdo -invertlat -ymonmean eraI__1990_2003_clim__tasmin0.nc eraI__1990_2003_clim__tasmin.nc
fi

# Retrieveing land/sea mask
##cdo -t ecmwf -f nc -selday,01 -selcode,172 -invertlat /oceano/gmeteo/DATA/ECMWF/INTERIM/escena/1989/INTERIM_198901_SFC_172.128.grb ./eraI__landseamask.nc
fi
