#
# Copyright 2016 Universidad de Cantabria
#
# Licensed under the EUPL, Version 1.1 only (the
# "Licence");
# You may not use this work except in compliance with the
# Licence.
# You may obtain a copy of the Licence at:
#
# http://ec.europa.eu/idabc/eupl
#
# Unless required by applicable law or agreed to in
# writing, software distributed under the Licence is
# distributed on an "AS IS" basis,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either
# express or implied.
# See the Licence for the specific language governing
# permissions and limitations under the Licence.
#

import os
import logging
import fortran_namelist as fn
from os.path          import exists
from wrf4g.utils.time import datetime2datewrf

def get_ptop():
    shcmd = "ncdump -v PRES $(\ls met_em.d??.????-??-??_??[:_]00[:_]00.nc | head -1) | tail -2 | head -1 | tr -d ',;' | awk '{printf $1}'"
    out=os.popen(shcmd).read().strip()
    return int(out)

def fix_ptop( namelist_input ):
    top_press = get_ptop()
    nmli = fn.WrfNamelist( namelist_input )
    try :
        specp_top = int( nmli.getValue( "p_top_requested" )[0] )
    except Exception :
        if top_press > 5000 :
            logging.info( "Default p_top is 5000., but your input files only reach %d. Fixing..." % top_press )
            nmli.setValue( "p_top_requested ", top_press, "domains" )
        else :
            logging.info( "Default p_top will be 5000." )
            nmli.setValue( "p_top_requested ", 5000, "domains" )
    else :
        if specp_top < top_press :
            logging.info( "Specified p_top is %d, but your input files only reach %d. Fixing..." % ( specp_top, top_press ) )
            nmli.setValue( "p_top_requested", top_press )
    nmli.wrfCheck()
    nmli.overWriteNamelist()

def get_num_metgrid_levels():
    shcmd = "ncdump -h $(\ls -1 met_em*.nc | head -1) | grep 'num_metgrid_levels =' | sed -e 's/^\t//' | tr '=;' '  ' | awk '{print $2}'"
    out=os.popen(shcmd).read().strip()
    return int(out)

def get_num_metgrid_soil_levels():
    shcmd = "ncdump -h $(\ls -1 met_em*.nc | head -1) | grep 'NUM_METGRID_SOIL_LEVELS' | sed -e 's/^\t//' | tr '=:;' '   ' | awk '{print $2}'"
    out=os.popen(shcmd).read().strip()
    return int(out)

def get_time_step(coarse_dx, factor):
    HOUR_DIVISORS = [
        1,2,3,4,5,6,8,9,10,12,15,16,18,20,24,25,30,36,40,45,48,50,60,
        72,75,80,90,100,120,144,150,180,200,225,240,300,360,400,450,600,
        720,900,1200
        ]
    tstep = int(factor*coarse_dx/1000)
    # Added list to fix execution in python 3
    ival = list(map(lambda x: x<=tstep, HOUR_DIVISORS)).index(0) - 1
    return HOUR_DIVISORS[ival]

def get_latlon_dx(start_date, dom):
    #  Try to get dx from the met_em or wrfinput files. Only
    #  required for lat-lon grids, otherwise it is available
    #  in the namelist.wps file
    dxfile=None
    file_name = "met_em.%s.%s.nc" % ( dom, datetime2datewrf( start_date ) )
    if exists( file_name ) :
        dxfile = file_name
        logging.info("Reading dx from %s" % file_name)
    else:
        file_name = "wrfinput_%s" % dom
        dxfile = file_name
        logging.info("Reading dx from %s" % file_name)
    if dxfile:
        shcmd = "ncdump -h %s | grep 'DX =' | sed -e 's/^\t//' | tr '=;' ' ' | awk '{printf \"%%f\", $2}'" % dxfile
        rval = round(float(os.popen(shcmd).read().strip()), 4)
    else:
        raise Exception('get_latlon_dx: no met_em or wrfinput file found')
    return rval

def wps2wrf( namelist_wps, namelist_input, sdate, edate, maxdom, chunk_is_restart, timestep_dxfactor='6') :
    nmlw = fn.FortranNamelist( namelist_wps )
    nmli = fn.WrfNamelist( namelist_input )
    nmli.setValue("max_dom", maxdom)
    for var in ["run_days", "run_hours", "run_minutes", "run_seconds"]:
        nmli.setValue(var, 0)
    nmli.setMaxDomValue("start_year",  sdate.year)
    nmli.setMaxDomValue("start_month", sdate.month)
    nmli.setMaxDomValue("start_day",   sdate.day)
    nmli.setMaxDomValue("start_hour",  sdate.hour)
    nmli.setMaxDomValue("end_year",    edate.year)
    nmli.setMaxDomValue("end_month",   edate.month)
    nmli.setMaxDomValue("end_day",     edate.day)
    nmli.setMaxDomValue("end_hour",    edate.hour)
    vars_to_copy = ["parent_grid_ratio", "i_parent_start", "j_parent_start",
                    "e_we", "e_sn", "interval_seconds"]
    for var in vars_to_copy:
        nmli.setValue(var, nmlw.getValue(var))
    nmli.setValue("parent_time_step_ratio", nmlw.getValue("parent_grid_ratio"))
    if exists("met_em.d01.%s.nc" % datetime2datewrf( sdate ) ):
        # If there are met_em files, we need to run real.exe. Otherwise, we
        # cannot get enough info (num_metgrid_*levels) to run real.exe
        nmli.setValue("num_metgrid_levels", get_num_metgrid_levels())
        nmli.setValue("num_metgrid_soil_levels", get_num_metgrid_soil_levels())
    #
    #  Compute the grid spacings. Read them from met_em files if the projection is lat-lon.
    #
    nmli.setValue("grid_id", list(range(1, maxdom+1)))
    # Update parant_id in the namelist
    nmli.setValue("parent_id", nmlw.getValue("parent_id"))

    alldx = []
    for idom in range(1,maxdom + 1):
        thisdx = get_latlon_dx(sdate, "d0%i" % idom)
        alldx.append(thisdx)

    nmli.setValue("dx", alldx)
    nmli.setValue("dy", alldx) # May be an issue for global WRF
    #
    # Compute the time step. 
    #
    if timestep_dxfactor.startswith("manual:"):
        nmli.setValue("time_step", int(timestep_dxfactor[7:]))
    elif timestep_dxfactor.startswith("adaptive:"):
        nmli.setValue("use_adaptive_time_step", ".true.", "domains")
    else:
        nmli.setValue("time_step",
            get_time_step(nmli.getValue("dx")[0], eval(timestep_dxfactor)))
    nmli.setValue("restart", chunk_is_restart)
    #
    #  Trim, check, overwrite the file and ... we are done!
    #
    #nmli.trimMaxDom()
    nmli.wrfCheck()
    #nmli.extendMaxDomVariables()
    nmli.overWriteNamelist()
