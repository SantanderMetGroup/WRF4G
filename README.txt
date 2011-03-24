
  ====================================================================
  ====================================================================
                             WRF4G README
  ====================================================================
  ====================================================================
hola
CONTENTS
--------

 o Execution tree
 o Usage
 o WRF4G.sh logic

EXECUTION TREE
--------------

Legend:
  <- file read
  -> file created
  << file downloaded
  >> file uploaded
  mm file modified
  ~> log to file
  [variable_defined_in_wrf4g.conf_or_wrf.input]

UI
  wrf4g_submitter.sh
    <- wrf.input
    <- wrf4g.conf
    -> wrf.chunk
    bin/fortnml
    wrf4g_submit.[JOB_TYPE]
      mm WRF4G_ini.sh
WN    
  WRF4G_ini.sh  ~>  WRF4G_ini.sh.e?????
    <- wrf.input
    <- wrf4g.conf
    <- wrf.chunk
    WRFGEL/get_date_current
    WRFGEL/get_date_restart
    << WRF4G-[WRF4G_VERSION].tar.gz
    << WRF4Gbin-[WRF_VERSION].tar.gz
    WRF4G.sh  ~>  WRF4G.log
      << (domain data)
      exist_wps
      bin/preprocessor.[global_preprocessor]
        << (input data)
      bin/ungrib.exe  ~>  ungrib_<date>.log
      bin/metgrid.exe  ~>  metgrid_<date>.log
      bin/real.exe  ~>  real_<date>.log
      >> wrfinput
      >> wrfbdy
      >> wrflowinp
      bin/wrf.exe  ~>  wrf_<date>.log
      WRFGEL/wrf4g_monitor  ~>  monitor.log
        WRFGEL/post_and_register
          bin/postprocessor.[postprocessor]
          -> TS.tar.gz
          WRFGEL/register_file  ~> wrfgel.out
            >> wrfout_*
            -> current_date.txt
            >> current_date.txt
            >> wrfrst_*
            >> wrfrain_*
            >> wrfxtrm_*
            >> TS.tar.gz

USAGE
-----

Set in your PATH the User Interface (UI) scripts:

WRF4G_ROOT=/oceano/gmeteo/users/chus/tmp/wrf4g
export PATH=$WRF4G_ROOT/ui/scripts/:$PATH

Now you have access to the commands:

  wrf4g_make_tarball_bin.sh
  wrf4g_make_tarball.sh
  wrf4g_submitter.sh 

Make a fresh directory for your experiment setup and create sample files:

mkdir myexp
cd myexp
wrf4g_create_files

Edit 'wrf4g.conf' to set the location of your input data, etc. Do not
forget to set the appropriate version of the binaries you need.

Edit 'wrf.input' to set up your experiment details

[Hacer . ~/eelaprod.sh]

Run the experiment with:

wrf4g_submitter.sh

WRF4G.sh LOGIC
--------------

if exists_wps
  wps_ran=0
  get_files_wpsout
else
  wps_ran=0
  run_wps
  run_real
  file_register wps
fi

if not wps_ran
  get_date_restart
  if not restart_date
    set restart false or exit
  elif restart_date >= end_date
    exit
  elif restart_date < start_date
    exit
  else
    get_files_restart
    set restart true
  fi
  setup_namelist restart_date
else
  if chunk_is_restart
    get_files_restart
  fi
  set restart true/false
fi

run_wrf
