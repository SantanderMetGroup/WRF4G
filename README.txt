
  ====================================================================
  ====================================================================
                             WRF4G README
  ====================================================================
  ====================================================================

CONTENTS
--------

 o Execution tree
 o Usage
 o WRF4G.sh logic

EXECUTION TREE
--------------

  wrf4g_submitter.sh             <- wrf.input, wrf4g.conf
    `-- wrf4g_submit.[JOB_TYPE]
          `-- WRF4G_ini.sh
                `--WRF4G.sh

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
