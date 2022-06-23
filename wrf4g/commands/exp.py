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

"""
Manage WRF4G experiments. 
    
Usage: 
    wrf4g exp list                [ --pattern=<name> ]
    wrf4g exp <name> define       [ --dbg ] [ --force ]   [ --from-template=<name> ] [ --dir=<directory> ] 
    wrf4g exp <name> edit         [ --dbg ] 
    wrf4g exp <name> create       [ --dbg ] [ --dry-run ] [ --dir=<directory> ]
    wrf4g exp <name> update       [ --dbg ] [ --dry-run ] 
    wrf4g exp <name> submit       [ --dbg ] [ --dry-run ] [ --priority=<value> ] [ --pattern=<name> ] [ --rea-state=<state> ] [ --rerun ]  [ --wps-only ] [ --mode=<value> ] 
    wrf4g exp <name> status       [ --dbg ] [ --pattern=<name> ] [ --rea-state=<state> ] [ --delay=<seconds> ] [ --show-chunks ]
    wrf4g exp <name> statistics   [ --pattern=<name> ]
    wrf4g exp <name> cancel       [ --dbg ] [ --dry-run ] [ --pattern=<name> ] [ --rea-state=<state> ] [ --hard ]
    wrf4g exp <name> set-priority [ --dbg ] [ --dry-run ] [ --pattern=<name> ] <priority>
    wrf4g exp <name> delete       [ --dbg ] [ --dry-run ] 
   
Options:
    --dbg                     Debug mode.
    -n --dry-run              Dry run.
    -f --force                Force to remove if it exists.
    -P --priority=<value>     Fix-priority for scheduling [default: 0]. 
    -p --pattern=<name>       Pattern to find experiments and realizations.
    -s --rea-state=<state>    Select only realizations in the indicated state. Available states :
                              PREPARED, SUBMITTED, RUNNING, PENDING, FAILED and FINISHED 
    -t --from-template=<name> Experiment template, avaible templates are default, single, physics. 
    -d --dir=<directory>      Directory to create or start an experiment [default: ./].
    --delay=<seconds>         Refresh experiment information every delay seconds.
    --rerun                   Force to run although this realization or experiment has finished.
    --hard                    Remove jobs from without synchronizing.
    -m --mode=<value>         0: run whole workflow, 1: only WPS and real, 2: only WRF [default: 0]
    --show-chunks                  Show advanced information about the chunks status
  
Commands:
    list                      Show all the experiments available.
    define                    Create the files needed to define a WRF4G experiment.
    edit                      Edit experiment.wrf4g file.
    create                    Given experiment.wrf4g file, prepare the experiment creating 
                              the realizations and chunks needed.
    update                    Update the experiment configuration.
    submit                    Submit the experiment.
    status                    Check the status of realizations and chunks showing computing resources, 
                              job identifier and exit codes (SEE EXIT CODES) 
    cancel                    Cancel the active realizations by killing their jobs.
    set-priority              Change the scheduling priority of any job releted to a realization. 
                              The priority must be in range [0,20], and the default value is 0. 
                              When a job gets a priority of 20, it becomes an urgent job. This job 
                              is dispatched as soon as possible passing all the scheduling policies.
    delete                    Remove the experiment from the database.

EXIT CODES
    1  : Error creating log directory        
    2  : Error copying apps            
    3  : Error app type does not exist            
    4  : Error executing source script       
    5  : Job already executed 
    6  : Error creating directory to simulate
    7  : Error finding WRF binaries 
    8  : Error copying restart files        
    9  : There is a mismatch in the restart date   
    10 : Error copying files    
    11 : Error downloading WPS files    
    12 : Error copying boundaries           
    13 : Error modifying namelist
    14 : Error executing PREPROCESSOR
    15 : Error linking GRIB files     
    16 : Error executing UNGRIB
    17 : Error executing METGRID       
    18 : Error executing REAL
    19 : Error uploading WPS files      
    20 : Error executing WRF
    21 : Error executing POSTPROCESSOR 
    22 : Error copying output file    
    23 : Job killed by the system 
    255: Unexpected error
"""

import sys
import time
import logging
import shutil
from sqlalchemy.orm.exc   import NoResultFound
from sqlalchemy.exc       import OperationalError
from wrf4g.db             import get_session
from wrf4g.core           import Experiment
from wrf4g.utils.command  import cls
from wrf4g.utils.time     import datetime2datewrf

def run( arg ) :
    logging.basicConfig( format = '%(message)s', 
                         level  = logging.DEBUG if arg[ '--dbg' ] else logging.DEBUG,  
                         stream = sys.stdout )
    if arg[ 'define' ] :
        Experiment.create_files( arg[ '<name>' ], 
                                 "default" if not arg[ '--from-template' ] else arg[ '--from-template' ], 
                                 arg[ '--force' ], arg[ '--dir' ] )
    else :
        # create a session to connect with the database
        session = get_session()
        try :
            if arg[ 'list' ] :
                l_exps = session.query( Experiment )
                if arg[ '--pattern' ] :
                    l_exps  = l_exps.filter( Experiment.name.like( arg[ '--pattern' ].replace('*','%') ) )
                if not l_exps.all() :
                    logging.info( "There are not experiments" )
                else :
                    # Header
                    logging.info( "\033[1;4m%-40s\033[0m" % ( "EXPERIMENT" ) )
                    for e in l_exps :
                        logging.info("%-s" % ( e.name ) ) 
            else :
                try :
                    exp = session.query( Experiment ).\
                            filter_by( name = arg[ '<name>' ] ).one()
                    exp.dryrun = arg[ '--dry-run' ]
                except NoResultFound :
                    if arg[ 'create' ] :
                        exp2        = Experiment()
                        exp2.name   = arg[ '<name>' ]
                        exp2.dryrun = arg[ '--dry-run' ]
                        exp2.prepare( update = False, directory = arg[ '--dir' ] )
                        session.add( exp2 )
                    else :
                        raise Exception( "'%s' experiment does not exist" % arg[ '<name>' ] )
                else :
                    # Options 
                    if arg[ 'edit' ] :
                        exp.edit( )
                    if arg[ 'update' ] :
                        exp.prepare( update = True )
                    elif arg[ 'submit' ] :
                        if arg['--wps-only']:
                            exp.run_wps( 
                                rerun           = arg[ '--rerun' ],
                                priority        = int( arg[ '--priority' ] ), 
                                )
                        else:
                            exp.run( arg[ '--rerun' ], 
                                 arg[ '--pattern' ], 
                                 arg[ '--rea-state' ],
                                 int( arg[ '--priority' ] )
                                 )
                    elif arg[ 'status' ] :
                        if not arg[ '--delay' ] :
                            exp.get_status( arg[ '--pattern' ], arg[ '--rea-state' ],arg['--show-chunks'] )
                        else :
                            try:
                                while True :
                                    cls()
                                    exp.get_status( arg[ '--pattern' ], arg[ '--rea-state' ] )
                                    time.sleep( int( arg[ '--delay' ] ) )
                            except KeyboardInterrupt : 
                                pass
                    elif arg[ 'cancel' ] :
                        exp.cancel( arg[ '--pattern' ], arg[ '--rea-state' ], arg[ '--hard' ] )
                    elif arg[ 'set-priority' ] :
                        exp.set_priority( arg[ '--pattern' ], int( arg[ '<priority>' ] ) )
                    elif arg[ 'statistics' ] :
                        exp.statistics( arg[ '--pattern' ] )
                    elif arg[ 'delete' ] :
                        exp.delete( )
                        session.delete( exp )
                        logging.info( "'%s' experiment has been deleted from the database" % exp.name )
                    elif arg[ 'create' ] :
                        logging.info( "'%s' experiment already exists" % exp.name )
                        
            if arg[ '--dry-run' ] :
                session.rollback()
            else :
                session.commit()
                if arg[ 'submit' ] :
                    if arg["--wps-only"]:
                        exp.release_wps()
                    else:
                        exp.release()
                    session.commit()
        except OperationalError as err :
            logging.error( err.message )
        except KeyboardInterrupt :
            session.rollback()
        except Exception as err :
            session.rollback()
            #logging.error( str( err ) )
            session.close()
            exit(str(err))
        finally:
            session.close()
