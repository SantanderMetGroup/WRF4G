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
Manage WRF4G realizations. 
    
Usage: 
     wrf4g rea <name> submit       [ --dbg ] [ --dry-run ] [ --priority=<value> ] [ --rerun ] [ --wps-only ] [ --mode=<value> ] [ <first_ch> [ <last_ch> ] ]
     wrf4g rea <name> status       [ --dbg ] [ --delay=<seconds> ] [ --verbose ] [ --show-chunks ]
     wrf4g rea <name> info
     wrf4g rea <name> log          [ --dbg ] [ --dir=<directory> ] <chunk_id>
     wrf4g rea <name> set-priority [ --dbg ] [ --dry-run ] <priority>
     wrf4g rea <name> cancel       [ --dbg ] [ --dry-run ] [ --hard ]
     wrf4g rea <name> set-restart  [ --dbg ] <date>
     wrf4g rea <name> get-restart  
   
Options:
    --dbg                  Debug mode.
    -n --dry-run           Dry run.
    --rerun                Force to run although the realization has finished.
    --wps-only             Only run wps, not wrf 
    -P --priority=<value>  Fix-priority for scheduling [default: 0].
    --delay=<seconds>      Refresh experiment information every delay seconds.    
    -d --dir=<directory>   Directory to unpack log files [default: ./].
    --hard                 Remove jobs from without synchronizing.
    -m --mode=<value>      0 (default): run whole workflow, 1: only WPS and real, 2: only WRF [default: 0]
    --show-chunks               Show advanced information about the chunks status
  
Commands:
    submit                 Submit the realization.       
    status                 Check the status of a realization showing computing resources, 
                           job identifier and exit codes (SEE EXIT CODES).
    info                   Get information about configuration.
    log                    Get log files from a chunk.
    set-priority           Change the scheduling priority of any job releted to the realization. 
                           The priority must be in range [0,20], and the default value is 0. 
                           When a job gets a priority of 20, it becomes an urgent job, and it is 
                           dispatched as soon as possible passing all the scheduling policies. 
    cancel                 Cancel the realization by killing its jobs.
    set-restart            Modify realization restart date.
    get-restart            Show realization restart date.

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

import logging
import sys
import time
from wsgiref.handlers import read_environ
from sqlalchemy.orm.exc   import NoResultFound
from sqlalchemy.exc       import OperationalError
from wrf4g.utils.command  import cls
from wrf4g.db             import get_session
from wrf4g.core           import Realization

def run( arg ) :
    logging.basicConfig( format = '%(message)s',
                         level  = logging.DEBUG if arg[ '--dbg' ] else logging.INFO,
                         stream = sys.stdout )
    # create a session to connect with the database
    session = get_session()
    try :
        try :
            rea = session.query( Realization ).\
                    filter( Realization.name   == arg[ '<name>' ], 
                            Realization.exp_id != None ).one()
            rea.dryrun = arg[ '--dry-run' ]
        except NoResultFound :
            raise Exception( "'%s' realization does not exist" % arg[ '<name>' ] )
        else :
            if arg[ 'submit' ] :
                if arg['--wps-only']:
                    rea.run_wps(first_chunk_run = arg[ '<first_ch>' ], 
                            last_chunk_run  = arg[ '<last_ch>' ], 
                            rerun           = arg[ '--rerun' ],
                            priority        = int( arg[ '--priority' ] ), 
                            )
                else:
                    rea.run( first_chunk_run = arg[ '<first_ch>' ], 
                            last_chunk_run  = arg[ '<last_ch>' ], 
                            rerun           = arg[ '--rerun' ],
                            priority        = int( arg[ '--priority' ] ) 
                            )
            elif arg[ 'status' ] :
                if arg['--show-chunks']: 
                    showchunks=1
                else:
                    showchunks=0
                if not arg[ '--delay' ] :
                    if showchunks == 0:
                        rea.status_header( )
                    else:
                        rea.status_chunk_header()

                    rea.get_status(showchunks)
                else :
                    try:
                        while True :
                            cls()
                            rea.status_header()
                            rea.get_status(showchunks)
                            time.sleep( int( arg[ '--delay' ] ) )
                    except KeyboardInterrupt :
                        pass
            elif arg[ 'log' ] :
                rea.get_log( arg[ '<chunk_id>' ], arg[ '--dir' ] )
            elif arg[ 'info' ] :
                rea.information()
            elif arg[ 'set-priority' ] :
                rea.set_priority( int( arg[ '<priority>' ] ) )
            elif arg[ 'set-restart' ] :
                rea.set_restart( arg[ '<date>' ] )
            elif arg[ 'get-restart' ] :
                rea.get_restart( )
            else :
                rea.cancel( arg[ '--hard' ] )
            if arg[ '--dry-run' ] :
                session.rollback()
            else :
                session.commit()
                if arg[ 'submit' ] :
                    if arg["--wps-only"]:
                        rea.release_wps()
                    else:
                        rea.release()
                    session.commit()
                
    except OperationalError as err :
        logging.error( err.message )
    except KeyboardInterrupt :
        session.rollback()
    except Exception as err :
        session.rollback()
        logging.error( str( err ) )
    finally:
        session.close()
