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
Get status and history and cancel jobs.

Usage: 
    wrf4g job list    [ --dbg ] [ --delay=<seconds> ] [ <job_id> ] 
    wrf4g job cancel  [ --dbg ] [ --hard ] <job_id>  
    wrf4g job log     [ --dbg ] <job_id>
    wrf4g job history [ --dbg ] <job_id> 
   
Arguments:
   <job_id>               Job identifier.

Options:
   --dbg                  Debug mode.
   --delay=<seconds>      Refresh experiment information every delay seconds.
   --hard                 Remove jobs from without synchronizing.
    
Commands:
   list                   Monitor jobs previously submitted.
   cancel                 Cancel jobs.
   log                    Keep track of a job.
   history                Get information about the execution history of a job.

Job field information:
   JID                    Job identification.
   DM                     Dispatch Manager state, one of: 
                                pend, hold, prol, prew, wrap, epil, canl, stop, migr, done, fail.
   EM                     Execution Manager state: pend, susp, actv, fail, done.
   START                  The time the job entered the system.
   END                    The time the job reached a final state (fail or done).
   EXEC                   Total execution time, includes suspension time in the remote queue system.
   XFER                   Total file transfer time, includes stage-in and stage-out phases.
   EXIT                   Job exit code.
   TEMPLATE               Filename of the job template used for this job.
   HOST                   Hostname where the job is being executed.
   HID                    Host identification.
   PROLOG                 Total prolog (file stage-in phase) time.
   WRAPPER                Total wrapper (execution phase) time.
   EPILOG                 Total epilog (file stage-out esphase) time.
   MIGR                   Total migration time.
   REASON                 The reason why the job left this host.
   QUEUE                  Queue name. 
"""

import logging
import sys
import time
from os.path                import join, exists
from drm4g.commands         import Daemon
from wrf4g.utils.command    import cls
from wrf4g.utils.gridwaylib import GWJob

def run( arg ) :
    logging.basicConfig( format = '%(message)s',
                         level  = logging.DEBUG if arg[ '--dbg' ] else logging.INFO,
                         stream = sys.stdout )
    try :
        daemon = Daemon( )
        if not daemon.is_alive() :
            raise Exception( 'DRM4G is stopped. ')
        gw_job = GWJob()
        if arg['list']:
            if not arg[ '--delay' ] :
                gw_job.list( None if not arg['<job_id>'] else arg['<job_id>'] )   
            else :
                try:
                    while True :
                        cls()
                        gw_job.list( None if not arg['<job_id>'] else arg['<job_id>'] )
                        time.sleep( int( arg[ '--delay' ] ) )
                except KeyboardInterrupt :
                    pass
        elif arg['history']:
            gw_job.history( arg['<job_id>'] )
        elif arg['log']:
            gw_job.log( arg['<job_id>'] )
        else :
            gw_job.kill( arg['<job_id>'], arg[ '--hard' ] )
    except KeyboardInterrupt :
        pass
    except Exception as err :
        logging.error( str( err ) )
