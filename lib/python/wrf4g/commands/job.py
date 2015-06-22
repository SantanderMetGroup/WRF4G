"""
Submit, get status and history and cancel jobs.

Usage: 
    wrf4g job submit  [ --dbg ] [ --dep <job_id> ... ] <template> 
    wrf4g job list    [ --dbg ] [ <job_id> ] 
    wrf4g job cancel  [ --dbg ] [ --hard ] <job_id>  
    wrf4g job log     [ --dbg ] <job_id>
    wrf4g job history [ --dbg ] <job_id> 
   
Arguments:
   <job_id>               Job identifier.
   <template>             Job template.

Options:
   --dbg                  Debug mode.
   --dep=<job_id> ...     Define the job dependency list of the job.
   --hard                 Remove jobs from without synchronizing.
    
Commands:
   submit                 Command for submitting jobs.
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
__version__  = '2.0.0'
__author__   = 'Carlos Blanco'
__revision__ = "$Id$"

import logging
import sys
from os.path                import join, exists
from drm4g.commands         import Daemon
from wrf4g.tools.gridwaylib import GWJob

def run( arg ) :
    logging.basicConfig( format = '%(message)s',
                         level  = logging.DEBUG if arg[ '--dbg' ] else logging.INFO,
                         stream = sys.stdout )
    try :
        daemon = Daemon( )
        if not daemon.is_alive() :
            raise Exception( 'DRM4G is stopped. ')
        gw_job = GWJob()
        if arg['submit']:
            gw_job.submit( dep = ' '.join( arg['--dep'] ), file_template = arg['<template>'] )
        elif arg['list']:
            gw_job.list( None if not arg['<job_id>'] else arg['<job_id>'] [ 0] )
        elif arg['history']:
            gw_job.history( arg['<job_id>'][ 0 ] )
        elif arg['log']:
            gw_job.log( arg['<job_id>'][ 0 ] )
        else :
            gw_job.kill( arg['<job_id>'][ 0 ], arg[ '--hard' ] )
    except Exception , err :
        import traceback
        traceback.print_exc(file=sys.stdout)   
        logging.error( str( err ) )
