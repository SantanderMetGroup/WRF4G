"""
Print information about the hosts available on WRF4G.
     
Usage: 
    wrf4g host [ --dbg ] [ list ] [ <hid> ] 
    
Arguments:
    <hid>         Host identifier.

Options:
    --dbg         Debug mode.        
 
Host field information:
    HID           Host identifier.
    ARCH          Architecture.
    JOBS(R/T)     Number of jobs: R = running, T = total.
    LRMS          Local Resource Management System.
    HOSTNAME      Host name. 
    QUEUENAME     Queue name.
    WALLT         Queue wall time.
    CPUT          Queue cpu time.
    MAXR          Max. running jobs.
    MAXQ          Max. queued jobs. 
"""
__version__  = '2.2.0'
__author__   = 'Carlos Blanco'
__revision__ = "$Id$"

import logging
import sys
from drm4g                import DRM4G_BIN
from drm4g.commands       import exec_cmd, Daemon

def run( arg ) :
    logging.basicConfig( format = '%(message)s', 
                         level  = logging.DEBUG if arg[ '--dbg' ] else logging.INFO,
                         stream = sys.stdout )
    try :
        daemon = Daemon()
        if not daemon.is_alive() :
            raise Exception('DRM4G is stopped.')
        cmd = '%s/gwhost '  % ( DRM4G_BIN )
        if arg[ '<hid>' ] :
            cmd = cmd + arg[ '<hid>' ]
        out , err = exec_cmd( cmd )
        logging.info( out )
        if err :
            logging.info( err )
    except KeyboardInterrupt :
        pass
    except Exception as err :
        logging.error( str( err ) )

