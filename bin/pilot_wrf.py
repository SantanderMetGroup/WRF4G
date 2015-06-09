import os
import sys
import logging
from wrf4g.pilot   import launch_pilot, JobError, PilotParams

__version__  = '2.0.0'
__author__   = 'Carlos Blanco'
__revision__ = "$Id$"

if __name__ == '__main__':
    try :
        ##
        # Define variables
        ##
        params = PilotParams()
        launch_pilot( params )
    except JobError, err :
        try :
            logging.error( err.msg )
        except :
            sys.stderr.write( err.msg )
        sys.exit( err.exit_code )
    except Exception, err :
        sys.stderr.write( str( err ) )
        sys.exit( -1 )
