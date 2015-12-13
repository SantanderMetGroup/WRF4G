import os
import sys
import logging
from wrf4g.wrapper   import launch_wrapper, JobError, PilotParams

__version__  = '2.1.0'
__author__   = 'Carlos Blanco'
__revision__ = "$Id$"

if __name__ == '__main__':
    try :
        ##
        # Define variables
        ##
        params = PilotParams()
        launch_wrapper( params )
    except JobError as err :
        try :
            logging.error( err.msg )
        except :
            sys.stderr.write( err.msg )
        sys.exit( err.exit_code )
    except Exception as err :
        sys.stderr.write( str( err ) )
        sys.exit( -1 )
