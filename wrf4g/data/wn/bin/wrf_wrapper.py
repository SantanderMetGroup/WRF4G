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
import sys
import logging
from wrf4g.wrapper   import launch_wrapper, JobError, PilotParams

__author__   = 'Valvanuz Fernández, Jesus Fernandez, Carlos Blanco and Antonio S. Cofiño'

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
