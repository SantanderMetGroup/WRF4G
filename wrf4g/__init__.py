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
from os.path      import dirname, join, abspath, exists

__version__  = '2.3.0'
__author__   = 'Carlos Blanco'
__revision__ = "$Id$"

HOME                 = os.environ.get( 'HOME' )
WRF4G_DIR            = os.environ[ 'GW_LOCATION' ] = join( os.environ.get( 'WRF4G_DIR', HOME ), '.wrf4g' )
WRF4G_DEPLOYMENT_DIR = os.path.dirname(os.path.realpath(__file__))
DB4G_CONF            = os.environ.get( 'DB4G_CONF',  join( WRF4G_DIR , 'etc' , 'db.conf' ) )
WRF4G_LOGGER         = join( WRF4G_DIR, 'etc', 'logger.conf')

import wrf4g.orm
