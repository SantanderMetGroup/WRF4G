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

__author__   = 'Valvanuz Fernández, Jesus Fernandez, Carlos Blanco and Antonio S. Cofiño'

HOME                 = os.environ.get( 'HOME' )
#WRF4G_DIR            = os.environ[ 'GW_LOCATION' ] = join( os.environ.get( 'WRF4G_DIR', HOME ), '.wrf4g' )
WRF4G_DIR            = join( os.environ.get( 'WRF4G_DIR', HOME ), '.wrf4g' )
os.environ[ 'DRM4G_DIR' ] =  join(os.environ.get( 'HOME' ), '.drm4g')
#os.environ[ 'GW_LOCATION' ]= join( os.environ.get( 'DRM4G_DIR'), '.drm4g' )
WRF4G_DEPLOYMENT_DIR = os.path.dirname(os.path.realpath(__file__))
DB4G_CONF            = os.environ.get( 'DB4G_CONF',  join( WRF4G_DIR , 'etc' , 'db.conf' ) )
WRF4G_LOGGER         = join( WRF4G_DIR, 'etc', 'logger.conf')

import wrf4g.orm
from ._version import __version__
