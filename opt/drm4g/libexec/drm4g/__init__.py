__all__ = ["communicators", "core", "managers", "utils", "commands"]

__version__  = '2.3.1'
__author__   = 'Carlos Blanco'
__revision__ = "$Id: __init__.py 2352 2015-02-24 10:23:57Z carlos $"

import sys
import os
import logging.config
from os.path import dirname , join , expandvars , exists , abspath

if sys.version_info < (2,5) and sys.version_info > (3,0):
    exit( 'The version number of the Python has to be > = 2.5 and < 3.0' )

########################################
# Default values used in DRM4G package.#
########################################
HOME                 = os.environ.get( 'HOME' )
DRM4G_DIR            = join( os.environ.get( 'WRF4G_DIR' , HOME ), '.wrf4g' )
os.environ[ 'GW_LOCATION' ] = DRM4G_DIR
DRM4G_DEPLOYMENT_DIR = dirname( dirname( dirname( abspath( __file__ ) ) ) )
DRM4G_BIN            = join( DRM4G_DEPLOYMENT_DIR , 'bin'  ) 
DRM4G_CONFIG_FILE    = join( DRM4G_DIR , 'etc' , 'resources.conf' )
DRM4G_LOGGER         = join( DRM4G_DIR , 'etc' , 'logger.conf')
DRM4G_DAEMON         = join( DRM4G_DIR , 'etc' , 'gwd.conf')
DRM4G_SCHED          = join( DRM4G_DIR , 'etc' , 'sched.conf')

logging.basicConfig( format='%(message)s', level = logging.INFO , stream = sys.stdout )

REMOTE_JOBS_DIR = "~/.wrf4g/jobs"
REMOTE_VOS_DIR  = "~/.wrf4g/security"
    
# ssh communicator
SSH_PORT            = 22
SSH_CONNECT_TIMEOUT = 30 # seconds
SFTP_CONNECTIONS    = 3

# Proxy
PROXY_THRESHOLD     = 178 # Proxy threshold in hours.
    
COMMUNICATORS = {
                 "ssh"   : "drm4g.communicators.ssh",
                 "local" : "drm4g.communicators.local",
                 }
RESOURCE_MANAGERS = {
                     "pbs"          : "drm4g.managers.pbs",
                     "sge"          : "drm4g.managers.sge",
                     "fork"         : "drm4g.managers.fork",         
                     "none"         : "drm4g.managers.fork",
                     "lsf"          : "drm4g.managers.lsf",
                     "loadleveler"  : "drm4g.managers.loadleveler",
                     "cream"        : "drm4g.managers.cream",
                     "slurm"        : "drm4g.managers.slurm",
                     "mnslurm"      : "drm4g.managers.marenostrum",
                     "slurm_res"    : "drm4g.managers.slurm_res",
                     "neptuno"      : "drm4g.managers.neptuno",
                     }

