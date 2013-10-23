__all__ = ["communicators", "core", "managers", "utils", "commands"]

__version__  = '1.0'
__author__   = 'Carlos Blanco'
__revision__ = "$Id: __init__.py 1935 2013-10-16 07:39:08Z carlos $"

import sys
from os.path import dirname , join , expandvars , exists

if sys.version_info < (2,5) and sys.version_info > (3,0):
    print 'The version number of the Python has to be > = 2.5 and < 3.0'
    sys.exit(-1)

########################################
# Default values used in DRM4G package.#
########################################
CONFIG_FILE_LOCATIONS = [
                         expandvars("$HOME/.drm4g/etc/drm4g.conf"),
                         expandvars("$GW_LOCATION/etc/drm4g.conf"),
                         expandvars("$HOME/.wrf4g/etc/drm4g.conf"),
                         expandvars("$WRF4G_LOCATION/etc/drm4g.conf"),
                         ]
for file in CONFIG_FILE_LOCATIONS :
    if exists(file) :
        DRM4G_DIR         = dirname ( dirname(file) )
        FILE_LOGGER       = join(DRM4G_DIR, "etc" , "logger.conf")
        DRM4G_CONFIG_FILE = file
assert DRM4G_CONFIG_FILE, "dm4g.conf does not exist, please provide one"

REMOTE_JOBS_DIR = "~/.drm4g/jobs"
REMOTE_VOS_DIR  = "~/.drm4g/security"
    
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
                     "pbs"     : "drm4g.managers.pbs",
                     "sge"     : "drm4g.managers.sge",
                     "fork"    : "drm4g.managers.fork",
                     "none"    : "drm4g.managers.fork",
                     "cream"   : "drm4g.managers.cream",
                     "slurm"   : "drm4g.managers.slurm",
                     "mnslurm" : "drm4g.managers.marenostrum",
                     "altamira": "drm4g.managers.altamira",
                     "neptuno" : "drm4g.managers.neptuno",
                     }

    
