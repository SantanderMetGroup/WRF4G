__all__ = ["communicators", "core", "managers", "utils", "commands"]

__version__  = '1.0'
__author__   = 'Carlos Blanco'
__revision__ = "$Id: __init__.py 1939 2013-10-23 11:38:46Z carlos $"

import sys
from os.path import dirname , join , expandvars , exists

if sys.version_info < (2,5) and sys.version_info > (3,0):
    print 'The version number of the Python has to be > = 2.5 and < 3.0'
    sys.exit(-1)

########################################
# Default values used in DRM4G package.#
########################################
CONFIG_FILE_LOCATIONS = [
                         #expandvars("$HOME/.drm4g/etc/drm4g.conf"),
                         #expandvars("$GW_LOCATION/etc/drm4g.conf"),
                         expandvars("$HOME/.wrf4g/etc/drm4g.conf"),
                         expandvars("$WRF4G_LOCATION/etc/drm4g.conf"),
                         ]
for file in CONFIG_FILE_LOCATIONS :
    if exists(file) :
        DRM4G_DIR         = dirname ( dirname(file) )
        FILE_LOGGER       = join(DRM4G_DIR, "etc" , "logger.conf")
        DRM4G_CONFIG_FILE = file
assert DRM4G_CONFIG_FILE, "dm4g.conf does not exist, please provide one"

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

STRING_DRM4G_CONF = """
# 
# Syntax
#   '#' Comments
#
# Resource sections: [resource]
#
# If you want to add a resource, you have to add a [resource] section.
# The resources are featured by the value of the keys.
#
# Common keys for all resources:
#
#    * enable:       True or False in order to enable or disable a resource
#
#    * communicator:  
#        - local:    The resource will be accessed directly
#        - ssh:      The resource will be accessed through ssh protocol
#    * username:     username to log on the front-end 
#    * frontend:     Front-end of the cluster. The syntax is "host:port", by default the port is 22. 
#    * private_key:  Private key identity file to log on the front-end. Otherwise, you can delegate
#                    your private keys in the ssh-agent and DRM4G will load from it
#
#    * lrms: 
#        - pbs:      TORQUE/OpenPBS cluster
#        - sge:      Grid Engine cluster 
#        - slurm:    SLURM cluster
#        - lsf:      LSF cluster
#        - fork:     SHELL
#        - cream:    CREAM Compute Elements (CE)
#
# Keys for Non-Grid resources such as HPC resources:
#
#    * queue:        Queue available on the resource. If there are several, you have to use a "," as follows  
#                    "queue  = short,medium,long"
#    * ncores:       Number of cores provided by the resource
#    * parallel_env: It defines the parallel environments available for Grid Engine cluster
#    * project:      It specifies the project variable and is only for TORQUE/OpenPBS and Grid Engine clusters
#
# Keys for Grid resources:
# 
#    * vo:             Virtual Organization (VO) name
#    * bdii:           It indicates the BDII host to be used. The syntax is "bdii:port"
#    * myproxy_server: Server to store the Grid credentials.
#
# Samples:
#
# By DEFAULT, DRM4G is going to use the local machine as 'fork' lrms:
#
[localhost]
enable       = True
communicator = local
frontend     = localhost
lrms         = fork
ncores       = 1

# PBS cluster, accessed through ssh protocol:
#
#[meteo]
#enable       = True
#communicator = ssh
#username     = user
#frontend     = ui.macc.unican.es
#private_key  = ~/.ssh/id_rsa
#lrms         = pbs
#queue        = estadistica
#ncores       = 1

# Virtual Organizations, accessed through user interface: 
#
#[esr]
#enable         = True
#communicator   = ssh
#username       = user
#frontend       = ui.macc.unican.es
#private_key    = ~/.ssh/id_rsa
#lrms           = cream
#vo             = esr
#bdii           = bdii.grid.sara.nl:2170
#myproxy_server = px.grid.sara.nl

#[ngi]
#enable         = False
#communicator   = ssh
#username       = user
#frontend       = ui.macc.unican.es
#private_key    = ~/.ssh/id_rsa
#lrms           = cream
#vo             = earth.vo.ibergrid.eu
#bdii           = topbdii.egi.cesga.es:2170
#myproxy_server = myproxy.egi.cesga.es
"""

STRING_LOGGER = """
# Configuration file for DRM4G logging
#
# For more information about logging configuration directives, you should check out 
#       http://docs.python.org/2/library/logging.config.html
# 
# Additionally, %(DRM4G_DIR)s variable indicates where DRM4G configuration resides

[loggers]
keys=root,drm4gIm,drm4gEm,drm4gTm,drm4gConfigure,drm4gManager,drm4gCommunicator

[handlers]
keys=handDrm4gIm,handDrm4gEm,handDrm4gTm,handConfigure,handManager,handCommunicator

[formatters]
keys=form01

[logger_root]
handlers=

[logger_drm4gIm]
handlers=handDrm4gIm
level=DEBUG
qualname=drm4g.core.im_mad

[logger_drm4gEm]
handlers=handDrm4gEm
level=DEBUG
qualname=drm4g.core.em_mad

[logger_drm4gTm]
handlers=handDrm4gTm
level=DEBUG
qualname=drm4g.core.tm_mad

[logger_drm4gConfigure]
handlers=handConfigure
level=DEBUG
qualname=drm4g.core.configure

[logger_drm4gManager]
handlers=handManager
level=DEBUG
qualname=drm4g.managers

[logger_drm4gCommunicator]
handlers=handCommunicator
level=DEBUG
qualname=drm4g.communicators

[handler_handDrm4gIm]
class=handlers.RotatingFileHandler
level=DEBUG
formatter=form01
args=('%(DRM4G_DIR)s/var/drm4g_im.log','w',5000000,4)

[handler_handDrm4gEm]
class=handlers.RotatingFileHandler
level=DEBUG
formatter=form01
args=('%(DRM4G_DIR)s/var/drm4g_em.log','w',5000000,4)

[handler_handDrm4gTm]
class=handlers.RotatingFileHandler
level=DEBUG
formatter=form01
args=('%(DRM4G_DIR)s/var/drm4g_tm.log','w',5000000,4)

[handler_handConfigure]
class=handlers.RotatingFileHandler
level=DEBUG
formatter=form01
args=('%(DRM4G_DIR)s/var/drm4g_configure.log','w',5000000,4)

[handler_handManager]
class=handlers.RotatingFileHandler
level=DEBUG
formatter=form01
args=('%(DRM4G_DIR)s/var/drm4g_manager.log','w',5000000,4)

[handler_handCommunicator]
class=handlers.RotatingFileHandler
level=DEBUG
formatter=form01
args=('%(DRM4G_DIR)s/var/drm4g_communicator.log','w',5000000,4)

[formatter_form01]
format=%(asctime)s %(levelname)-9s %(name)-8s %(message)s
"""
