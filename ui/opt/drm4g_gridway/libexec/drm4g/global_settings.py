##############
# MIDDLEWARE #
##############

COMMUNICATOR = {
    "ssh"     : "drm4g.communicators.ssh",
    "local"   : "drm4g.communicators.local",
    }
                  
RESOURCE_MANAGER = {
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

#########
# HOSTS #
#########

#File to parser
PATH_HOSTS = __import__('os').environ['GW_LOCATION'] + "/../../etc/framework4g.conf"

#Section to parser
HOST_SECTION = "ComputingResources"

##########
# LOGGER #
##########

PATH_LOGGER = __import__('os').environ['GW_LOCATION'] + "/../../etc/logger.conf"