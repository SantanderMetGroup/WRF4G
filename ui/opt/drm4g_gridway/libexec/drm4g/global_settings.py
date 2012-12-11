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

PATH_HOST = r"etc/hosts_drm4g.list"

