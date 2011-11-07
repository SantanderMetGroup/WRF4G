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
    "mnslurm" : "drm4g.managers.marenostrum",
    "fork"    : "drm4g.managers.fork",
    "none"    : "drm4g.managers.fork",
    }

#########
# HOSTS #
#########

PATH_HOST = r"etc/hosts_drm4g.list"


########
# DEBUG#
########

Debug = False  #To enable debug DEBUG = True. The Working Directory won't be removed by DRM4G 

