import drm4g.managers.slurm 
from string import Template
from drm4g.managers import sec_to_H_M_S
import re

__version__ = '0.1'
__author__  = 'Carlos Blanco'
__revision__ = "$Id:$"

# The programs needed by these utilities. If they are not in a location
# accessible by PATH, specify their location here.
MSUB    = 'msub'   #submit a job
SQUEUE  = 'squeue'   #show status of jobs
SCANCEL = 'scancel'  #delete a job

class Resource (drm4g.managers.slurm.Resource):
    
    def lrmsProperties(self):
        return ('NEPTUNO', 'NEPTUNO')

class Job (drm4g.managers.slurm.Job):
    
    def jobSubmit(self, pathScript):
        out, err = self.Communicator.execCommand('%s %s' % (MSUB, pathScript))
        if out:
            return out.split()[0]
        else:
            raise drm4g.managers.JobException(' '.join(err.split('\n')))        

    def jobTemplate(self, parameters):
        args  = '#!/bin/bash\n'
        args += '#MOAB -N JID_%s\n' % (parameters['environment']['GW_JOB_ID'])
        args += '#MOAB -o $stdout\n'
        args += '#MOAB -e $stderr\n'
        args += '#MOAB -l nodes=$count\n'
        args += '#MOAB -V'
        args += ''.join(['export %s=%s\n' % (k, v) for k, v in parameters['environment'].items()])
        args += 'cd $directory\n'
        args += '$executable\n'
        return Template(args).safe_substitute(parameters)


