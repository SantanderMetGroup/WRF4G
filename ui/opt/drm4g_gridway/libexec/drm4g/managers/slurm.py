import drm4g.managers 
from string import Template
from drm4g.managers import sec_to_H_M_S
import re

__version__ = '0.1'
__author__  = 'Carlos Blanco'
__revision__ = "$Id$"

# The programs needed by these utilities. If they are not in a location
# accessible by PATH, specify their location here.
SBATCH  = 'sbatch'   #submit a job
SQUEUE  = 'squeue'   #show status of jobs
SCANCEL = 'scancel'  #delete a job

class Resource (drm4g.managers.Resource):
    MAX_RESOURCES = 1000

    def lrmsProperties(self):
        return ('SLURM', 'SLURM')

    def dynamicNodes(self):
        out, err = self.Communicator.execCommand('%s -h | wc -l' % (SQUEUE))
        if err:
            raise drm4g.managers.ResourceException(' '.join(err.split('\n')))
        busy_cpu = int(out)
        free_cpu = self.MAX_RESOURCES - busy_cpu
        return (str(self.MAX_RESOURCES), str(free_cpu))

    def queuesProperties(self, searchQueue, project):
        queue              = drm4g.managers.Queue()
        queue.Name         = 'default'
        queue.Nodes        = self.TotalCpu
        queue.FreeNodes    = self.FreeCpu
        queue.DispatchType = 'batch' 
        return [queue]

class Job (drm4g.managers.Job):
   
    #job status <--> GridWay job status
    states_SLURM = {'CANCELLED': 'DONE', 
                  'COMPLETED' : 'DONE', 
                  'COMPLETING': 'ACTIVE',  
                  'RUNNING'   : 'ACTIVE',  
                  'NODE_FAIL' : 'FAILED',  
                  'FAILED'    : 'FAILED',
                  'PENDING'   : 'PENDING',  
                  'SUSPENDED' : 'SUSPENDED',
                  'TIMEOUT'   : 'FAILED',
                }
    
    def jobSubmit(self, path_script):
        out, err = self.Communicator.execCommand('%s %s' % (SBATCH, path_script))
        re_job_id = re.compile(r'Submitted batch job (\d*)').search(out)
        if re_job_id:
            return re_job_id.group(1)
        else:
            raise drm4g.managers.JobException(' '.join(err.split('\n')))        

    def jobStatus(self):
        out, err = self.Communicator.execCommand('squeue -h -o %T -j ' + self.JobId)
        if err:
            return 'UNKNOWN'
        elif not out:
            return 'DONE'
        else:
            return self.states_SLURM.setdefault(out.rstrip('\n'), 'UNKNOWN')
    
    def jobCancel(self):
        out, err = self.Communicator.execCommand('%s %s' % (SCANCEL, self.JobId))
        if err: 
            raise drm4g.managers.JobException(' '.join(err.split('\n')))

    def jobTemplate(self, parameters):
        args  = '#!/bin/bash\n'
        args += '#SBATCH --job-name=JID_%s\n' % (parameters['environment']['GW_JOB_ID'])
        args += '#SBATCH --output=$stdout\n'
        args += '#SBATCH --error=$stderr\n'
        if parameters.has_key('maxWallTime'): 
            args += '#SBATCH --time=%s\n' % (sec_to_H_M_S(parameters['maxWallTime']))
        if parameters.has_key('maxMemory'):
            args += '#SBATCH --mem=%s\n' % (parameters['maxMemory'])
        if parameters.has_key('tasksPerNode'): 
            args += '#SBATCH --ntasks-per-node=$tasksPerNode'
        args += '#SBATCH --ntasks=$count\n'
        args += ''.join(['export %s=%s\n' % (k, v) for k, v in parameters['environment'].items()])
        args += 'cd $directory\n'
        args += '$executable\n'
        return Template(args).safe_substitute(parameters)


