import drm4g.managers 
from string import Template
import xml.dom.minidom
import re
import time

__version__  = '2.4.1'
__author__   = 'Carlos Blanco'
__revision__ = "$Id: slurm_res.py 2811 2015-09-22 11:33:32Z carlos $"

# The programs needed by these utilities. If they are not in a location
# accessible by PATH, specify their location here.
MNSUBMIT = 'LANG=POSIX mnsubmit' #mnsubmit - submits a job script to the queue system 
MNCANCEL = 'LANG=POSIX mncancel' #mncancel - removes his/her job from the queue system, canceling the execution of the job if it was already running
MNQ      = 'LANG=POSIX mnq'      #mnq      - shows all the jobs submitted

class Resource (drm4g.managers.Resource):
    pass

class Job (drm4g.managers.Job):
    
    #clock_wall_time is mandatory
    walltime_default = '3600' # 1 hours 
    #mn job status <--> GridWay job status
    states_altamira = {
                  'CANCELLED': 'DONE',
                  'COMPLETED' : 'DONE',
                  'COMPLETING': 'ACTIVE',
                  'RUNNING'   : 'ACTIVE',
                  'NODE_FAIL' : 'FAILED',
                  'FAILED'    : 'FAILED',
                  'PENDING'   : 'PENDING',
                  'SUSPENDED' : 'SUSPENDED',
                  'TIMEOUT'   : 'FAILED',
                }                 
    
    def jobSubmit(self, pathScript):
        out, err = self.Communicator.execCommand('%s %s' % (MNSUBMIT, pathScript))
        re_job_id = re.compile(r'Submitted batch job (\d*)').search(out)
        if re_job_id:
            return re_job_id.group(1)
        else:
            raise drm4g.managers.JobException(' '.join(err.split('\n')))

    def jobStatus(self):
        out, err = self.Communicator.execCommand('%s -j %s -h' % (MNQ, self.JobId))
        if err or not out: 
            return 'DONE'
        else:
            state = out.split()[3]
            return self.states_altamira.setdefault(state, 'UNKNOWN')
    
    def jobCancel(self):
        out, err = self.Communicator.execCommand('%s %s' % (MNCANCEL, self.JobId))
        if err:
            raise drm4g.managers.JobException(' '.join(err.split('\n')))

    def jobTemplate(self, parameters):
        args  = '#!/bin/bash\n'
        args += '# @ job_name = JID_%s\n' % (parameters['environment']['GW_JOB_ID'])
        args += '# @ initialdir = .\n'
        args += '# @ output = $stdout\n'
        args += '# @ error  = $stderr\n'
        args += '# @ total_tasks = $count\n'
        if 'ppn' in parameters :
            args += '# @ tasks_per_node =$ppn\n'
        if 'maxWallTime' in parameters :
            walltime = parameters['maxWallTime']
        else:
            walltime = self.walltime_default
        args += '# @ wall_clock_limit = %s\n' % (walltime)
        args += ''.join(['export %s=%s\n' % (k, v) for k, v in list(parameters['environment'].items())])
        args += '\n'
        args += '$executable\n'
        return Template(args).safe_substitute(parameters)

