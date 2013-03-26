import drm4g.managers 
from string import Template
import xml.dom.minidom
import re
import time

__version__ = '0.1'
__author__  = 'Carlos Blanco'
__revision__ = "$Id: marenostrum.py 1359 2012-01-16 10:11:25Z carlos $"

# The programs needed by these utilities. If they are not in a location
# accessible by PATH, specify their location here.
MNSUBMIT = 'LANG=POSIX mnsubmit' #mnsubmit - submits a job script to the queue system 
MNCANCEL = 'LANG=POSIX mncancel' #mncancel - removes his/her job from the queue system, canceling the execution of the job if it was already running
MNQ      = 'LANG=POSIX mnq'      #mnq      - shows all the jobs submitted

class Resource (drm4g.managers.Resource):

    def lrmsProperties(self):
        return ('ALTAMIRA', 'ALTAMIRA')
     
    def queueProperties(self, queueName):
        queue              = drm4g.managers.Queue()
        queue.Name         = queueName
        queue.Nodes        = self.TotalCpu
        queue.FreeNodes    = self.FreeCpu
        queue.DispatchType = 'batch'
        return queue

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
        re_job_id = re.compile(r'Submitted batch job (\d*)').search(err)
        if re_job_id:
            return re_job_id.group(1)
        else:
            raise drm4g.managers.JobException(' '.join(err.split('\n')))

    def jobStatus(self):
        out, err = self.Communicator.execCommand('%s -j %s -h' % (MNQ, self.JobId))
        if err:
            return 'UNKNOWN'
        elif not out: 
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
        args += '# @ initialdir = $directory\n'
        args += '# @ output = $stdout\n'
        args += '# @ error  = $stderr\n'
        args += '# @ total_tasks = $count\n'
        if parameters.has_key('ppn'):
            args += '# @ tasks_per_node =$ppn\n'
        if parameters.has_key('maxWallTime'):
            walltime = parameters['maxWallTime']
        else:
            walltime = self.walltime_default
        args += '# @ wall_clock_limit = %s\n' % (walltime)
        args += ''.join(['export %s=%s\n' % (k, v) for k, v in parameters['environment'].items()])
        args += '$executable\n'
        return Template(args).safe_substitute(parameters)

