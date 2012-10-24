import drm4g.managers 
from string import Template
import xml.dom.minidom
import re
import time
from drm4g.managers import sec_to_H_M_S

__version__ = '0.1'
__author__  = 'Carlos Blanco'
__revision__ = "$Id: marenostrum.py 1359 2012-01-16 10:11:25Z carlos $"

# The programs needed by these utilities. If they are not in a location
# accessible by PATH, specify their location here.
MNSUBMIT = 'LANG=POSIX mnsubmit' #mnsubmit - submits a job script to the queue system 
MNCANCEL = 'LANG=POSIX mncancel' #mncancel - removes his/her job from the queue system, canceling the execution of the job if it was already running
MNQ      = 'LANG=POSIX mnq'      #mnq      - shows all the jobs submitted

class Resource (drm4g.managers.Resource):
    MAX_RESOURCES = 1000

    def lrmsProperties(self):
        return ('ALTAMIRA', 'ALTAMIRA')

    def dynamicNodes(self):
        out, err = self.Communicator.execCommand('%s -h | wc -l' % (MNQ))
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
    
    #clock_wall_time is mandatory
    walltime_default = '3600' # 1 hours 
    #mn job status <--> GridWay job status
    states_mn = {
                  'PENDING' : 'PENDING',
                  'RUNNING' : 'ACTIVE',    
                }                 
    
    def jobSubmit(self, path_script):
        out, err = self.Communicator.execCommand('%s %s' % (MNSUBMIT, path_script))
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
            out_parser = xml.dom.minidom.parseString(out)
            state = out.split()[3]
            return self.states_mn.setdefault(state, 'UNKNOWN')
    
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
        if parameters.has_key('tasksPerNode'):
            args += '# @ tasks_per_node =$tasksPerNode\n'
        if parameters.has_key('maxWallTime'):
            walltime = sec_to_H_M_S(parameters['maxWallTime'])
        else:
            walltime = self.walltime_default
        args += '# @ wall_clock_limit = %s\n' % (walltime)
        args += ''.join(['export %s=%s\n' % (k, v) for k, v in parameters['environment'].items()])
        if parameters['jobType'] == "mpi":
            args += 'srun $executable\n'
        else:
            args += '$executable\n'
        return Template(args).safe_substitute(parameters)

