import drm4g.managers 
from string import Template
import os

__version__ = '0.1'
__author__  = 'Carlos Blanco'
__revision__ = "$Id: fork.py 1763 2013-02-11 14:19:22Z carlos $"

SH = '/bin/bash'

class Resource (drm4g.managers.Resource):    


    def lrmsProperties(self):
        return ('FORK' ,'FORK')

    def queueProperties(self, queueName):
        queue              = drm4g.managers.Queue()
        queue.Name         = queueName
        queue.Nodes        = self.TotalCpu
        queue.FreeNodes    = self.FreeCpu
        queue.DispatchType = 'Immediate'
        return queue

class Job (drm4g.managers.Job):
    
    def jobSubmit(self, pathScript):
        out, err = self.Communicator.execCommand('%s %s' % (SH, pathScript))
        if err: 
            raise drm4g.managers.JobException(' '.join(err.split('\n')))
        job_id = out.rstrip('\n')
        return job_id          

    def jobStatus(self):
        out, err = self.Communicator.execCommand('ps --no-heading -p %s' %(self.JobId))
        if out:
            return 'ACTIVE'
        else:   
            return 'DONE'
    
    def jobCancel(self):
        jobs_to_kill = [self.JobId]
        while jobs_to_kill:
            for job in jobs_to_kill:
                 out, err = self.Communicator.execCommand('ps ho pid --ppid %s' % (job)) 
                 jobs_to_kill = [line.lstrip() for line in out.splitlines()] + jobs_to_kill
                 out, err = self.Communicator.execCommand('kill -9 %s' % (job))
                 if err:
                     raise drm4g.managers.JobException('Could not kill %s : %s' % (job, ' '.join(err.split('\n'))))
                 jobs_to_kill.remove(job)

    def jobTemplate(self, parameters):
        line  = '#!/bin/bash\n'
        line += ''.join(['export %s=%s\n' % (k, v) for k, v in parameters['environment'].items()])
        line += 'cd $directory\n' 
        line += 'nohup $executable 2> $stderr > $stdout &\n' 
        line += 'echo $$!\n'
        return Template(line).safe_substitute(parameters)


