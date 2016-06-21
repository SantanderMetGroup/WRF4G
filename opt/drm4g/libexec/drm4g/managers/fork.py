import os
import drm4g.managers 
from string import Template

__version__  = '2.4.1'
__author__   = 'Carlos Blanco'
__revision__ = "$Id: fork.py 2811 2015-09-22 11:33:32Z carlos $"

SH = '/bin/bash'

class Resource (drm4g.managers.Resource):
    pass

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
        line += ''.join(['export %s=%s\n' % (k, v) for k, v in list(parameters['environment'].items())])
        line += '\n' 
        line += 'nohup $executable 2> $stderr > $stdout &\n' 
        line += 'echo $$!\n'
        return Template(line).safe_substitute(parameters)


